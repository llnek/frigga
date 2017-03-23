;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.frigga.pong.core

  (:require [czlab.basal.logging :as log])

  (:use [czlab.loki.net.core]
        [czlab.basal.process]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game Game Arena]
           [czlab.loki.sys Player Session]
           [czlab.loki.net EventError Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;; 150px/sec
;;(def ^:private INITIAL_BALL_SPEED 150)
;;(def ^:private BALL_SPEEDUP 25) ;; pixels / sec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private velo "" [world] `(if (:port? ~world) :vx :vy))
(defmacro ^:private axis "" [world] `(if (:port? ~world) :x :y))
(defmacro ^:private bspeed "" [b] `(* (randSign)(:speed ~b)))
(defmacro ^:private halve [v] `(* ~v 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol GameAPI
  ""
  (registerPlayers [_ p1 p2])
  (getPlayer [_ _])
  (getPlayer2 [_])
  (getPlayer1 [_])
  (gameOver [_ winner])
  (enqueue [_ evt])
  (postUpdateArena [_])
  (runGameLoop [_ _])
  (pokeAndStartUI [_] )
  (updateArena [_] )
  (syncClients [_] )
  (updatePoint [_ winner] )
  (syncTick [_ dt] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wrapPlayer
  "" [idValue pcolor psession]
  {:value idValue :color pcolor :session psession })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rectXrect?
  "If 2 rects intersect" [ra rb]
  (not (or (< (:right ra) (:left rb))
           (< (:right rb) (:left ra))
           (< (:top ra) (:bottom rb))
           (< (:top rb) (:bottom ra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rect<>
  "Make a rect with all 4 corners"

  ([obj] (rect<> (:x obj) (:y obj) (:width obj) (:height obj)))

  ([x y w h]
   (let [h2 (halve h)
         w2 (halve w)]
     {:left (- x w2) :right (+ x w2) :top (+ y h2) :bottom (- y h2) })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hhalve "" [obj] [(halve (:height obj)) (halve (:width obj))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- oob? "" [op kw o1 o2] (op (kw o1) (kw o2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- clampPad
  "Ensure paddle does not go out of bound"
  [paddle world dt]

  ;;first move then clamp
  (let [paddle (assoc paddle
                      (axis world)
                      (+ (* dt ((velo world) paddle))
                         ((axis world) paddle)))
        [h2 w2] (hhalve paddle)
        rc (rect<> paddle)]
    (or (if (:port? world)
          (cond
            (oob? < :left rc world)
            (assoc paddle :x (+ (:left world) w2))
            (oob? > :right rc world)
            (assoc paddle :x (- (:right world) w2)))
          (cond
            (oob? < :bottom rc world)
            (assoc paddle :y (+ (:bottom world) h2))
            (oob? > :top rc world)
            (assoc paddle :y (- (:top world) h2))))
        paddle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The *enclosure* is the bounding box => the world.
(defn- clampBall
  "Check if the ball has just hit a wall"
  [ball bbox dt]

  ;;first move the ball then clamp it
  (let [{:keys [vx vy] :as b}
        (merge ball
                 {:y (+ (* dt (:vy ball)) (:y ball))
                  :x (+ (* dt (:vx ball)) (:x ball))})
        [h2 w2] (hhalve b)
        rc (rect<> b)
        bx (cond
             (oob? < :left rc bbox)
             (assoc b
                    :vx (- vx)
                    :x (+ (:left bbox) w2))
             (oob? > :right rc bbox)
             (assoc b
                    :vx (- vx)
                    :x (- (:right bbox) w2)))
        b (or bx b)
        by (cond
             (oob? < :bottom rc bbox)
             (assoc b
                    :vy (- vy)
                    :y (+ (:bottom bbox) h2))
             (oob? > :top rc bbox)
             (assoc b
                    :vy (- vy)
                    :y (- (:top bbox) h2)))
        b (or by b)
        hit? (or (some? bx)(some? by))]
    b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- winner? "" [p1 p2 ball bbox port?]
  (let [br (rect<> ball)
        r2 (rect<> p2)
        r1 (rect<> p1)]
    (if port?
      (cond
        (<= (:bottom ball) (:bottom bbox))
        (if (> (:bottom r2) (:top r1)) 2 1)
        (>= (:top ball) (:top bbox))
        (if (< (:top r2) (:bottom r1)) 2 1)
        :else 0)
      (cond
        (<= (:left ball) (:left bbox))
        (if (> (:left r2) (:right r1)) 2 1)
        (>= (:right ball) (:right bbox))
        (if (< (:right r2) (:left r1)) 2 1)
        :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- collide?
  "If the ball has collided with a paddle"
  [p1 p2 ball bbox]

  (let [[hh hw] (hhalve ball)
        {:keys [vx vy]}
        ball
        br (rect<> ball)
        r2 (rect<> p2)
        r1 (rect<> p1)
        b (if (rectXrect? br r2)
            (if (:port? bbox)
              (assoc ball
                     :vy (- vy)
                     :y (- (:bottom r2) hh))
              (assoc ball
                     :vx (- vx)
                     :x (- (:left r2) hw))))
        ball (or b ball)
        b (if (rectXrect? br r1)
            (if (:port? bbox)
              (assoc ball
                     :vy (- vy)
                     :y (+ (:top r1) hh))
              (assoc ball
                     :vx (- vx)
                     :x (+ (:right r1) hw))))
        ball (or b ball)]
    ball))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgBall "" [world ball]
  (let [[h2 w2] (hhalve world)]
    (merge ball
           {:vx (bspeed ball)
            :vy (bspeed ball) :y h2 :x w2})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgPads "" [world paddle ball]

  (let [[hp wp] (hhalve paddle)
        [hw ww] (hhalve world)]
    (if (:port? world)
      [{:x ww :y (- (:top world)
                    hp
                    (:height ball))}
       {:x ww :y (+ (:bottom world)
                    hp
                    (:height ball))}]
      [{:y hw :x (- (:right world)
                    wp
                    (:width ball))}
       {:y hw :x (+ (:left world)
                    wp
                    (:width ball))}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getFrames "" [sessions]
  (->> (preduce<vec>
         #(let [{:keys [framespersec]}
                (.settings ^Session %2)]
            (if (spos? framespersec)
              (conj! %1 framespersec) %1))
         (vals sessions))
       (apply min 60)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mvpad "" [pad world dt]
  (if (:port? world)
    (+ (* dt (:vx pad)) (:x pad))
    (+ (* dt (:vy pad)) (:y pad))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Pong
  "" ^Game [^Arena room sessions]

  (let [world {:port? true :top 479 :bottom 0 :right 319 :left 0}
        ball {:speed 150 :vx 0 :vy 0 :width 15 :height 15}
        paddle {:width 50 :height 12 :vx 0 :vy 0}
        framespersec (getFrames sessions)
        fps (/ 1000 framespersec)
        actors (object-array 3)
        syncMillis 3000
        numpts 9
        score (volatile! {})
        state (volatile! {})]

    (reify Game

      (init [me _]
        (log/debug "pong: init: FPS=%d" framespersec)
        (let [p1 (wrapPlayer (long \X) :X (first sessions))
              p2 (wrapPlayer (long \O) :O (last sessions))]
          (.registerPlayers me p1 p2)))

      (startRound [this cmd]
        (doto this .pokeAndStartUI (.runGameLoop cmd)))

      (endRound [_ ])

      (start [me arg]
        (log/debug "pong: start called()")
        (->> {(:color (.getPlayer2 me)) 0
              (:color (.getPlayer1 me)) 0}
             (vreset! score))
        (.startRound me {:new? true}))

      (onEvent [this evt]
        (log/debug "game engine got an update %s" evt)
        (if (isMove? evt)
          (let [{:keys [body context]} evt]
            (log/debug "received paddle-move: %s%s%s"
                       body " from session " context)
            (.enqueue this evt))))

      GameAPI

      (getPlayer [_ n]
        (aget #^"[Ljava.lang.Object;" actors n))
      (getPlayer2 [_] (.getPlayer _ 2))
      (getPlayer1 [_] (.getPlayer _ 1))

      (registerPlayers [_ p1 p2]
        (aset #^"[Ljava.lang.Object;" actors 2 p2)
        (aset #^"[Ljava.lang.Object;" actors 1 p1)
        (log/debug "Player2: %s" p2)
        (log/debug "Player1: %s" p1))

      (gameOver [this winner]
        (let [src {:winner (merge @score
                                  {:pnum winner})}]
          (log/debug "game over: winner is %s" src)
          (.stop this)
          (bcast! room Events/GAME_WON src)))

      (enqueue [me evt]
        (let [dir (if (:port? world) :vx :vy)
              {:keys [context body]}
              evt
              pnum (.number ^Session context)
              p2 (.getPlayer2 me)
              p1 (.getPlayer1 me)
              kw (:color (if (= pnum 1) p1 p2))
              st (:session (if (= pnum 1) p2 p1))
              pv (:pv (kw body))]
          (vswap! state
                  update-in [kw] assoc dir pv)
          (syncArena! room body st)))

      (postUpdateArena [this]
        (let [[a b] (vals @score)]
          (if (and (not (:resetting-pt? @state))
                   (or (>= b numpts)
                       (>= a numpts)))
            (do
              (.endRound this)
              (trap! Exception "loop breaker"))
            (try! (Thread/sleep fps)))))

      (runGameLoop [this cmd]
        (vswap! state
                assoc
                :lastTick (now<>)
                :lastSync 0
                :sync? true
                :resetting-pt? false)
        (if (:new? cmd)
          (async! #(while true
                     (try! (.updateArena this))
                     (.postUpdateArena this))
                  {:daemon true})))

      (pokeAndStartUI [me]
        (let [[p2 p1] (cfgPads world paddle ball)
              b (cfgBall world ball)
              c2 (:color (.getPlayer2 me))
              c1 (:color (.getPlayer1 me))]
          (vreset! state {c2 p2 c1 p1 :ball b})
          (vreset! score {c2 0 c1 0})
          (->> {:score @score :ball b c2 p2 c1 p1}
               (bcast! room Events/START_ROUND ))))

      (updateArena [this]
        (if-not (:resetting-pt? @state)
          (let [lastTick (:lastTick @state)
                now (now<>)
                lastSync (:lastSync @state)]
            ;; --- update the game with the difference
            ;;in ms since the
            ;; --- last tick
            (let [diff (- now lastTick)
                  lastSync2 (+ lastSync diff)]
              (.syncTick this (/ diff 1000))
              (vswap! state
                      assoc
                      :lastTick now
                      :lastSync lastSync2)
              ;; --- check if time to send a ball update
              (when (> lastSync syncMillis)
                (if (:sync? @state) (.syncClients this))
                (vswap! state assoc :lastSync 0))))))

      ;;Update UI with states of local entities
      (syncClients [me]
        (let [c2 (:color (.getPlayer2 me))
              c1 (:color (.getPlayer1 me))
              src (select-keys @state [c2 c1 :ball])]
          (log/debug "sync new values %s" src)
          (syncArena! room src)))

      ;;A point has been won. Update the score, and maybe trigger game-over
      (updatePoint [me winner]
        (let [p (.getPlayer me winner)
              c (:color p)
              sx (inc (@score c))]
          (vswap! state
                  assoc
                  :sync? false
                  :resetting-pt? true)
          (vswap! score assoc c sx)
          (log/debug "updated score by 1, new score: %s" @score)
          (if (>= sx numpts)
            (.gameOver me winner)
            (.startRound me {}))))

      ;;Move local entities per game loop
      (syncTick [me dt]
        (let [c2 (:color (.getPlayer2 me))
              c1 (:color (.getPlayer1 me))
              ball (clampBall (:ball @state) world dt)
              pad2 (clampPad (c2 @state) world dt)
              pad1 (clampPad (c1 @state) world dt)
              win (winner? pad1 pad2 ball world)]
          (if (> win 0)
            (.updatePoint me win)
            (vswap! state
                    assoc
                    :ball (collide? pad1 pad2 ball world)
                    c2 pad2
                    c1 pad1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

