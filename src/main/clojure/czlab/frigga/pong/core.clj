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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
  (syncArena [_ dt] ))

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
(defn- hhalve "" [obj] [(halve (:height obj)) (halve (:width obj))]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- oob? "" [op kw o1 o2] (op (kw o1) (kw o2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- clampPad
  "Ensure paddle does not go out of bound"
  [paddle bbox port?]

  (let [[h2 w2] (hhalve paddle)
        rc (rect<> paddle)]

    (or (if port?
          (cond
            (oob? < :left rc bbox)
            (assoc paddle :x (+ (:left bbox) w2))
            (oob? > :right rc bbox)
            (assoc paddle :x (- (:right bbox) w2)))
          (cond
            (oob? < :bottom rc bbox)
            (assoc paddle :y (+ (:bottom bbox) h2))
            (oob? > :top rc bbox)
            (assoc paddle :y (- (:top bbox) h2))))

        paddle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The *enclosure* is the bounding box => the world.
(defn- clampBall
  "Check if the ball has just hit a wall"
  [ball dt bbox]

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
  [p1 p2 ball bbox port?]

  (let [[hh hw] (hhalve ball)
        {:keys [vx vy]}
        ball
        br (rect<> ball)
        r2 (rect<> p2)
        r1 (rect<> p1)
        b (if (rectXrect? br r2)
            (if port?
              (assoc ball
                     :vy (- vy)
                     :y (- (:bottom r2) hh))
              (assoc ball
                     :vx (- vx)
                     :x (- (:left r2) hw))))
        ball (or b ball)
        b (if (rectXrect? br r1)
            (if port?
              (assoc ball
                     :vy (- vy)
                     :y (+ (:top r1) hh))
              (assoc ball
                     :vx (- vx)
                     :x (+ (:right r1) hw))))
        ball (or b ball)]
    ball))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Pong
  "" ^Game [^Arena room sessions]

  (let [world {:top 479 :bottom 0 :right 319 :left 0}
        paddle {:width 50 :height 12}
        ball {:speed 150 :width 15 :height 15}
        actors (object-array 3)
        syncMillis 3000
        framespersec 60
        portrait? true
        numpts 9
        score (volatile! {})
        state (volatile! {})]

    (reify Game

      (init [me _]
        (log/debug "pong: init called()")
        (let [p1 (wrapPlayer (long \X) "X" (first sessions))
              p2 (wrapPlayer (long \O) "O" (last sessions))]
          (.registerPlayers me p1 p2)))

      (startRound [this cmd]
        (.pokeAndStartUI this)
        (.runGameLoop this cmd))

      (endRound [_ ])

      (getPlayer [_ n]
        (aget #^"[Ljava.lang.Object;" actors n))
      (getPlayer2 [_] (.getPlayer _ 2))
      (getPlayer1 [_] (.getPlayer _ 1))

      (start [me arg]
        (let [c2 (:color (.getPlayer2 me))
              c1 (:color (.getPlayer1 me))]
          (log/debug "pong: start called()")
          (vreset! score {c2 0 c1 0})
          (.startRound me {:new? true})))

      (onEvent [this evt]
        (log/debug "game engine got an update %s" evt)
        (if (isMove? evt)
          (let [{:keys [body context]} evt]
            (log/debug "received paddle-move: %s%s%s"
                       body " from session " context)
            (.enqueue this evt))))

      GameAPI

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
        (let [^Session p2 (:session (.getPlayer2 me))
              ^Session p1 (:session (.getPlayer1 me))
              {:keys [body context]}
              evt
              pnum (.number ^Session context)
              kw (if (= pnum 1) :p1 :p2)
              pt (if (= pnum 1) p2 p1)
              ;;pv (* (:dir (kw cmd)) (:speed paddle))
              pv (:pv (kw body))
              dir (if portrait? :vx :vy)
              which (if (= pnum 2) :paddle2 :paddle1)]
          (vswap! state
                  update-in [which] assoc dir pv)
          (syncArena! room body pt)))

      ;;"After update, check if either one of the score has reached the target value, and if so, end the game else pause and loop again"
      (postUpdateArena [this]
        (let [fps (/ 1000 framespersec)
              [a b] (vals @score)]
          (if (and (not (:resetting-pt? @state))
                   (or (>= b numpts)
                       (>= a numpts)))
            (do
              (.endRound this)
              ;; use this to get out of the while loop
              (trap! Exception "game over."))
            (try! (Thread/sleep fps)))))

      ;;"Spawn a game loop in a separate thread"
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
        (let [p2 (:color (.getPlayer2 me))
              p1 (:color (.getPlayer1 me))
              p (merge paddle
                       {:x (* 0.5 (:width world))
                        :y 0})
              hp (* 0.5 (:height paddle))
              p2 (assoc p :y (- (:top world)
                                hp
                                (:height ball)))
              p1 (assoc p :y (+ (:bottom world)
                                hp
                                (:height ball)))
              b (merge ball
                       {:y (* 0.5 (:height world))
                        :x (* 0.5 (:width world))
                        :vx (* (randSign) (:speed ball))
                        :vy (* (randSign) (:speed ball))})]
            (vswap! state assoc :numpts numpts)
            (vreset! score
                     (-> (assoc {} (keyword p1) 0)
                         (assoc (keyword p2) 0)))
          (->> (-> {:ball b :score @score}
                   (assoc (keyword p2) p2)
                   (assoc (keyword p1) p1))
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
              (.syncArena this (/ diff 1000))
              (vswap! state
                      assoc
                      :lastTick now
                      :lastSync lastSync2)
              ;; --- check if time to send a ball update
              (when (> lastSync syncMillis)
                (if (:sync? @state) (.syncClients this))
                (vswap! state assoc :lastSync 0))))))

      ;;"Update UI with states of local entities"
      (syncClients [this]
        (let [pad2 (:paddle2 @state)
              pad1 (:paddle1 @state)
              ball (:ball @state)
              src {:p2 {:y (.getv pad2 :y)
                        :x (.getv pad2 :x)
                        :pv (if portrait?
                              (.getv pad2 :vx)
                              (.getv pad2 :vy))}
                   :p1 {:y (.getv pad1 :y)
                        :x (.getv pad1 :x)
                        :pv (if portrait?
                              (.getv pad1 :vx)
                              (.getv pad1 :vy))}
                   :ball {:y (.getv ball :y)
                          :x (.getv ball :x)
                          :vy (.getv ball :vy)
                          :vx (.getv ball :vx)}}]
          (log/debug "sync new BALL values %s" (:ball src))
          (syncArena! room src)))

      ;;A point has been won. Update the score, and maybe trigger game-over
      (updatePoint [me winner]
        (let [p (.getPlayer me winner)
              c (:color p)
              s (get @score c)
              sx (inc s)]
          (vswap! state assoc :sync? false)
          (vswap! score assoc c sx)
          (log/debug "updated score by 1, new score: %s" @score)
          (if (>= sx numpts)
            (.gameOver me winner)
            (do
              ;; skip game loop logic until new point starts
              (vswap! state assoc :resetting-pt? true)
              (.startRound me {})))))

      ;;Move local entities per game loop
      (syncArena [this dt]
        (let [pad2 (:paddle2 @state)
              pad1 (:paddle1 @state)
              ball (:ball @state)
              [pad2 pad1]
              (if portrait?
                [(assoc pad2 :x (+ (* dt (:vx pad2))
                                   (:x pad2)))
                 (assoc pad1 :x (+ (* dt (:vx pad1))
                                   (:x pad1)))]
                [(assoc pad2 :y (+ (* dt (:vy pad2))
                                   (:y pad2)))
                 (assoc pad1 :y (+ (* dt (:vy pad1))
                                   (:y pad1)))])
              pad2 (clampPad pad2 world portrait?)
              pad1 (clampPad pad1 world portrait?)
              ball (clampBall ball dt world)
              win (winner? pad1 pad2 ball world)]
          (if (> win 0)
            (.updatePoint this win)
            (let
              [ball (collide? pad1
                              pad2
                              ball
                              world portrait?)]
              (vswap! state
                      assoc
                      :paddle2 pad2
                      :paddle1 pad1
                      :ball ball))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

