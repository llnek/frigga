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
           [czlab.jasal Muble]
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
(defprotocol TableAPI
  ""
  (registerPlayers [_ p1Wrap p2Wrap])
  (maybeStartNewPoint [_ winner])
  (gameOver [_ winner])
  (enqueue [_ evt])
  (postUpdateArena [_])
  (runGameLoop [_ _])
  (pokeAndStartUI [_] )
  (reposAll [_] )
  (updateArena [_] )
  (syncClients [_] )
  (updatePoint [_ winner] )
  (syncArena [_ dt] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyPlayer
  "" [idValue pcolor psession]
  {:value idValue :color pcolor :session psession })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyObject "" [x y w h]
  (muble<> {:x x :y y
            :vx 0 :vy 0
            :height h :width w}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyPaddle "" [x y w h] (reifyObject x y w h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyBall "" [x y w h] (reifyObject x y w h))

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

  ([obj] (rect<> (:x obj)
                 (:y obj)
                 (:width obj)
                 (:height obj)))

  ([x y w h]
   (let [h2 (halve h)
         w2 (halve w)]
     {:left (- x w2)
      :right (+ x w2)
      :top (+ y h2)
      :bottom (- y h2) })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- within? "" [op kw o1 o2] (op (kw o1) (kw o2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- clamp
  "Ensure paddle does not go out of bound"
  [paddle bbox port?]

  (let [h2 (halve (:height paddle))
        w2 (halve (:width paddle))
        rc (rect<> paddle)]
    (or (if port?
          (cond
            (within? < :left rc bbox)
            (assoc paddle :x (+ (:left bbox) w2))
            (within? > :right rc bbox)
            (assoc paddle :x (- (:right bbox) w2)))
          (cond
            (within? < :bottom rc bbox)
            (assoc paddle :y (+ (:bottom bbox) h2))
            (within? > :top rc bbox)
            (assoc paddle :y (- (:top bbox) h2))))
        paddle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The *enclosure* is the bounding box => the world.
(defn- traceEnclosure
  "Check if the ball has just hit a wall"
  [ball dt bbox]

  (let [b (merge ball
                 {:y (+ (* dt (:vy ball)) (:y ball))
                  :x (+ (* dt (:vx ball)) (:x ball))})
        h2 (halve (:height ball))
        w2 (halve (:width ball))
        rc (rect<> b)
        bx (cond
             (within? < :left rc bbox)
             (assoc b
                    :vx (- (:vx ball))
                    :x (+ (:left bbox) w2))
             (within? > :right rc bbox)
             (assoc b
                    :vx (- (:vx ball))
                    :x (- (:right bbox) w2)))
        b (or bx b)
        by (cond
             (within? < :bottom rc bbox)
             (assoc b
                    :vy (- (:vy ball))
                    :y (+ (:bottom bbox) h2))
             (within? > :top rc bbox)
             (assoc b
                    :vy (- (:vy ball))
                    :y (- (:top bbox) h2)))
        b (or by b)
        hit? (or (some? bx)(some? by))]
    b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- winner? "" [p1 p2 ball bbox]
  (let [br (rect<> ball)
        r2 (rect<> p2)
        r nil
        r1 (rect<> p1)]
    (cond
      (> (:bottom br)(:top r)) 1
      (< (:top br)(:bottom r)) 2
      :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- collide?
  "If the ball has collided with a paddle"
  [p1 p2 ball bbox port?]

  (let [hh (halve (:height ball))
        hw (halve (:width ball))
        br (rect<> ball)
        r2 (rect<> p2)
        r1 (rect<> p1)
        b2 (when (rectXrect? br r2)
             (if port?
               (assoc ball
                      :vy (- (:vy ball))
                      :y (- (:bottom r2) hh))
               (assoc ball
                      :vx (- (:vx ball))
                      :x (- (:left r2) hw))))
        ball (or b2 ball)
        b1 (when (rectXrect? br r1)
             (if port?
               (assoc ball
                      :vy (- (:vy ball))
                      :y (+ (:top r1) hh))
               (assoc ball
                      :vx (- (.getv ball :vx))
                      :x (+ (:right r1) hw))))
        ball (or b1 ball)]
    ball))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Pong
  "" ^Game [^Arena room sessions]

  (let [world {:top 479 :bottom 0 :right 319 :left 0}
        paddle {:width 50 :height 12}
        ball {:speed 150 :width 15 :height 15}
        actors (object-array 3)
        syncMillis 1000
        impl (muble<>)
        portrait? true
        numpts 9
        scores (atom {})
        state (atom {})]

    (reify Game

      (init [me _]
        (log/debug "pong: init called()")
        (let [p1 (reifyPlayer (long \X) "X" (first sessions))
              p2 (reifyPlayer (long \O) "O" (last sessions))]
          (.registerPlayers me p1 p2)))

      (startRound [this cmd]
        (.pokeAndStartUI this)
        (.runGameLoop this cmd))

      (endRound [_ ])

      ;; starts a new game by creating a new arena and players
      ;; follow by starting the first point.
      (start [this arg]
        (log/debug "pong: start called()")
        (.startRound this {:new? true}))

      (onEvent [this evt]
        (log/debug "game engine got an update %s" evt)
        (if (isMove? evt)
          (let [{:keys [body context]} evt]
            (log/debug "received paddle-move: %s%s%s"
                       body " from session " context)
            (.enqueue this evt))))

      TableAPI

      (registerPlayers [_ p1 p2]
        (aset #^"[Ljava.lang.Object;" actors 2 p2)
        (aset #^"[Ljava.lang.Object;" actors 1 p1)
        (log/debug "Player2: %s" p2)
        (log/debug "Player1: %s" p1))

      (gameOver [_ winner]
        (let [s2 (.getv impl :score2)
              s1 (.getv impl :score1)
              src {:winner {:pnum winner
                            :scores {:ps2 s2 :ps1 s1}}}]
          (log/debug "game over: winner is %s" src)
          (syncArena! room src)
          (stop! room {})))

      (maybeStartNewPoint [this winner]
        (let [s2 (.getv impl :score2)
              s1 (.getv impl :score1)]
         (syncArena! room
                     {:scores {:ps2 s2 :ps1 s1 }})
         ;; skip game loop logic until new point starts
         (.setv impl :resetting-pt? true)
         (.startRound this {})))

      (enqueue [_ evt]
        (let [^Session p2 (:session (.getv impl :p2))
              ^Session p1 (:session (.getv impl :p1))
              ^Session pss (:context evt)
              pnum (.number pss)
              kw (if (= pnum 1) :p1 :p2)
              pt (if (= pnum 1) p2 p1)
              src (:body evt)
              cmd (readJsonStrKW src)
              ;;pv (* (:dir (kw cmd)) (:speed paddle))
              pv (:pv (kw cmd))
              ^Muble
              other (if (= pnum 2)
                      (.getv impl :paddle2)
                      (.getv impl :paddle1))]
          (if (.getv impl :portrait?)
            (.setv other :vx pv)
            (.setv other :vy pv))
          (syncArena! room cmd pt)))

      ;;"After update, check if either one of the score has reached the target value, and if so, end the game else pause and loop again"
      (postUpdateArena [this]
        (let [{:keys [framespersec numpts]}
              @impl
              fps (/ 1000 framespersec)
              s2 (.getv impl :score2)
              s1 (.getv impl :score1)]
          (if (and (not (true? (.getv impl :resetting-pt?)))
                   (or (>= s2 numpts)
                       (>= s1 numpts)))
            (do
              (log/debug "haha score %s vs %s%s"
                         s2 s1
                         " :-------------------> game over")
              (.endRound this)
              ;; use this to get out of the while loop
              (trap! Exception "game over."))
            (try! (Thread/sleep fps)))))

      ;;"Spawn a game loop in a separate thread"
      (runGameLoop [this cmd]
        (let [p2 (:color (.getPlayer2 this))
              p1 (:color (.getPlayer1 this))]
          (vswap! state
                  assoc
                  :lastTick (now<>)
                  :lastSync 0
                  :sync? true
                  :resetting-pt? false)
          (when (:new? cmd)
            (async! #(while true
                       (try! (.updateArena this))
                       (.postUpdateArena this))
                    {:daemon true}))))

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
            (vreset! scores
                     (-> (assoc {} (keyword p1) 0)
                         (assoc (keyword p2) 0)))
          (->> (-> {:ball b :scores @scores}
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
        (let [port? (.getv impl :portrait?)
              ^Muble pad2 (.getv impl :paddle2)
              ^Muble pad1 (.getv impl :paddle1)
              ^Muble ball (.getv impl :ball)
              src {:p2 {:y (.getv pad2 :y)
                        :x (.getv pad2 :x)
                        :pv (if port?
                              (.getv pad2 :vx)
                              (.getv pad2 :vy))}
                   :p1 {:y (.getv pad1 :y)
                        :x (.getv pad1 :x)
                        :pv (if port?
                              (.getv pad1 :vx)
                              (.getv pad1 :vy))}
                   :ball {:y (.getv ball :y)
                          :x (.getv ball :x)
                          :vy (.getv ball :vy)
                          :vx (.getv ball :vx)}}]
          (log/debug "sync new BALL values %s" (:ball src))
          (syncArena! room src)))

      ;;"A point has been won. Update the score, and maybe trigger game-over"
      (updatePoint [this winner]
        (let [s2 (.getv impl :score2)
              s1 (.getv impl :score1)
              nps (:numpts @impl)
              sx (if (= winner 2) (inc s2) (inc s1))]
          (log/debug "increment score by 1, %s%s,%s"
                     "someone lost a point" s1  s2)
          (.setv impl :sync? false)
          (if (= winner 2)
            (.setv impl :score2 sx)
            (.setv impl :score1 sx))
          (.maybeStartNewPoint this winner)
          (when (>= sx nps) (.gameOver this winner))))

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
              pad2 (clamp pad2 world portrait?)
              pad1 (clamp pad1 world portrait?)
              ball (traceEnclosure ball dt world)
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

