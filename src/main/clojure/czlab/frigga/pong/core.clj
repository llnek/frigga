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
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game GameImpl GameRoom]
           [czlab.loki.game Player Session]
           [czlab.jasal Muble]
           [czlab.loki.net EventError Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;; 150px/sec
;;(def ^:private INITIAL_BALL_SPEED 150)
;;(def ^:private BALL_SPEEDUP 25) ;; pixels / sec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro halve [v] `(* ~v 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol TableAPI
  ""
  (registerPlayers [_ p1 p2])
  (updateArena [_])
  (innards [_] )
  (room [_])
  (getPlayer2 [_])
  (getPlayer1 [_])
  (enqueue [_ cmd]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyPlayer
  "" [idValue pcolor psession]
  (doto {:value idValue
         :color pcolor
         :session psession }))

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
(defn- rect
  "Make a rect with all 4 corners"

  ([^Muble obj]
   (rect (.getv obj :x)
         (.getv obj :y)
         (.getv obj :width)
         (.getv obj :height)))

  ([x y w h]
   (let [h2 (halve h)
         w2 (halve w) ]
     {:left (- x w2)
      :right (+ x w2)
      :top (+ y h2)
      :bottom (- y h2) })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- clamp
  "Ensure paddle does not go out of bound"
  [^Muble paddle bbox port?]

  (let [fx (fn [op kw o1 o2] (op (kw o1) (kw o2)))
        h2 (halve (.getv paddle :height))
        w2 (halve (.getv paddle :width))
        rc (rect paddle)]
    (if port?
      (do
        (when (fx < :left rc bbox)
          (.setv paddle :x (+ (:left bbox) w2)))
        (when (fx > :right rc bbox)
          (.setv paddle :x (- (:right bbox) w2))))
      (do
        (when (fx < :bottom rc bbox)
          (.setv paddle :y (+ (:bottom bbox) h2)))
        (when (fx > :top rc bbox)
          (.setv paddle :y (- (:top bbox) h2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The *enclosure* is the bounding box => the world.
(defn- traceEnclosure
  "Check if the ball has just hit a wall"
  [^Muble ball dt bbox port?]

  (with-local-vars
    [y (+ (* dt (.getv ball :vy))
          (.getv ball :y))
     x (+ (* dt (.getv ball :vx))
          (.getv ball :x))
     hit false]
    (let [sz (halve (.getv ball :height))
          sw (halve (.getv ball :width))]
      (if port?
        (do
          ;;check left and right walls
          (when (cond
                  (> (+ @x sw) (:right bbox))
                  (do (var-set x (- (:right bbox) sw))
                      true)
                  (< (- @x sw) (:left bbox))
                  (do (var-set x (+ (:left bbox) sw))
                      true)
                  :else false)
            (.setv ball :vx (- (.getv ball :vx)))
            (var-set hit true)))
        (do
          ;;check top and bottom walls
          (when (cond
                  (< (- @y sz) (:bottom bbox))
                  (do
                    (var-set y (+ (:bottom bbox) sz))
                    true)
                  (> (+ @y sz) (:top bbox))
                  (do (var-set y (- (:top bbox) sz))
                      true)
                  :else false)
            (.setv ball :vy (- (.getv ball :vy)))
            (var-set hit true))))
      (.setv ball :x @x)
      (.setv ball :y @y))
    @hit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- collide?
  "If the ball has collided with a paddle"
  [^Muble p1 ^Muble p2 ^Muble ball bbox port?]

  (with-local-vars [winner 0]
    (let [hh (halve (.getv ball :height))
          hw (halve (.getv ball :width))
          br (rect ball)]
      (let [r (rect p2)]
        (if (rectXrect? br r)
          (if port?
            (do
              (.setv ball :vy (- (.getv ball :vy)))
              (.setv ball :y (- (:bottom r) hh)))
            (do
              (.setv ball :vx (- (.getv ball :vx)))
              (.setv ball :x (- (:left r) hw))))
          (when (> (:bottom br)(:top r))
            (var-set winner 1))))
      (let [r (rect p1)]
        (if (rectXrect? br r)
          (if port?
            (do
              (.setv ball :vy (- (.getv ball :vy)))
              (.setv ball :y (+ (:top r) hh)))
            (do
              (.setv ball :vx (- (.getv ball :vx)))
              (.setv ball :x (+ (:right r) hw))))
          (when (< (:top br)(:bottom r))
            (var-set winner 2)))))
    @winner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeStartNewPoint
  "Point won, let the UI know, reset entities"
  [^czlab.frigga.pong.core.TableAPI arena winner]

  (let [^Muble impl (.innards arena)
        ^GameRoom room (.room arena)
        s2 (.getv impl :score2)
        s1 (.getv impl :score1)]
    (->> {:scores {:py2 s2 :py1 s1 }}
         (eventObj<> Events/PUBLIC
                     Events/SYNC_ARENA)
         (.send room))
    ;; toggle flag to skip game loop logic until new
    ;; point starts
    (.setv impl :resetting-point true)
    (.startRound arena nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gameOver
  "Game over.  Let the UI know"
  [^czlab.frigga.pong.core.TableAPI arena ^GameRoom room winner]

  (let [^Muble impl (.innards arena)
        ^Session ps2 (:session (.getv impl :p2))
        ^Session ps1 (:session (.getv impl :p1))
        s2 (.getv impl :score2)
        s1 (.getv impl :score1)
        ;;pwin (if (> s2 s1) ps2 ps1)
        src {:winner {:pnum winner ;;(.number pwin)
                      :scores {:py2 s2 :py1 s1}}}]
    ;; end game
    (log/debug "game over: winner of this game is %s" src)
    (. room send (eventObj<> Events/PUBLIC
                             Events/SYNC_ARENA src))
    (. room send (eventObj<> Events/PUBLIC
                             Events/STOP nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- postUpdateArena
  "After update, check if either one of the
   score has reached the target value, and if
   so, end the game else pause and loop again"
  [^czlab.frigga.pong.core.TableAPI arena ^GameRoom room options]

  (let [fps (/ 1000 (:framespersec options))
        ^Muble impl (.innards arena)
        {:keys [numpts]}
        options
        s2 (.getv impl :score2)
        s1 (.getv impl :score1)]
    (if (and (not (true? (.getv impl :resetting-point)))
             (or (>= s2 numpts)
                 (>= s1 numpts)))
      (do
        (log/debug "haha score %s vs %s%s"
                   s2 s1
                   " :-------------------> game over")
        (.endRound arena nil)
        ;; use this to get out of the while loop
        (trap! Exception "game over."))
      (try! (Thread/sleep fps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- runGameLoop
  "Spawn a game loop in a separate thread"
  [^czlab.frigga.pong.core.TableAPI arena ^GameRoom room options new?]

  (let [^Muble impl (.innards arena)]
    (.setv impl :lastTick (System/currentTimeMillis))
    (.setv impl :lastSync 0)
    (.setv impl :sync true)
    (.setv impl :resetting-point false)
    (when new?
      (.setv impl :numpts (:numpts options))
      (.setv impl :score2 0)
      (.setv impl :score1 0)
      (async! #(while true
                 (try! (updateArena arena room options))
                 (postUpdateArena arena room options))
              {:daemon true}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- initEntities
  "Initialize all local entities"
  [^czlab.frigga.pong.TableAPI arena
   {:keys [paddle ball py1 py2] :as options}]

  (let [^Muble impl (.innards arena)]
    (log/debug "resetting all entities back to default positions")
    (.setv impl :paddle2 (reifyPaddle (:x py2)
                                      (:y py2)
                                      (:width paddle)
                                      (:height paddle)))
    (.setv impl :paddle1 (reifyPaddle (:x py1)
                                      (:y py1)
                                      (:width paddle)
                                      (:height paddle)))
    (let [^Muble
          b (reifyBall (:x ball)
                       (:y ball)
                       (:width ball)
                       (:height ball))]
      (.setv b :vx (* (randomSign) (:speed ball)))
      (.setv b :vy (* (randomSign) (:speed ball)))
      (.setv impl :ball b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pokeAndStartUI ""
  [^czlab.frigga.pong.core.TableAPI arena
   ^GameRoom room
   {:keys [paddle ball py1 py2] :as options}]

  (let [^Muble impl (.innards arena)
        ^Session p2 (:session (.getv impl :p2))
        ^Session p1 (:session (.getv impl :p1))
        ^Muble
        ball (.getv impl :ball)
        src {:ball {:vx (.getv ball :vx)
                    :vy (.getv ball :vy)
                    :x (.getv ball :x)
                    :y (.getv ball :y)} }]
    (->> (reifySSEvent Events/POKE_MOVE
                       {:pnum (.number p2)} p2)
         (.send room))
    (->> (reifySSEvent Events/POKE_MOVE
                       {:pnum (.number p1)} p1)
         (.send room))
    (->> (eventObj<> Events/PUBLIC
                     Events/SYNC_ARENA src)
         (.send room))
    (log/debug "setting default ball values %s" src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Pong "" ^GameImpl [^GameRoom room players hints]

  (let [options (atom hints)
        impl (muble<>)]
    (reify

      TableAPI

      (registerPlayers [this p1Wrap p2Wrap]
        (doto impl
          (.setv :p2 p2Wrap)
          (.setv :p1 p1Wrap)))

      (getPlayer2 [_] (-> (.getv impl :p2) :player))
      (getPlayer1 [_] (-> (.getv impl :p1) :player))

      (innards [_] impl)

      (restart [_ _]
        (doto impl
          (.setv :resetting-point false)
          (.setv :score2 0)
          (.setv :score1 0)))

      (enqueue [_ evt]
        (let [^Session p2 (:session (.getv impl :p2))
              ^Session p1 (:session (.getv impl :p1))
              ^Session pss (:context evt)
              pnum (.number pss)
              kw (if (= pnum 1) :p1 :p2)
              pt (if (= pnum 1) p2 p1)
              src (:source evt)
              cmd (readJsonKW src)
              ;;pv (* (:dir (kw cmd)) (:speed paddle))
              pv (:pv (kw cmd))
              ^Muble
              other (if (= pnum 2)
                      (.getv impl :paddle2)
                      (.getv impl :paddle1))]
          (if (.getv impl :portrait)
            (.setv other :vx pv)
            (.setv other :vy pv))
          (->> (reifySSEvent Events/SYNC_ARENA cmd pt)
               (.send room))))

      "Update the state of the Arena per game loop"
      (updateArena [this]
        (if-not (true? (.getv impl :resetting-point))
          (let [{:keys[syncMillis numpts world]}
                @options
                lastTick (.getv impl :lastTick)
                lastSync (.getv impl :lastSync)
                now (System/currentTimeMillis)]
            ;; --- update the game with the difference
            ;;in ms since the
            ;; --- last tick
            (let [diff (- now lastTick)
                  lastSync2 (+ lastSync diff)]
              (.updateEntities this (/ diff 1000) world)
              (.setv impl :lastSync lastSync2)
              (.setv impl :lastTick now)
              ;; --- check if time to send a ball update
              (when (> lastSync syncMillis)
                (when (.getv impl :sync)
                  (.syncClients this))
                (.setv impl :lastSync 0))))))

      "Update UI with states of local entities"
      (syncClients [this]
        (let [port? (.getv impl :portrait)
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
          (.broadcast room
                      (publicEvent<> Events/SYNC_ARENA src))))

      ;;"A point has been won. Update the score, and maybe trigger game-over"
      (updatePoint [this winner]
        (let [nps (.getv impl :numpts)
              s2 (.getv impl :score2)
              s1 (.getv impl :score1)
              sx (if (= winner 2) (inc s2) (inc s1))]
          (log/debug "increment score by 1, %s%s,%s"
                     "someone lost a point" s1  s2)
          (.setv impl :sync false)
          (if (= winner 2)
            (.setv impl :score2 sx)
            (.setv impl :score1 sx))
          (.maybeStartNewPoint this winner)
          (when (>= sx nps) (.gameOver this winner))))

      "Move local entities per game loop"
      (updateEntities [_ dt bbox]
        (let [^Muble pad2 (.getv impl :paddle2)
              ^Muble pad1 (.getv impl :paddle1)
              ^Muble ball (.getv impl :ball)
              port? (.getv impl :portrait)]
          (if port?
            (do
              (.setv pad2 :x (+ (* dt (.getv pad2 :vx))
                                (.getv pad2 :x)))
              (.setv pad1 :x (+ (* dt (.getv pad1 :vx))
                                (.getv pad1 :x))))
            (do
              (.setv pad2 :y (+ (* dt (.getv pad2 :vy))
                                (.getv pad2 :y)))
              (.setv pad1 :y (+ (* dt (.getv pad1 :vy))
                                (.getv pad1 :y)))))
          (clamp pad2 bbox port?)
          (clamp pad1 bbox port?)
          (traceEnclosure ball dt bbox port?)
          (let [winner (collide? pad1 pad2 ball bbox port?)]
            (when (> winner 0)
              (.updatePoint this winner)))))



      GameImpl

      (startRound [this cmd]
        (initEntities this @options)
        (pokeAndStartUI this @options)
        (runGameLoop this room @options (true? (:new cmd))))

      ;; starts a new game by creating a new arena and players
      ;; follow by starting the first point.
      (start [this arg]
        (let [p1 (reifyPlayer (long \X) \X (first players))
              p2 (reifyPlayer (long \O) \O (last players))
              ;;{:keys [numpts world paddle ball py2 py1]}
              world (:world arg)]
          (reset! options arg)
          (.setv impl
                 :portrait
                 (> (- (:top world)(:bottom world))
                    (- (:right world)(:left world))))
          (.registerPlayers this p1 p2)
          (.startRound this {:new true})))

      (onEvent [this evt]
        (log/debug "game engine got an update %s" evt)
        (cond
          (and (= Events/PRIVATE (:type evt))
               (= Events/PLAY_MOVE (:code evt)))
          (let [{:keys [source context]} evt]
            (log/debug "received paddle-move %s%s%s"
                       source " from session " context)
            (. ^czlab.frigga.pong.core.TabelAPI this enqueue evt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

