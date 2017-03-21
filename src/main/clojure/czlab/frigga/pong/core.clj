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

  (:import [czlab.loki.game GameImpl Arena]
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
  (restart [_ _])
  (enqueue [_ evt])
  (postUpdateArena [_])
  (runGameLoop [_ _])
  (pokeAndStartUI [_] )
  (initEntities [_] )
  (updateArena [_] )
  (syncClients [_] )
  (updatePoint [_ winner] )
  (updateEntities [_ dt bbox] ))

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
     hit? false]
    (let [sz (halve (.getv ball :height))
          sw (halve (.getv ball :width))]
      (if port?
        (do
          ;;check left and right walls
          (when (cond
                  (> (+ @x sw) (:right bbox))
                  (do->true (var-set x (- (:right bbox) sw)))
                  (< (- @x sw) (:left bbox))
                  (do->true (var-set x (+ (:left bbox) sw)))
                  :else false)
            (.setv ball :vx (- (.getv ball :vx)))
            (var-set hit? true)))
        (do
          ;;check top and bottom walls
          (when (cond
                  (< (- @y sz) (:bottom bbox))
                  (do->true
                    (var-set y (+ (:bottom bbox) sz)))
                  (> (+ @y sz) (:top bbox))
                  (do->true (var-set y (- (:top bbox) sz)))
                  :else false)
            (.setv ball :vy (- (.getv ball :vy)))
            (var-set hit? true))))
      (.setv ball :x @x)
      (.setv ball :y @y))
    @hit?))

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
(defn Pong
  "" ^GameImpl [^Arena room sessions]

  (let [options (atom {})
        impl (muble<>)]
    (reify

      TableAPI

      (registerPlayers [_ p1Wrap p2Wrap]
        (doto impl
          (.setv :p2 p2Wrap)
          (.setv :p1 p1Wrap)))

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

      (restart [_ _]
        (doto impl
          (.setv :resetting-pt? false)
          (.setv :score2 0)
          (.setv :score1 0)))

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
              @options
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
        (.setv impl :lastTick (System/currentTimeMillis))
        (.setv impl :lastSync 0)
        (.setv impl :sync? true)
        (.setv impl :resetting-pt? false)
        (when (:new cmd)
          (.setv impl :numpts (:numpts @options))
          (.setv impl :score2 0)
          (.setv impl :score1 0)
          (async! #(while true
                     (try! (.updateArena this))
                     (.postUpdateArena this))
                  {:daemon true})))

      (pokeAndStartUI [_]
        (let [^Session p2 (:session (.getv impl :p2))
              ^Session p1 (:session (.getv impl :p1))
              ^Muble ball (.getv impl :ball)
              src {:ball {:vx (.getv ball :vx)
                          :vy (.getv ball :vy)
                          :x (.getv ball :x)
                          :y (.getv ball :y)} }]
          (pokeMove! room {:pnum (.number p2)} p2)
          (pokeMove! room {:pnum (.number p1)} p1)
          (syncArena! room src)
          (log/debug "setting default ball values %s" src)))

      ;;"Initialize all local entities"
      (initEntities [_]
        (let [{:keys [paddle ball py2 py1]}
              @options]
          (log/debug "resetting entities to default")
          (.setv impl :paddle2
                 (reifyPaddle (:x py2)
                              (:y py2)
                              (:width paddle) (:height paddle)))
          (.setv impl :paddle1
                 (reifyPaddle (:x py1)
                              (:y py1)
                              (:width paddle) (:height paddle)))
          (let [^Muble b (reifyBall (:x ball)
                                    (:y ball)
                                    (:width ball) (:height ball))]
            (.setv b :vx (* (randSign) (:speed ball)))
            (.setv b :vy (* (randSign) (:speed ball)))
            (.setv impl :ball b))))

      ;;"Update the state of the Arena per game loop"
      (updateArena [this]
        (if-not (true? (.getv impl :resetting-pt?))
          (let [{:keys [syncMillis numpts world]}
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
                (when (.getv impl :sync?)
                  (.syncClients this))
                (.setv impl :lastSync 0))))))

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
              nps (:numpts @options)
              sx (if (= winner 2) (inc s2) (inc s1))]
          (log/debug "increment score by 1, %s%s,%s"
                     "someone lost a point" s1  s2)
          (.setv impl :sync? false)
          (if (= winner 2)
            (.setv impl :score2 sx)
            (.setv impl :score1 sx))
          (.maybeStartNewPoint this winner)
          (when (>= sx nps) (.gameOver this winner))))

      ;;"Move local entities per game loop"
      (updateEntities [this dt bbox]
        (let [^Muble pad2 (.getv impl :paddle2)
              ^Muble pad1 (.getv impl :paddle1)
              ^Muble ball (.getv impl :ball)
              port? (.getv impl :portrait?)]
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
        (.initEntities this)
        (.pokeAndStartUI this)
        (.runGameLoop this cmd))

      (endRound [_ ])

      ;; starts a new game by creating a new arena and players
      ;; follow by starting the first point.
      (start [this arg]
        (let [p1 (reifyPlayer (long \X) \X (first sessions))
              p2 (reifyPlayer (long \O) \O (last sessions))
              ;;{:keys [numpts world paddle ball py2 py1]}
              world (:world arg)]
          (reset! options arg)
          (.setv impl
                 :portrait?
                 (> (- (:top world)(:bottom world))
                    (- (:right world)(:left world))))
          (.registerPlayers this p1 p2)
          (.startRound this {:new true})))

      (onEvent [this evt]
        (log/debug "game engine got an update %s" evt)
        (if (isMove? evt)
          (let [{:keys [body context]} evt]
            (log/debug "received paddle-move %s%s%s"
                       body " from session " context)
            (.enqueue this evt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

