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
        [czlab.loki.sys.util]
        [czlab.basal.process]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game Game Arena]
           [czlab.loki.sys Player Session]
           [czlab.loki.net EventError Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private _270_ (* 1.5 Math/PI))
(def ^:private _3pi_ (* 3 Math/PI))
(def ^:private _2pi_ (* 2 Math/PI))
(def ^:private _pi_ Math/PI)
(def ^:private _90_ (/ 2 _pi_))
(def ^:private _45_ (/ 4 _pi_))
(def ^:private _paddle-speed_ 65)
(def ^:private _ball-speed_ 100)
(def ^:private _ball-acc_ 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private halve [v] `(* ~v 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol GameAPI
  ""
  (registerPlayers [_ p1 p2])
  (playerXXX [_ _ _])
  (player [_ _])
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
  {:value idValue :color pcolor :session psession})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private rectXrect?
  "If 2 rects intersect" (fn [world _ _] (:opengl? world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod rectXrect? true [world ra rb]

  (not (or (< (:right ra) (:left rb))
           (< (:right rb) (:left ra))
           (< (:top ra) (:bottom rb))
           (< (:top rb) (:bottom ra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod rectXrect? false [world ra rb]

  (not (or (< (:right ra) (:left rb))
           (< (:right rb) (:left ra))
           (> (:top ra) (:bottom rb))
           (> (:top rb) (:bottom ra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private rect<>
  "Make a rect with all 4 corners" (fn [w _ _ _ _] (:opengl? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- bbox<>
  "" [world {:keys [x y
                    width height]}]
  (rect<> world x y width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod rect<> true [world x y w h]

  (let [h2 (halve h)
        w2 (halve w)]
    {:left (- x w2) :right (+ x w2)
     :top (+ y h2) :bottom (- y h2)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod rect<> false [world x y w h]

  (let [h2 (halve h)
        w2 (halve w)]
    {:left (- x w2) :right (+ x w2)
     :top (- y h2) :bottom (+ y h2)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hhalve "" [obj] [(halve (:height obj)) (halve (:width obj))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;out of bound?
(defn- oob? "" [op kw o1 o2] (op (kw o1) (kw o2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private syncPad
  "Move and clamp" (fn [w _ _] (:layout w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod syncPad :portrait [world pad dt]

  (let [dp (* dt (:speed pad) (:theta pad))
        pad (update-in pad [:x] + dp)
        [h2 w2] (hhalve pad)
        r (bbox<> world pad)]
    (or (some->> (cond
                   (oob? > :right r world)
                   (- (:right world) w2)
                   (oob? < :left r world)
                   (+ (:left world) w2))
                 (assoc pad :x))
        pad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod syncPad :landscape [world pad dt]

  (let [{:keys [top bottom left right opengl?]}
        world
        dp (* dt (:speed pad) (:theta pad))
        [h2 w2] (hhalve pad)
        pad (if opengl?
              (assoc pad :y + dp)
              (assoc pad :y - dp))
        r (bbox<> world pad)]
    (or (if opengl?
          (some->> (cond
                     (oob? < :bottom r world)
                     (+ bottom h2)
                     (oob? > :top r world)
                     (- top h2))
                   (assoc pad :y))
          (some->> (cond
                     (oob? > :bottom r world)
                     (- bottom h2)
                     (oob? < :top r world)
                     (+ top h2))
                   (assoc pad :y)))
        pad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private winner?? "" (fn [world _ _ _] (:layout world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod winner?? :portrait [world p1 p2 ball]

  (let [{:keys [opengl?]} world
        {:keys [x y]} ball
        r2 (bbox<> world p2)
        r1 (bbox<> world p1)]
    (if opengl?
      (cond
        (< y (:bottom r1)) 2
        (> y (:top r2)) 1
        :else 0)
      (cond
        (> y (:bottom r2)) 1
        (< y (:top r1)) 2
        :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod winner?? :landscape [world p1 p2 ball]

  (let [r2 (bbox<> world p2)
        r1 (bbox<> world p1)
        {:keys [x y]} ball]
    (cond
      (< x (:left r1)) 2
      (> x (:right r2)) 1
      :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;top, bottom = 2pi - t
;;side - going up = pi - t else 3pi - t
(defn- bounce! "" [ball axis]
  (let [{:keys [theta]} ball
        t (case axis
            :right
            (cond
              (and (> theta 0) (< theta _90_))
              (- _pi_ theta)
              (and (> theta _270_) (< theta _2pi_))
              (- _3pi_ theta))
            :left
            (cond
              (and (> theta _90_) (< theta _pi_))
              (- _pi_ theta)
              (and (> theta _pi_) (< theta _270_))
              (- _3pi_ theta))
            ;;(:bottom :top)
            (- _2pi_ theta))]
    (if (some? t)
      (-> (update-in ball [:speed] + _ball-acc_)
          (assoc :theta t))
      ball)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private hitWall?? "" (fn [world _] (:layout world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod hitWall?? :portrait [world ball]
  ;;check side walls
  (let [{:keys [left right opengl?]}
        world
        [_ w2] (hhalve ball)
        b (bbox<> world ball)
        [axis x']
        (cond
          (oob? < :left b world)
          [:left (+ left w2)]
          (oob? > :right b world)
          [:right (- right w2)])]
    (if (some? axis)
      (-> (assoc ball :x x')
          (bounce! axis))
      ball)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod hitWall?? :landscape [world ball]
  ;;check top bottom walls
  (let [{:keys [bottom top opengl?]}
        world
        [h2 _] (hhalve ball)
        b (bbox<> world ball)
        [axis y']
        (if opengl?
          (cond
            (oob? < :bottom b world)
            [:bottom (+ bottom  h2)]
            (oob? > :top b world)
            [:top (- top h2)])
          (cond
            (oob? > :bottom b world)
            [:bottom (- bottom  h2)]
            (oob? < :top b world)
            [:top (+ top h2)]))]
    (if (some? axis)
      (-> (assoc ball :y y')
          (bounce! axis))
      ball)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private hitPad?? "" (fn [world _ _ _] (:layout world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod hitPad?? :portrait [world p1 p2 ball]

  (let [{:keys [opengl?]} world
        [b2h _] (hhalve ball)
        br (bbox<> world ball)
        r2 (bbox<> world p2)
        r1 (bbox<> world p1)
        [axis y']
        (cond
          (rectXrect? world br r2)
          (if opengl?
            [:top (- (:bottom r2) b2h)]
            [:bottom (- (:top r2) b2h)])
          (rectXrect? world br r1)
          (if opengl?
            [:bottom (+ (:top r1) b2h)]
            [:top (+ (:bottom r1) b2h)]))]
    (if (some? axis)
      (-> (assoc ball :y y')
          (bounce! axis))
      (hitWall?? world ball))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod hitPad?? :landscape [world p1 p2 ball]

  (let [[_ b2w] (hhalve ball)
        br (bbox<> world ball)
        r2 (bbox<> world p2)
        r1 (bbox<> world p1)
        [axis x']
        (cond
          (rectXrect? world br r2)
          [:right (- (:left r2) b2w)]
          (rectXrect? world br r1)
          [:left (+ (:right r1) b2w)])]
    (if (some? axis)
      (-> (assoc ball :x x')
          (bounce! axis))
      (hitWall?? world ball))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private cfgBall "" (fn [w _] (:layout w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod cfgBall :portrait [world ball]
  (let [{:keys [x y opengl?]} world
        r (* (Math/random) _90_)]
    ;;calc random angle 45 < t < 135 or 225 < t < 315
    (merge ball
           {:x x :y y}
           (if (spos? (randSign))
             {:theta (+ r _45_)}
             {:theta (+ r _pi_ _45_)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod cfgBall :landscape [world ball]
  (let [{:keys [x y opengl?]} world
        ;;calc random angle 45 > t > 315 or 135 < t < 225
        r (* (Math/random) _90_)
        {:keys [theta] :as b}
        (merge ball
               {:x x :y y}
               (if (spos? (randSign))
                 {:theta (+ r _90_ _45_)}
                 {:theta (+ r _270_ _45_)}))]
    (if (> theta _2pi_)
      (assoc b :theta (- theta _2pi_)) b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private cfgPads
  "[p1 p2] p1 is always closest to origin" (fn [w _] (:layout w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod cfgPads :portrait [world paddle]
  (let [[hp wp] (hhalve paddle)
        {:keys [x y opengl?]} world]
    (mapv #(merge paddle %)
          (if opengl?
            [{:x x :y (+ (:bottom world) hp)} ;;p1
             {:x x :y (- (:top world) hp)}] ;;p2
            [{:x x :y (+ (:top world) hp)} ;;p1
             {:x x :y (- (:bottom world) hp)}])))) ;;p2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod cfgPads :landscape [world paddle]
  (let [[hp wp] (hhalve paddle)
        {:keys [x y opengl?]} world]
    (mapv #(merge paddle %)
          [{:y y :x (+ (:left world) wp)} ;;p1
           {:y y :x (- (:right world) wp)}]))) ;;p2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getFrames "" [sessions]
  (->> (preduce<vec>
         #(let [{:keys [framespersec]}
                (.settings ^Session %2)]
            (if (spos? framespersec)
              (conj! %1 framespersec) %1)) sessions)
       (apply min 60)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctorWorld ""
  ([w h] (ctorWorld 0 0 w h true))
  ([x y w h openGL?]
   (let [c (merge {:left x :right (- w x 1)}
                  (if openGL?
                    {:bottom y :top (- h y 1)}
                    {:top y :bottom (- h y 1)})
                  {:width w :height h
                   :opengl? openGL?
                   :layout (if (> h w) :portrait :landscape)})]
     (merge c
            {:x (+ (:left c) (halve w))}
            (if openGL?
              {:y (+ (:bottom c) (halve h))}
              {:y (+ (:top c) (halve h))})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private ctorObj "" (fn [w _ _ _] (:layout w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod ctorObj :portrait [box w h s]
  (doto
    {:speed s :theta 0 :x 0 :y 0 :width w :height h}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod ctorObj :landscape [box w h s]
  (doto
    {:speed s :theta 0 :x 0 :y 0 :width h :height w}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pong
  "" ^Game [^Arena room sessions]

  (let [framespersec (getFrames sessions)
        tick (/ 1000 framespersec)
        syncMillis 2000
        numpts 5
        world (ctorWorld 320 480)
        ball (ctorObj world 15 15 _ball-speed_)
        paddle (ctorObj world 50 12 _paddle-speed_)
        actors (object-array 3)
        score (volatile! {})
        state (volatile! {})]

    (reify Game

      (playerGist [me id]
        (some #(let [^Session s (:session %)]
                 (if (= id (.. s player id))
                   (dissoc % :session))) (drop 1 actors)))

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
        (->> {(.playerXXX me 2 :color) 0
              (.playerXXX me 1 :color) 0}
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

      (player [_ n]
        (aget #^"[Ljava.lang.Object;" actors n))
      (playerXXX [_ n kee] (kee (.player _ n)))

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
        (let [{:keys [^Session context body]}
              evt
              pnum (.number context)
              p2 (.player me 2)
              p1 (.player me 1)
              kw (:color (if (= pnum 1) p1 p2))
              st (:session (if (= pnum 1) p2 p1))
              pv (:pv (kw body))
              sign (numSign pv)]
          (vswap! state
                  update-in [kw] assoc :theta sign)
          (syncArena! room body st)))

      (postUpdateArena [this]
        (let [[a b] (vals @score)]
          ;;someone has won, get out
          (if (or (>= b numpts)
                  (>= a numpts))
            (trap! Exception "loop breaker")
            (try! (Thread/sleep tick)))))

      (runGameLoop [this cmd]
        (vswap! state
                assoc
                :lastTick (now<>)
                :lastSync 0
                :resetting-pt? false)
        (if (:new? cmd)
          (async! #(try!
                     (while true
                        (try! (.updateArena this))
                        (.postUpdateArena this)))
                  {:daemon true})))

      (pokeAndStartUI [me]
        (let [[p1 p2] (cfgPads world paddle)
              b (cfgBall world ball)
              c2 (.playerXXX me 2 :color)
              c1 (.playerXXX me 1 :color)]
          (vreset! state {c2 p2 c1 p1 :ball b})
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
                (.syncClients this)
                (vswap! state assoc :lastSync 0))))))

      ;;Update UI with states of local entities
      (syncClients [me]
        (let [c2 (.playerXXX me 2 :color)
              c1 (.playerXXX me 1 :color)
              src (select-keys @state [c2 c1 :ball])]
          (log/debug "sync new values %s" src)
          (syncArena! room src)))

      ;;A point has been won. Update the score, and maybe trigger game-over
      (updatePoint [me winner]
        (let [p (.player me winner)
              c (:color p)
              sx (inc (@score c))]
          (vswap! state
                  assoc
                  :resetting-pt? true)
          (vswap! score assoc c sx)
          (log/debug "updated score by 1, new score: %s" @score)
          (if (>= sx numpts)
            (.gameOver me winner)
            (.startRound me {}))))

      ;;Move local entities per game loop
      (syncTick [me dt]
        (let [c2 (.playerXXX me 2 :color)
              c1 (.playerXXX me 1 :color)
              pad2 (syncPad world (c2 @state) dt)
              pad1 (syncPad world (c1 @state) dt)
              _ (vswap! state assoc c2 pad2 c1 pad1)
              ball (moveObject! (:ball @state)
                                dt (:opengl? world))
              win (winner?? world pad1 pad2 ball)
              ball
              (if-not (spos? win)
                (hitPad?? world pad1 pad2 ball) ball)]
          (if (spos? win)
            (.updatePoint me win)
            (vswap! state assoc :ball ball)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

