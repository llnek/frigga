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
(def ^:private _360_ (* 2 Math/PI))
(def ^:private _180_ Math/PI)
(def ^:private _90_ (/ 2 Math/PI))
(def ^:private _paddle-speed_ 65)
(def ^:private _ball-speed_ 100)
(def ^:private _ball-acc_ 16)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rectXrect?
  "If 2 rects intersect" [ra rb bbox]
  (if (:opengl? bbox)
    (not (or (< (:right ra) (:left rb))
             (< (:right rb) (:left ra))
             (< (:top ra) (:bottom rb))
             (< (:top rb) (:bottom ra))))
    (not (or (< (:right ra) (:left rb))
             (< (:right rb) (:left ra))
             (> (:top ra) (:bottom rb))
             (> (:top rb) (:bottom ra))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rect<>
  "Make a rect with all 4 corners"

  ([obj bbox] (rect<> (:x obj) (:y obj)
                      (:width obj) (:height obj) bbox))

  ([x y w h bbox]
   (let [h2 (halve h)
         w2 (halve w)]
     (merge {:left (- x w2) :right (+ x w2)}
            (if (:opengl? bbox)
              {:top (+ y h2) :bottom (- y h2)}
              {:top (- y h2) :bottom (+ y h2)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hhalve "" [obj] [(halve (:height obj)) (halve (:width obj))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;out of bound?
(defn- oob? "" [op kw o1 o2] (op (kw o1) (kw o2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ppos "" [r2 r1 world]
  (if (:port? world)
    (if (> (:bottom r2)
           (:bottom r1)) [r1 r2] [r2 r1])
    (if (> (:right r2)
           (:right r1)) [r1 r2] [r2 r1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- syncPad
  "Move and clamp" [pad world dt]

  (let [dp (* dt (:speed pad) (:theta pad))
        [k op] (if (:port? world)
                 [:x +]
                 (if (:opengl? world) [:y +] [:y -]))
        pad (update-in pad k op dp)
        [h2 w2] (hhalve pad)
        r (rect<> pad world)]
    (or (if (:port? world)
          (some->> (cond
                     (oob? > :right r world)
                     (- (:right world) w2)
                     (oob? < :left r world)
                     (+ (:left world) w2))
                   (assoc pad :x))
          (if (:opengl? world)
            (some->> (cond
                       (oob? < :bottom r world)
                       (+ (:bottom world) h2)
                       (oob? > :top r world)
                       (- (:top world) h2))
                     (assoc pad :y))
            (some->> (cond
                       (oob? > :bottom r world)
                       (- (:bottom world) h2)
                       (oob? < :top r world)
                       (+ (:top world) h2))
                     (assoc pad :y))))
        pad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- winner? "" [p1 p2 ball bbox]
  (let [br (rect<> ball bbox)
        r2 (rect<> p2 bbox)
        r1 (rect<> p1 bbox)]
    (if (:port? bbox)
      (if (:opengl? bbox)
        (cond
          (<= (:bottom br) (:bottom bbox))
          (if (> (:bottom r2) (:top r1)) 2 1)
          (>= (:top br) (:top bbox))
          (if (< (:top r2) (:bottom r1)) 2 1)
          :else 0)
        (cond
          (>= (:bottom br) (:bottom bbox))
          (if (< (:bottom r2) (:top r1)) 2 1)
          (<= (:top br) (:top bbox))
          (if (> (:top r2) (:bottom r1)) 2 1)
          :else 0))
      (cond
        (<= (:left br) (:left bbox))
        (if (> (:left r2) (:right r1)) 2 1)
        (>= (:right br) (:right bbox))
        (if (< (:right r2) (:left r1)) 2 1)
        :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- bounce! "" [ball axis]
  (assoc ball
         :theta
         (mod (+ (- (* 2 axis) (:theta ball)) _360_) _360_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- syncBall
  "Move and clamp" [ball bbox dt]

  (let [r nil w2 nil h2 nil vx 0 vy 0 b nil
        bx (some->> (cond
                      (oob? < :left r bbox)
                      (+ (:left bbox) w2)
                      (oob? > :right r bbox)
                      (- (:right bbox) w2))
                    (assoc b :vx (- vx) :x))
        b (or bx b)
        by (some->> (cond
                      (oob? < :bottom r bbox)
                      (+ (:bottom bbox) h2)
                      (oob? > :top r bbox)
                      (- (:top bbox) h2))
                    (assoc b :vy (- vy) :y))]
    (or by b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- traceBall
  "" [p1 p2 ball bbox]

  (let [[b2h b2w] (hhalve ball)
        br (rect<> ball bbox)
        r2 (rect<> p2 bbox)
        r1 (rect<> p1 bbox)
        theta (:theta ball)
        s (:speed ball)
        [top bot]
        (if (:port?)
          (if (:opengl? bbox)
            (if (> (:y p2) (:y p1)) r2 r1)
            (if (< (:y p2) (:y p1)) r2 r1))
          (if (> (:x p2) (:x p1)) r2 r1))]
    (cond
      (rectXrect? br top bbox)
      (-> (if (:port? bbox)
            (-> (bounce! ball (if (> theta _90_) _180_ _360_))
                (assoc :y ((if (:opengl? bbox) - +) (:bottom top) b2h)))
            (-> (bounce! ball (if (> theta _270_) _270_ _90_))
                (assoc :x (- (:left top) b2w))))
          (assoc :speed (+ s _ball-acc_)))
      (rectXrect? br bot bbox)
      (-> (if (:port? bbox)
            (-> (bounce! ball (if (> theta _270_) _360_ _180_))
                (assoc :y ((if (:opengl? bbox) + -) (:top bot) b2h)))
            (-> (bounce! ball (if (> theta _180_) _270_ _90_))
                (assoc :x (+ (:right bot) b2w))))
          (assoc :speed (+ s _ball-acc_))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgBall "" [ball world]
  (let [[h2 w2] (hhalve world)]
    (merge ball
           {:vx (bspeed ball)
            :vy (bspeed ball) :y h2 :x w2})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgPads "" [paddle ball world]

  (let [[hp wp] (hhalve paddle)
        bh (:height ball)
        bw (:width ball)
        [hw ww] (hhalve world)]
    (if (:port? world)
      (mapv #(merge paddle %)
            [{:x ww :y (- (:top world) hp bh)}
             {:x ww :y (+ (:bottom world) hp bh)}])
      (mapv #(merge paddle %)
            [{:y hw :x (- (:right world) wp bw)}
             {:y hw :x (+ (:left world) wp bw)}]))))

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
   (merge {:left x :right (- w x 1)}
          (if openGL?
            {:bottom y :top (- h y 1)}
            {:top y :bottom (- h y 1)})
          {:width w :height h
           :opengl? openGL? :port? (> h w)} )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctorObj ""
  ([box w h] (ctorObj box w h 0))
  ([box w h s]
   (merge
     {:speed s :theta 0 :x 0 :y 0}
     (if (:port? box)
       {:width w :height h} {:width h :height w}))))

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
              pv (:pv (kw body))]
          (vswap! state
                  update-in [kw] assoc (velo world) pv)
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
        (let [[p2 p1] (cfgPads paddle ball world)
              b (cfgBall ball world)
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
              pad2 (syncPad (c2 @state) world dt)
              pad1 (syncPad (c1 @state) world dt)
              _ (vswap! state assoc c2 pad2 c1 pad1)
              ball (moveObject! (:ball @state)
                                dt (:opengl? world))
              win (winner? pad1 pad2 ball world)
              ball
              (if-not (spos? win)
                (traceBall pad1 pad2 ball world) ball)]
          (if (spos? win)
            (.updatePoint me win)
            (vswap! state assoc :ball ball)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

