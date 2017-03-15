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
      :author "kenl"}

  czlab.frigga.tttoe.core

  (:require [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game GameImpl GameRoom]
           [czlab.loki.core Player Session]
           [czlab.loki.net EventError Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _cvz_ 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mapGoalSpace "" [bsize]
  (with-local-vars
    [dx (long-array bsize 0)
     dy (long-array bsize 0)
     rowsp (transient [])
     colsp (transient [])]
    (doseq [r (range 0 bsize)
           :let [h (long-array bsize 0)
                 v (long-array bsize 0)]]
      (doseq [c (range 0 bsize)]
        (aset h c (long (+ c (* r bsize))))
        (aset v c (long (+ r (* c bsize)))))
      (var-set rowsp (conj! @rowsp h))
      (var-set colsp (conj! @colsp v))
      (aset ^longs @dx
            r
            (long (+ (* r bsize) r)))
      (aset ^longs @dy
            r
            (long (+ r (* bsize
                          (dec (- bsize r)))))))
    (into [] (concat [@dx] [@dy]
                     (persistent! @rowsp)
                     (persistent! @colsp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(comment
  (doseq [v (mapGoalSpace 3)]
  (println (seq v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- reifyPlayer
  "" [idValue pcolor psession]

  {:value idValue
   :color pcolor
   :session psession })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol BoardAPI
  ""
  (broadcastStatus [_ ecode cmd status] )
  (registerPlayers [_ p1 p2])
  (getCur [_])
  (getOther [_ a])
  (isActive [_])
  (getPlayer2 [_])
  (getPlayer1 [_])
  (enqueue [_ cmd])
  (checkWin [_ cmd])
  (drawGame [_ cmd])
  (endGame [_ cmd combo])
  (toggleActor [_ cmd])
  (onStopReset [_])
  (repoke [_])
  (dequeue [_ cmd])
  (finz [_])
  (isStalemate [_])
  (isWinner [_ a]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn TicTacToe
  "" ^GameImpl [room players options]

  (let [bsize (or (:size options) 3)
        grid (long-array (* bsize bsize) _cvz_)
        goalspace (mapGoalSpace bsize)
        actors (object-array 3)
        numcells (alength grid)
        impl (muble<> {:gameon false}) ]
    (reify

      GameImpl

      (start [me _]
        (let [p1 (reifyPlayer (long \X) \X (first players))
              p2 (reifyPlayer (long \O) \O (last players))]
          (.registerPlayers me p1 p2)
          (.dequeue me nil)))

      (onEvent [me evt]
        (log/debug "game engine got an update %s" evt)
        (cond
          (and (= Events/PRIVATE (:type evt))
               (= Events/PLAY_MOVE (:code evt)))
          (let [pss (:context evt)
                src (:source evt)
                cmd (readJsonKW src)]
            (log/debug "rec'ved cmd %s from session %s" src pss)
            (.enqueue me cmd))))

      BoardAPI

      (getCur [_] (aget #^"[Ljava.lang.Object;" actors 0))
      (isActive [_] (.getv impl :gameon))

      (registerPlayers [this p1 p2]
        (let [which (if (> (randomSign) 0) p1 p2)]
          (aset #^"[Ljava.lang.Object;" actors 0 which)
          (aset #^"[Ljava.lang.Object;" actors 2 p2)
          (aset #^"[Ljava.lang.Object;" actors 1 p1)
          (.setv impl :gameon true)))

      (getPlayer2 [_] (aget #^"[Ljava.lang.Object;" actors 2))
      (getPlayer1 [_] (aget #^"[Ljava.lang.Object;" actors 1))

      (dequeue [this cmd]
        (let [cp (.getCur this)
              op (.getOther this cp)
              gvs (vec grid)
              src (if (some? cmd)
                    {:grid (vec grid) :cmd cmd}
                    {:grid (vec grid)})
              ^Session cpss (:session cp)
              ^Session opss (:session op)]
          (->> (reifySSEvent Events/POKE_WAIT
                             (assoc src :pnum (.number opss))
                             opss)
               (.sendMsg room))
          (->> (reifySSEvent Events/POKE_MOVE
                             (assoc src :pnum (.number cpss))
                             cpss)
               (.sendMsg room))))

      (repoke [this]
        (let [^Session pss (:session (.getCur this))]
          (->> (reifySSEvent Events/POKE_MOVE
                             {:pnum (.number pss)
                              :grid (vec grid) }
                             pss)
               (.sendMsg room))))

      (enqueue [this src]
        (let [cmd (dissoc src :grid)
              gvs (:grid src)]
          (if (and (>= (:cell cmd) 0)
                   (< (:cell cmd) numcells)
                   (= (:value (.GetCur this)
                              (:value cmd)))
                   (= _cvz_ (aget grid (:cell cmd))))
            (do
              (aset ^longs grid (:cell cmd)
                    (long (:value cmd)))
              (.checkWin this cmd))
            (.repoke this))))

      (checkWin [this cmd]
        (log/debug "checking for win %s, pos= %s"
                   (:color cmd)
                   (:cell cmd))
        (log/debug "current grid = %s" (vec grid))
        (if-some [combo (.isWinner this (.getCur this)) ]
          (.endGame this cmd combo)
          (if (.isStalemate this)
            (.drawGame this cmd)
            (.toggleActor this cmd))))

      (broadcastStatus [this ecode data status]
        (let [src (merge {:grid (vec grid)
                          :status status }
                         data)]
          (->> (reifyNWEvent ecode src)
               (.sendMsg room))))

      (drawGame [this cmd]
        (.onStopReset this)
        (.broadcastStatus this
                          Events/STOP
                          {:cmd cmd :combo []} 0))

      (endGame [this cmd combo]
        (let [^Session pss (:session (.getCur this))]
          (log/debug "game to end, winner found! combo = %s" combo)
          (.onStopReset this)
          (.broadcastStatus this
                            Events/STOP
                            {:cmd cmd :combo combo}
                            (.number pss))))

      (toggleActor [this cmd]
        (aset #^"[Ljava.lang.Object;" actors 0
              (.getOther this (.getCur this)))
        (.dequeue this cmd))

      (finz [this] (.onStopReset this))

      (onStopReset [this]
        (.setv impl :gameon false))

      (isStalemate [_]
        (not (some #(== _cvz_ %) (seq grid))))

      (isWinner [this actor]
        (log/debug "test isWinner - actor value = %s" (:value actor))
        (some (fn [r]
                (if (every? #(= (:value actor) %)
                            ;;(map #(aget grid %) r))
                            (let [xxx
                            (amap ^longs r idx ret
                                  (long (aget ^longs grid (aget ^longs r idx))))]
                              (log/debug "test one row === %s" (vec xxx))
                              xxx))
                  (vec r)
                  nil))
              goalspace))

      (getOther [_ cp]
        (condp identical? cp
          (aget #^"[Ljava.lang.Object;" actors 1)
          (aget #^"[Ljava.lang.Object;" actors 2)
          ;;else
          (aget #^"[Ljava.lang.Object;" actors 2)
          (aget #^"[Ljava.lang.Object;" actors 1)
          nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

