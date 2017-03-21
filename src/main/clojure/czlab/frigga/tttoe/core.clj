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

  czlab.frigga.tttoe.core

  (:require [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.loki.net.core]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game GameImpl Arena GameRoom]
           [czlab.loki.sys Player Session]
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
  (fmtStatus [_ cmd status] )
  (registerPlayers [_ p1 p2])
  (getCur [_])
  (getOther [_ a])
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
  (isStalemate [_])
  (isWinner [_ a]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tictactoe
  "" ^GameImpl [^GameRoom room sessions]

  (let [grid (long-array (* 3 3) _cvz_)
        goalspace (mapGoalSpace 3)
        actors (object-array 3)
        numcells (alength grid)
        impl (muble<> )]
    (reify

      GameImpl

      (playerGist [me id]
        (some #(let [s (:session %)]
                 (if (= id
                        (.. ^Session
                            s
                            player id))
                   (dissoc % :session))) (drop 1 actors)))

      (startRound [_ _])
      (endRound [_])

      (init [me arg]
        (log/debug "tictactoe: init called()")
        (.setv impl :arena (:arena arg))
        (let [p1 (reifyPlayer (long \X) "X" (first sessions))
              p2 (reifyPlayer (long \O) "O" (last sessions))]
          (.registerPlayers me p1 p2)))

      (start [me _]
        (doall
          (for [x (range 0 numcells)] (aset-long grid x _cvz_)))
        (log/debug "tictactoe: start called()")
        (log/debug "tictactoe: grid = %s" (vec grid))
        (let [which (if (> (randSign) 0) 1 2)]
          (->> (aget #^"[Ljava.lang.Object;" actors which)
               (aset #^"[Ljava.lang.Object;" actors 0))
          (.dequeue me nil)))

      (onEvent [me evt]
        (log/debug "game engine got an update %s" evt)
        (if (isMove? evt)
          (let [b (:body evt)]
            (log/debug "rec'ved cmd %s from session %s" b (:context evt))
            (.enqueue me b))))

      BoardAPI

      (getCur [_] (aget #^"[Ljava.lang.Object;" actors 0))

      (registerPlayers [this p1 p2]
        (aset #^"[Ljava.lang.Object;" actors 2 p2)
        (aset #^"[Ljava.lang.Object;" actors 1 p1)
        (log/debug "Player2: %s" p2)
        (log/debug "Player1: %s" p1))

      (getPlayer2 [_] (aget #^"[Ljava.lang.Object;" actors 2))
      (getPlayer1 [_] (aget #^"[Ljava.lang.Object;" actors 1))

      (dequeue [this cmd]
        (let [cp (.getCur this)
              op (.getOther this cp)
              src {:grid (vec grid)}
              src (if (map? cmd)
                    (assoc src :cmd cmd) src)
              ^Session cpss (:session cp)
              ^Session opss (:session op)]
          (pokeWait! room
                     (assoc src :pnum (.number opss)) opss)
          (pokeMove! room
                     (assoc src :pnum (.number cpss)) cpss)))

      (repoke [this]
        (let [^Session pss (:session (.getCur this))]
          (pokeMove! room
                     {:pnum (.number pss)
                      :grid (vec grid) } pss)))

      (enqueue [this cmd]
        (let [cell (or (:cell cmd) 911)
              cval (:value cmd)]
          (if (and (>= cell 0)
                   (< cell numcells)
                   (= (:value (.getCur this)) cval)
                   (= _cvz_ (aget grid cell)))
            (do
              (aset ^longs grid cell (long cval))
              (.checkWin this cmd))
            (.repoke this))))

      (checkWin [this cmd]
        (log/debug "current grid = %s" (vec grid))
        (log/debug "checking for %s" cmd)
        (if-some [combo (.isWinner this
                                   (.getCur this))]
          (.endGame this cmd combo)
          (if (.isStalemate this)
            (.drawGame this cmd)
            (.toggleActor this cmd))))

      (fmtStatus [_ data status]
        (merge {:grid (vec grid) :status status} data))

      (drawGame [this cmd]
        (.onStopReset this)
        (stop! room (.fmtStatus this
                                {:cmd cmd :combo []} 0)))

      (endGame [this cmd combo]
        (let [^Session pss (:session (.getCur this))
              pnum (.number pss)]
          (log/debug "game to end, winner found! combo = %s" combo)
          (.onStopReset this)
          (stop! room
                 (.fmtStatus this {:cmd cmd :combo combo} pnum))))

      (toggleActor [this cmd]
        (aset #^"[Ljava.lang.Object;"
              actors 0
              (.getOther this (.getCur this)))
        (.dequeue this cmd))

      (onStopReset [this]
        (some-> ^Arena (.getv impl :arena) .stop))

      (isStalemate [_]
        (not (some #(= _cvz_ %) (seq grid))))

      (isWinner [this actor]
        (log/debug "test isWinner - actor value = %s" (:value actor))
        (some (fn [r]
                (if (every? #(= (:value actor) %)
                            (let [xxx (amap ^longs r idx ret
                                            (long (aget ^longs grid
                                                        (aget ^longs r idx))))]
                              (log/debug "test one row === %s" (vec xxx))
                              xxx))
                  (vec r))) goalspace))

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

