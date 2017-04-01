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

  (:import [czlab.loki.game Game]
           [czlab.loki.sys Room]
           [czlab.loki.net Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _bsize_ 3)
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
  {:value idValue :color pcolor :session psession })

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
(defentity TicTacToe
  Game
  (playerGist [me id]
    (some #(let [s (:session %)]
             (if (= id (id?? (:player @s)))
               (dissoc % :session)))
          (->> (:actors @data) (drop 1))))
  (startRound [_ _])
  (endRound [_])
  (init [me _]
    (log/debug "tictactoe: init called()")
    (let [[s1 s2] (:sessions @data)]
      (. ^BoardAPI
         me
         registerPlayers
         (reifyPlayer (long \X) :X s1)
         (reifyPlayer (long \O) :O s2))))
  (restart [me] (.restart ^Restartable me _empty-map_))
  (restart [me _] (.start ^Startable me _))
  (start [me] (.start ^Startable me _empty-map_))
  (start [me _]
    (let [which (if (> (randSign) 0) 1 2)
          {:keys [numcells
                  actors grid]} @data
          _ (doall
              (for [x (range 0 numcells)]
                (aset-long grid x _cvz_)))]
      (log/debug "tictactoe: start called()")
      (log/debug "tictactoe: grid = %s" (vec grid))
      (->> (aget #^"[Ljava.lang.Object;" actors which)
           (aset #^"[Ljava.lang.Object;" actors 0))
      (. ^BoardAPI me dequeue _empty-map_)))
  (onEvent [me evt]
    (let [{:keys [context body]} evt]
      (log/debug "game got an update %s" evt)
      (cond
        (isMove? evt)
        (do->nil
          (log/debug "rec'ved %s from [%s]" body context)
          (. ^BoardAPI me enqueue body))
        (isQuit? evt)
        Events/TEAR_DOWN)))

  BoardAPI
  (gcur [_] (. ^BoardAPI me player 0))

  (regoPlayers [this p1 p2]
    (let [{:keys [actors]} @data]
      (aset #^"[Ljava.lang.Object;" actors 2 p2)
      (aset #^"[Ljava.lang.Object;" actors 1 p1)
      (log/debug "Player2: %s" p2)
      (log/debug "Player1: %s" p1)))
  (player [_ n]
    (aget #^"[Ljava.lang.Object;" (:actors @data) n))

  (dequeue [me cmd]
    (let [{:keys [grid]} @data
          cp (.gcur me)
          op (.gother me cp)
          src {:grid (vec grid)}
          src (if (map? cmd)
                (assoc src :cmd cmd) src)
          cpss (:session cp)
          opss (:session op)]
      (pokeWait! room
                 (assoc src :pnum (:number @opss)) opss)
      (pokeMove! room
                 (assoc src :pnum (:number @cpss)) cpss)))

  (repoke [me]
    (let [pss (:session (.gcur me))
          {:keys [grid]} @data]
      (pokeMove! room
                 {:pnum (:number @pss)
                  :grid (vec grid)} pss)))

  (enqueue [me cmd]
    (let [{:keys [numcells grid]} @data
          cell (or (:cell cmd) 911)
          cval (:value cmd)]
      (if (and (>= cell 0)
               (< cell numcells)
               (= (:value (.gcur me)) cval)
               (= _cvz_ (aget grid cell)))
        (do
          (aset ^longs grid cell (long cval))
          (. ^BoardAPI me checkWin cmd))
        (.repoke ^BoardAPI me))))

  (checkWin [me cmd]
    (log/debug "current grid = %s" (vec grid))
    (log/debug "checking for %s" cmd)
    (if-some [combo (. me isWinner (.gcur me))]
      (. me endGame cmd combo)
      (if (.isStalemate me)
        (.drawGame me cmd)
        (.toggleActor me cmd))))

  (fmtStatus [_ data status]
    (let [{:keys [grid]} @data]
      (merge data {:grid (vec grid) :status status})))

  (drawGame [me cmd]
    (log/debug "game to end, no winner!!!")
    (.onStopReset me)
    (bcast! room
            Events/GAME_TIE
            (.fmtStatus me
                        {:cmd cmd :combo []} 0)))

  (endGame [me cmd combo]
    (let [pss (:session (.gcur me))
          pnum (:number @pss)]
      (log/debug "game to end, winner found! combo = %s" combo)
      (.onStopReset me)
      (bcast! room
              Events/GAME_WON
              (.fmtStatus me {:cmd cmd :combo combo} pnum))))

  (toggleActor [me cmd]
    (aset #^"[Ljava.lang.Object;"
          actors 0 (.gother me (.gcur me)))
    (.dequeue me cmd))

  (onStopReset [_] (.stop ^Startable room))

  (isStalemate [_]
    (not (some #(= _cvz_ %) (seq (:grid data)))))

  (isWinner [me actor]
    (let [{:keys [grid]} @data]
      (log/debug "isWinner? actor-value = %s" (:value actor))
      (some (fn [r]
              (if (every? #(= (:value actor) %)
                          (let [xxx (amap ^longs r idx ret
                                          (long (aget ^longs grid
                                                      (aget ^longs r idx))))]
                            (log/debug "test row: %s" (vec xxx))
                            xxx))
              (vec r))) goalspace)))

  (gother [_ cp]
    (condp identical? cp
      (aget #^"[Ljava.lang.Object;" actors 1)
      (aget #^"[Ljava.lang.Object;" actors 2)
      ;;else
      (aget #^"[Ljava.lang.Object;" actors 2)
      (aget #^"[Ljava.lang.Object;" actors 1)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

