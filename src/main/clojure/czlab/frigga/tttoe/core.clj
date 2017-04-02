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

  (:import [czlab.jasal Restartable Startable]
           [czlab.loki.game Game]
           [czlab.loki.sys Room]
           [czlab.loki.net Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _bsize_ 3)
(def ^:private _cvz_ 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genGoalSpace  "" [bsize]
  (let [arr #(preduce<vec>
                (fn [a _]
                  (conj! a (long-array bsize 0))) (range %))
        rows (arr bsize)
        cols (arr bsize)
        dags (arr 2)]
    (doseq [n (range bsize)]
      (doseq [k (range bsize)]
        (aset ^longs (nth rows n) k ^long (+ (* bsize n) k))))
    (doseq [n (range bsize)]
      (doseq [k (range bsize)
              :let [r0 (nth rows 0)]]
        (aset ^longs (nth cols n) k ^long (+ (aget ^longs r0 n)
                                             (* bsize k)))))
    (doseq [n (range bsize)]
      (aset ^longs
            (first dags)
            n
            (aget ^longs (nth rows n) n))
      (aset ^longs
            (last dags)
            n
            (aget ^longs (nth rows n) (- bsize n 1))))
    (into [] (concat dags rows cols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(comment
  (doseq [v (genGoalSpace 3)]
  (println (seq v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gistPlayer
  "" [idValue pcolor psession]
  {:value idValue :color pcolor :session psession })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol BoardAPI
  ""
  (fmtStatus [_ cmd status] )
  (gcur [_])
  (gother [_ a])
  (player [_ _])
  (enqueue [_ cmd])
  (checkWin [_ cmd])
  (drawGame [_ cmd])
  (endGame [_ cmd combo])
  (toggleActor [_ cmd])
  (onStopReset [_])
  (repoke [_])
  (dequeue [_ cmd])
  (stalemate? [_])
  (winner? [_ a]))

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
    (let [{:keys [sessions actors]} @data
          [s1 s2] sessions
          p1 (gistPlayer (long \X) :X s1)
          p2 (gistPlayer (long \O) :O s2)]
      (aset #^"[Ljava.lang.Object;" actors 2 p2)
      (aset #^"[Ljava.lang.Object;" actors 1 p1)
      ;;(log/debug "tictactoe: init: state= %s" @data)
      (log/debug "Player2: %s" p2)
      (log/debug "Player1: %s" p1)))

  (restart [me] (.restart me _empty-map_))
  (restart [me _] (.start me _))
  (start [me] (.start me _empty-map_))
  (start [me _]
    (let [{:keys [actors grid]} @data
          sz (alength ^longs grid)]
      (doall (map #(aset-long grid % _cvz_)
                  (range sz)))
      (log/debug "tictactoe: start called()")
      (log/debug "tictactoe: grid = %s" (vec grid))
      (->> (if (> (randSign) 0) 1 2)
           (aget #^"[Ljava.lang.Object;" actors)
           (aset #^"[Ljava.lang.Object;" actors 0))
      (. me dequeue _empty-map_)))
  (onEvent [me evt]
    (let [{:keys [context body]} evt]
      (log/debug "game got an update %s" evt)
      (cond
        (isMove? evt)
        (do->nil
          (log/debug "rec'ved %s from [%s]" body context)
          (. me enqueue body))
        (isQuit? evt)
        Events/TEAR_DOWN)))

  BoardAPI

  (gcur [me] (.player me 0))
  (player [_ n]
    (aget #^"[Ljava.lang.Object;" (:actors @data) n))

  (dequeue [me cmd]
    (let [{:keys [room grid]} @data
          cp (.gcur me)
          op (.gother me cp)
          src {:grid (vec grid)
               :cmd cmd}
          cpss (:session cp)
          opss (:session op)]
      (doto room
        (pokeWait! (assoc src :pnum (:number @opss)) opss)
        (pokeMove! (assoc src :pnum (:number @cpss)) cpss))))

  (repoke [me]
    (let [pss (:session (.gcur me))
          {:keys [room grid]} @data]
      (pokeMove! room
                 {:pnum (:number @pss)
                  :grid (vec grid)} pss)))

  (enqueue [me cmd]
    (let [{:keys [cell value]} cmd
          {:keys [grid]} @data
          sz (alength ^longs grid)]
      (if (and (number? cell)
               (>= cell 0)
               (< cell sz)
               (= (:value (.gcur me)) value)
               (= _cvz_ (aget ^longs grid cell)))
        (do
          (aset-long grid cell (long value))
          (. me checkWin cmd))
        (.repoke me))))

  (checkWin [me cmd]
    (let [{:keys [grid]} @data]
      (log/debug "current grid = %s" (vec grid))
      (log/debug "checking for %s" cmd)
      (if-some [combo (.winner? me (.gcur me))]
        (.endGame me cmd combo)
        (if (.stalemate? me)
          (.drawGame me cmd)
          (.toggleActor me cmd)))))

  (fmtStatus [_ msg status]
    (let [{:keys [grid]} @data]
      (merge msg {:grid (vec grid) :status status})))

  (drawGame [me cmd]
    (log/debug "game to end, no winner!!!")
    (.onStopReset me)
    (bcast! (:room @data)
            Events/GAME_TIE
            (.fmtStatus me
                        {:cmd cmd :combo []} 0)))

  (endGame [me cmd combo]
    (let [pss (:session (.gcur me))
          pnum (:number @pss)]
      (log/debug "game to end, winner found! combo = %s" combo)
      (.onStopReset me)
      (bcast! (:room @data)
              Events/GAME_WON
              (.fmtStatus me {:cmd cmd :combo combo} pnum))))

  (toggleActor [me cmd]
    (aset #^"[Ljava.lang.Object;"
          (:actors @data) 0 (.gother me (.gcur me)))
    (.dequeue me cmd))

  (onStopReset [_] (.stop ^Startable (:room @data)))

  (stalemate? [_]
    (not (some #(= _cvz_ %) (seq (:grid @data)))))

  (winner? [me actor]
    (let [{:keys [grid]} @data]
      (log/debug "isWinner? actor-value = %s" (:value actor))
      (some (fn [r]
              (if (every? #(= (:value actor) %)
                          (let [a (amap ^longs r idx ret
                                          (long (aget ^longs grid
                                                      (aget ^longs r idx))))]
                            (log/debug "test row: %s" (vec a))
                            a))
              (vec r))) (:goalspace @data))))

  (gother [_ cp]
    (let [{:keys [actors]} @data]
      (condp identical? cp
        (aget #^"[Ljava.lang.Object;" actors 1)
        (aget #^"[Ljava.lang.Object;" actors 2)
        (aget #^"[Ljava.lang.Object;" actors 2)
        (aget #^"[Ljava.lang.Object;" actors 1)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tictactoe
  "" ^Game [room sessions]

  (let [grid (long-array (* _bsize_ _bsize_) _cvz_)]
    (entity<> TicTacToe
            {:goalspace (genGoalSpace _bsize_)
             :sessions sessions
             :grid grid
             :room room
             :actors (object-array 3)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

