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

  (:require [czlab.loki.xpis :as loki :refer :all]
            [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.loki.net.core]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.jasal Startable Initable Restartable]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getPlayer [me n]
  (aget #^"[Ljava.lang.Object;" (:actors @me) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gcur "" [me] (getPlayer me 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gother "" [me cp]
  (let [{:keys [actors]} @me]
    (condp identical? cp
      (aget #^"[Ljava.lang.Object;" actors 1)
      (aget #^"[Ljava.lang.Object;" actors 2)
      (aget #^"[Ljava.lang.Object;" actors 2)
      (aget #^"[Ljava.lang.Object;" actors 1)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- dequeue "" [me cmd]
  (let [{:keys [room grid]} @me
        cp (gcur me)
        op (gother me cp)
        src {:grid (vec grid)
             :cmd cmd}
        cpss (:session cp)
        opss (:session op)]
    (doto room
      (pokeWait! (assoc src :pnum (:number @opss)) opss)
      (pokeMove! (assoc src :pnum (:number @cpss)) cpss))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toggleActor "" [me cmd]
  (aset #^"[Ljava.lang.Object;"
        (:actors @me) 0 (gother me (gcur me)))
  (dequeue me cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- onStopReset "" [me] (.stop ^Startable (:room @me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- stalemate? "" [me]
  (not (some #(= _cvz_ %) (seq (:grid @me)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- winner? "" [me actor]
  (let [{:keys [grid]} @me]
    (log/debug "isWinner? actor-value = %s" (:value actor))
    (some (fn [r]
            (if (every? #(= (:value actor) %)
                        (let [a (amap ^longs r idx ret
                                        (long (aget ^longs grid
                                                    (aget ^longs r idx))))]
                          (log/debug "test row: %s" (vec a))
                          a))
            (vec r))) (:goalspace @me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- repoke "" [me]
  (let [pss (:session (gcur me))
        {:keys [room grid]} @me]
    (pokeMove! room
               {:pnum (:number @pss)
                :grid (vec grid)} pss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fmtStatus "" [me msg status]
  (let [{:keys [grid]} @me]
    (merge msg {:grid (vec grid) :status status})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- drawGame [me cmd]
  (log/debug "game to end, no winner!!!")
  (onStopReset me)
  (bcast! (:room @me)
          ::loki/game-tie
          (fmtStatus me
                     {:cmd cmd :combo []} 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- endGame "" [me cmd combo]
  (let [pss (:session (gcur me))
        pnum (:number @pss)]
    (log/debug "game to end, winner found! combo = %s" combo)
    (onStopReset me)
    (bcast! (:room @me)
            ::loki/game-won
            (fmtStatus me {:cmd cmd :combo combo} pnum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- checkWin "" [me cmd]
  (let [{:keys [grid]} @me]
    (log/debug "current grid = %s" (vec grid))
    (log/debug "checking for %s" cmd)
    (if-some [combo (winner? me (gcur me))]
      (endGame me cmd combo)
      (if (stalemate? me)
        (drawGame me cmd)
        (toggleActor me cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- enqueue "" [me cmd]
  (let [{:keys [cell value]} cmd
        {:keys [grid]} @me
        sz (alength ^longs grid)]
    (if (and (number? cell)
             (>= cell 0)
             (< cell sz)
             (= (:value (gcur me)) value)
             (= _cvz_ (aget ^longs grid cell)))
      (do
        (aset-long grid cell (long value))
        (checkWin me cmd))
      (repoke me))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable TicTacToe
  GameImpl
  (get-player-gist [me id]
    (some #(let [s (:session %)]
             (if (= id (id?? (:player @s)))
               (dissoc % :session)))
          (->> (:actors @me) (drop 1))))
  (start-round [_ _])
  (end-round [_])
  (on-game-event [me evt]
    (let [{:keys [context body]} evt]
      (log/debug "game got an update %s" (prettyEvent evt))
      (cond
        (isMove? evt)
        (do->nil
          (log/debug "rec'ved %s from [%s]" body (id?? context))
          (enqueue me body))
        (isQuit? evt)
        ::loki/tear-down)))

  Initable
  (init [me _]
    (log/debug "tictactoe: init called()")
    (let [{:keys [sessions actors]} @me
          [s1 s2] sessions
          p1 (gistPlayer (long \X) :X s1)
          p2 (gistPlayer (long \O) :O s2)]
      (aset #^"[Ljava.lang.Object;" actors 2 p2)
      (aset #^"[Ljava.lang.Object;" actors 1 p1)
      ;;(log/debug "tictactoe: init: state= %s" @data)
      (log/debug "Player2: %s" (dissoc p2 :session))
      (log/debug "Player1: %s" (dissoc p1 :session))))
  Restartable
  (restart [me] (.restart me nil))
  (restart [me _] (.start me _))
  Startable
  (start [me] (.start me nil))
  (start [me _]
    (let [{:keys [actors grid]} @me
          sz (alength ^longs grid)]
      (doall (map #(aset-long grid % _cvz_)
                  (range sz)))
      (log/debug "tictactoe: start called()")
      (log/debug "tictactoe: grid = %s" (vec grid))
      (->> (if (> (randSign) 0) 1 2)
           (aget #^"[Ljava.lang.Object;" actors)
           (aset #^"[Ljava.lang.Object;" actors 0))
      (dequeue me nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tictactoe
  "" [room sessions]

  (let [grid (long-array (* _bsize_ _bsize_) _cvz_)]
    (mutable<> TicTacToe
               {:goalspace (genGoalSpace _bsize_)
                :sessions sessions
                :grid grid
                :room room
                :actors (object-array 3)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

