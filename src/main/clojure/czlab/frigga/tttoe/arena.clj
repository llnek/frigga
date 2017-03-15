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

  czlab.frigga.tttoe.arena

  (:require
    [czlab.xlib.util.str :refer [hgl? strim]]
    [czlab.xlib.util.logging :as log]
    [czlab.xlib.util.core
    :refer
    [MubleObj! RandomSign]])

  (:use [czlab.xlib.util.format]
        [czlab.cocos2d.games.meta]
        [czlab.odin.event.core])

  (:import
    [com.zotohlab.odin.game Game PlayRoom GameEngine
    Player PlayerSession]
    [com.zotohlab.skaro.core Muble]
    [com.zotohlab.odin.event Msgs Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private CV_Z 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mapGoalSpace ""

  [bsize]

  (with-local-vars
    [dx (long-array bsize 0)
     dy (long-array bsize 0)
     rowsp (transient [])
     colsp (transient []) ]
    (doseq [r (range 0 bsize)
           :let [h (long-array bsize 0)
                 v (long-array bsize 0) ]]
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
(defn ReifyPlayer ""

  [idValue pcolor psession]

  {:value idValue
   :color pcolor
   :session psession })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol BoardAPI

  ""

  (BroadcastStatus [_ ecode cmd status] )
  (RegisterPlayers [_ p1 p2])
  (Engine [_])
  (GetCur [_])
  (GetOther [_ a])
  (IsActive [_])
  (GetPlayer2 [_])
  (GetPlayer1 [_])
  (Enqueue [_ cmd])
  (CheckWin [_ cmd])
  (DrawGame [_ cmd])
  (EndGame [_ cmd combo])
  (ToggleActor [_ cmd])
  (OnStopReset [_])
  (Repoke [_])
  (Dequeue [_ cmd])
  (Finz [_])
  (IsStalemate [_])
  (IsWinner [_ a]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ReifyArena ""

  [^GameEngine theEngine options]

  (let [bsize (or (:size options) 3)
        grid (long-array (* bsize bsize) CV_Z)
        goalspace (mapGoalSpace bsize)
        actors (object-array 3)
        numcells (alength grid)
        impl (MubleObj! {:gameon false}) ]

    (reify

      BoardAPI

      (GetCur [_] (aget #^"[Ljava.lang.Object;" actors 0))
      (IsActive [_] (.getv impl :gameon))

      (Engine [_] theEngine)

      (RegisterPlayers [this p1 p2]
        (let [which (if (> (RandomSign) 0) p1 p2)]
          (aset #^"[Ljava.lang.Object;" actors 0 which)
          (aset #^"[Ljava.lang.Object;" actors 2 p2)
          (aset #^"[Ljava.lang.Object;" actors 1 p1)
          (.setv impl :gameon true)))

      (GetPlayer2 [_] (aget #^"[Ljava.lang.Object;" actors 2))
      (GetPlayer1 [_] (aget #^"[Ljava.lang.Object;" actors 1))

      (Dequeue [this cmd]
        (let [^PlayRoom room (.container theEngine)
              cp (.GetCur this)
              op (.GetOther this cp)
              gvs (vec grid)
              src (if (some? cmd)
                    {:grid (vec grid) :cmd cmd}
                    {:grid (vec grid) })
              ^PlayerSession cpss (:session cp)
              ^PlayerSession opss (:session op) ]
          (->> (ReifySSEvent Events/POKE_WAIT
                             (assoc src :pnum (.number opss))
                             opss)
               (.sendMsg room))
          (->> (ReifySSEvent Events/POKE_MOVE
                             (assoc src :pnum (.number cpss))
                             cpss)
               (.sendMsg room))))

      (Repoke [this]
        (let [^PlayerSession pss (:session (.GetCur this))
              ^PlayRoom room (.container theEngine)]
          (->> (ReifySSEvent Events/POKE_MOVE
                             {:pnum (.number pss)
                              :grid (vec grid) }
                             pss)
               (.sendMsg room))))

      (Enqueue [this src]
        (let [cmd (dissoc src :grid)
              gvs (:grid src)]
          (if (and (>= (:cell cmd) 0)
                   (< (:cell cmd) numcells)
                   (= (:value (.GetCur this)
                              (:value cmd)))
                   (= CV_Z (aget grid (:cell cmd))))
            (do
              (aset ^longs grid (:cell cmd)
                    (long (:value cmd)))
              (.CheckWin this cmd))
            (.Repoke this))))

      (CheckWin [this cmd]
        (log/debug "checking for win %s, pos= %s"
                   (:color cmd)
                   (:cell cmd))
        (log/debug "current grid = %s" (vec grid))
        (if-some [combo (.IsWinner this (.GetCur this)) ]
          (.EndGame this cmd combo)
          (if (.IsStalemate this)
            (.DrawGame this cmd)
            (.ToggleActor this cmd))))

      (BroadcastStatus [this ecode data status]
        (let [^PlayRoom room (.container theEngine)
              src (merge {:grid (vec grid)
                          :status status }
                         data)]
          (->> (ReifyNWEvent ecode src)
               (.sendMsg room))))

      (DrawGame [this cmd]
        (.OnStopReset this)
        (.BroadcastStatus this
                          Events/STOP
                          {:cmd cmd :combo []} 0))

      (EndGame [this cmd combo]
        (let [^PlayerSession pss (:session (.GetCur this))]
          (log/debug "game to end, winner found! combo = %s" combo)
          (.OnStopReset this)
          (.BroadcastStatus this
                            Events/STOP
                            {:cmd cmd :combo combo}
                            (.number pss))))

      (ToggleActor [this cmd]
        (aset #^"[Ljava.lang.Object;" actors 0
              (.GetOther this (.GetCur this)))
        (.Dequeue this cmd))

      (Finz [this] (.OnStopReset this))

      (OnStopReset [this]
        (.setv impl :gameon false))

      (IsStalemate [_]
        (not (some #(== CV_Z %) (seq grid))))

      (IsWinner [this actor]
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

      (GetOther [_ cp]
        (condp identical? cp
          (aget #^"[Ljava.lang.Object;" actors 1)
          (aget #^"[Ljava.lang.Object;" actors 2)
          ;;else
          (aget #^"[Ljava.lang.Object;" actors 2)
          (aget #^"[Ljava.lang.Object;" actors 1)

          nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
