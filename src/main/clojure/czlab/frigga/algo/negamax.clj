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

  czlab.frigga.algo.negamax

  (:require [czlab.loki.xpis :as loki :refer :all]
            [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.basal.core GenericMutable]
           [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol NegaAlgoAPI "" (eval-algo [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable NegaSnapshotObj )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _pinf_ 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- negaMaxAlgo
  "The Nega-Max algo implementation"
  [board game maxDepth depth alpha beta]

  (if (or (== depth 0)
          (game-over? board game))
    (eval-score board game)
    ;;:else
    (let
      [mu (GenericMutable. {:openMoves (get-next-moves board game)
                            :bestValue (- _pinf_)
                            :localAlpha alpha
                            :halt? false
                            :rc 0})
       _ (setf! mu :bestMove (nth (:openMoves @mu) 0))]
      (when (== depth maxDepth)
        (setf! game
               :lastBestMove
               (nth (:openMoves @mu) 0)))
      (loop [n 0]
        (when-not (or (> n (count (:openMoves @mu)))
                      (true? (:halt? @mu)))
          (let [move (nth (:openMoves @mu) n)]
            (doto board
              (make-move game move)
              (switch-player game))
            (setf! mu
                   :rc
                   (- (negaMaxAlgo board
                                   game
                                   maxDepth
                                   (dec depth)
                                   (- beta) (- (:localAlpha @mu)))))
            (doto board
              (switch-player game)
              (unmake-move game move))
            (setf! mu
                   :bestValue
                   (Math/max (long (:rc @mu))
                             (long (:bestValue @mu))))
            (when (< (:localAlpha @mu) (:rc @mu))
              (setf! mu :localAlpha (:rc @mu))
              (setf! mu :bestMove move)
              (when (== depth maxDepth)
                (setf! game :lastBestMove move))
              (when (>= (:localAlpha @mu) beta)
                (setf! mu :halt? true)))
            (recur (inc n) ))))
      (:bestValue @mu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn negamax<>
  "Implementation of nega-max" [board]

  (reify
    NegaAlgoAPI
    (eval-algo [_]
      (let [snapshot (take-snap-shot board)]
        (negaMaxAlgo board snapshot 10 10 (- _pinf_) _pinf_)
        (:lastBestMove @snapshot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn snapshot<>
  "Create a snapshot" []

  (mutable<> NegaSnapshotObj
             {:lastBestMove nil
              :otherPlayer nil
              :curPlayer nil
              :state nil }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

