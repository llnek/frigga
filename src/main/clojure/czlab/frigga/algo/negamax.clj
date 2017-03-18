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

  (:require [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.loki.game Board]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _pinf_ 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol NegaAlgoAPI "" (evaluate [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol NegaSnapshotAPI
  ""
  (setLastBestMove [_ m] )
  (setOther [_ o] )
  (setCur [_ c] )
  (setState [_ s] )
  (lastBestMove [_] )
  (other [_] )
  (cur [_] )
  (state [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- negaMaxAlgo
  "The Nega-Max algo implementation"
  [^Board board game maxDepth depth alpha beta]

  (if (or (== depth 0)
          (.isOver board game))
    (.evalScore board game)
    ;;:else
    (with-local-vars
      [openMoves (.getNextMoves board game)
       bestValue (- _pinf_)
       localAlpha alpha
       halt false
       rc 0
       bestMove (nth openMoves 0) ]
      (when (== depth maxDepth)
        (setLastBestMove game (nth @openMoves 0))) ;; this will change overtime, most likely
      (loop [n 0]
        (when-not (or (> n (count @openMoves))
                      (true? @halt))
          (let [move (nth @openMoves n) ]
            (doto board
              (.makeMove game move)
              (.switchPlayer game))
            (var-set rc (- (negaMaxAlgo board
                                        game
                                        maxDepth
                                        (dec depth)
                                        (- beta) (- @localAlpha))))
            (doto board
              (.switchPlayer game)
              (.unmakeMove game move))
            (var-set bestValue (Math/max (long @bestValue) (long @rc)))
            (when (< @localAlpha @rc)
              (var-set localAlpha @rc)
              (var-set bestMove move)
              (when (== depth maxDepth)
                (setLastBestMove game move))
              (when (>= @localAlpha beta)
                (var-set halt true)))
            (recur (inc n) ))))
      @bestValue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn negamax<>
  "Implementation of nega-max" [^Board board]

  (reify
    NegaAlgoAPI
    (evaluate [_]
      (let [snapshot (.takeSnapshot board) ]
        (negaMaxAlgo board snapshot 10 10 (- _pinf_) _pinf_)
        (lastBestMove snapshot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn snapshot<>
  "Create a snapshot" []

  (let [impl (muble<>)]
    (reify
      NegaSnapshotAPI
      (setLastBestMove [_ m] (.setv impl :lastbestmove m))
      (setOther [_ o] (.setv impl :other o))
      (setCur [_ c] (.setv impl :cur c))
      (setState [_ s] (.setv impl :state s))
      (lastBestMove [_] (.getv impl :lastbestmove))
      (other [_] (.getv impl :other))
      (cur [_] (.getv impl :cur))
      (state [_] (.getv impl :state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

