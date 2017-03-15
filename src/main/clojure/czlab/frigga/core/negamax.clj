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

  czlab.frigga.core.negamax

  (:require
    [czlab.xlib.util.core :refer [MubleObj! ]]
    [czlab.xlib.util.logging :as log]
    [czlab.xlib.util.str :refer [strim hgl?]])

  (:import
    [com.zotohlab.odin.game Game PlayRoom
    Board Player PlayerSession]
    [com.zotohlab.skaro.core Muble]
    [com.zotohlab.odin.core Session]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private PINF 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol NegaAlgoAPI "" (Evaluate [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol NegaSnapshotAPI

  ""

  (SetLastBestMove [_ m] )
  (SetOther [_ o] )
  (SetCur [_ c] )
  (SetState [_ s] )
  (LastBestMove [_] )
  (Other [_] )
  (Cur [_] )
  (State [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn NegaMax

  "The Nega-Max algo implementation"

  [^Board board game
   maxDepth depth alpha beta]

  (if (or (== depth 0)
          (.isOver board game))
    (.evalScore board game)
    ;;:else
    (with-local-vars
      [openMoves (.getNextMoves board game)
       bestValue (- PINF)
       localAlpha alpha
       halt false
       rc 0
       bestMove (nth openMoves 0) ]
      (when (== depth maxDepth)
        (SetLastBestMove game (nth @openMoves 0))) ;; this will change overtime, most likely
      (loop [n 0]
        (when-not (or (> n (count @openMoves))
                      (true? @halt))
          (let [move (nth @openMoves n) ]
            (doto board
              (.makeMove game move)
              (.switchPlayer game))
            (var-set rc (- (NegaMax board
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
                (SetLastBestMove game move))
              (when (>= @localAlpha beta)
                (var-set halt true)))
            (recur (inc n) ))))
      @bestValue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ReifyNegaMaxAlgo

  "Create an implementation of nega-max"

  [^Board board]

  (reify
    NegaAlgoAPI
    (Evaluate [_]
      (let [snapshot (.takeSnapshot board) ]
        (NegaMax board snapshot 10 10 (- PINF) PINF)
        (LastBestMove snapshot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ReifySnapshot

  "Create a snapshot"

  []

  (let [impl (MubleObj!)]
    (reify
      NegaSnapshotAPI
      (SetLastBestMove [_ m] (.setv impl :lastbestmove m))
      (SetOther [_ o] (.setv impl :other o))
      (SetCur [_ c] (.setv impl :cur c))
      (SetState [_ s] (.setv impl :state s))
      (LastBestMove [_] (.getv impl :lastbestmove))
      (Other [_] (.getv impl :other))
      (Cur [_] (.getv impl :cur))
      (State [_] (.getv impl :state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

