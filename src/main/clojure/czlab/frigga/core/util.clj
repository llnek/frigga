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

  czlab.frigga.core.util

  (:require
    [czlab.xlib.util.core :refer :all]
    [czlab.xlib.util.logging :as log]
    [czlab.xlib.util.str :refer [strim hgl?]])

  (:use [czlab.xlib.util.format]
        [czlab.odin.event.core])

  (:import
    [com.zotohlab.odin.game PlayRoom GameEngine PlayerSession]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn MapPlayers

  "Map ids to player sessions"

  ;; outputs map {id -> player-session}
  [players]

  (reduce #(assoc %1 (.id ^PlayerSession %2) %2)
          {}
          players))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn MapPlayersEx

  "Map ids to player [number + id]"

  ;; outputs map {id -> [player-number, player-id]}
  [players]

  {:ppids (reduce (fn [memo ^PlayerSession ps]
                      (assoc memo
                             (.id (.player ps))
                             [ (.number ps) (.id (.player ps)) ]))
                    {}
                    players) })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn BCastAll

  "Broadcast message to all player sessions"

  [^PlayRoom room code body]

  (->> (ReifyNWEvent code body)
       (.sendMsg room)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

