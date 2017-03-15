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

  (:require
    [czlab.xlib.util.core :refer [MubleObj! trap! ]]
    [czlab.xlib.util.logging :as log]
    [czlab.xlib.util.str :refer [strim hgl?]])

  (:use [czlab.xlib.util.format]
        [czlab.cocos2d.games.meta]
        [czlab.odin.event.core]
        [czlab.frigga.core.util]
        [czlab.frigga.tttoe.arena])

  (:import
    [com.zotohlab.odin.game Game PlayRoom GameEngine
    Player PlayerSession]
    [com.zotohlab.frwk.core Morphable]
    [com.zotohlab.odin.event EventError Msgs Events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- onSessionMsg ""

  [^GameEngine eng evt stateRef]

  (condp = (:code evt)

    Events/REPLAY
    (.restart eng {})

    Events/PLAY_MOVE
    (let [bd (:arena (.state eng))
          pss (:context evt)
          src (:source evt)
          cmd (ReadJsonKW src)]
      (log/debug "rec'ved cmd %s from session %s" src pss)
      (Enqueue bd cmd))

    Events/STARTED
    (do
      (log/debug "received started event %s" evt)
      (let [^PlayerSession ps (:context evt) ]
      (dosync
        (let [m (dissoc @stateRef (.id ps)) ]
          (if (empty? m)
            (do
              (ref-set stateRef {})
              (.start eng {}))
            (ref-set stateRef m))))))

    (log/warn "game engine: unhandled session msg %s" evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn TicTacToe ""

  ^GameEngine
  [stateAtom stateRef]

  (reify

    GameEngine

    (initialize [this players]
      (dosync
        (ref-set stateRef (MapPlayers players))
        (reset! stateAtom {:players players})))

    (start [this options]
      (let [[ps1 ps2] (:players @stateAtom)
            p1 (ReifyPlayer (long \X) \X ps1)
            p2 (ReifyPlayer (long \O) \O ps2)
            bd (ReifyArena this {}) ]
        (swap! stateAtom assoc :arena bd)
        (RegisterPlayers bd p1 p2)
        (Dequeue bd nil)))

    (update [this evt]
      (log/debug "game engine got an update %s" evt)
      (condp = (:type evt)
        Msgs/NETWORK
        (trap! EventError "Unexpected network event.")

        Msgs/SESSION
        (onSessionMsg this evt stateRef)

        (log/warn "game engine: unhandled msg %s" evt)))

    (restart [this options]
      (log/debug "restarting tictactoe game one more time...")
      (let [parr (:players @stateAtom)
            m (MapPlayers parr)]
        (dosync (ref-set stateRef m))
        (BCastAll (.container this)
                  Events/RESTART (MapPlayersEx parr))))

    (ready [this room]
      (swap! stateAtom assoc :room room)
      (BCastAll (.container this)
                Events/START
                (MapPlayersEx (:players @stateAtom))))

    (container [_] (:room @stateAtom))

    (startRound [_ obj])
    (endRound [_ obj])

    (stop [_] )
    (finz [_] )

    (state [_] @stateAtom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

