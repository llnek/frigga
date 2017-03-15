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

  czlab.frigga.pong.core

  (:require
    [czlab.xlib.util.core :refer [MubleObj! trap! ]]
    [czlab.xlib.util.logging :as log]
    [czlab.xlib.util.str :refer [strim hgl?]])

  (:use [czlab.xlib.util.format]
        [czlab.cocos2d.games.meta]
        [czlab.odin.event.core]
        [czlab.frigga.core.util]
        [czlab.frigga.pong.arena])

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
    (let [aa (:arena (.state eng))
          {:keys [source context]} evt]
      (log/debug "received paddle-move %s%s%s"
                 source " from session " context)
      (Enqueue aa evt))

    Events/STARTED
    (do
      (let [^PlayerSession pss (:context evt)
            src (:source evt)
            cmd (ReadJsonKW src)]
        (log/debug "received started-event from %s" pss)
        (dosync
          (let [m (dissoc @stateRef (.id pss)) ]
            (if (empty? m)
              (do
                (ref-set stateRef {})
                (.start eng cmd))
              (ref-set stateRef m))))))

    (log/warn "game engine: unhandled session msg %s" evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Pong ""

  ^GameEngine
  [stateAtom stateRef]

  (reify GameEngine

    ;; one time only, sets up the players
    (initialize [this players]
      (dosync
        (ref-set stateRef (MapPlayers players))
        (reset! stateAtom {:players players})))

    ;; starts a new game by creating a new arena and players
    ;; follow by starting the first point.
    (start [this options]
      (let [[ps1 ps2] (:players @stateAtom)
            p1 (ReifyPlayer (long \X) \X ps1)
            p2 (ReifyPlayer (long \O) \O ps2)
            aa (ReifyArena this options) ]
        (swap! stateAtom assoc :arena aa)
        (RegisterPlayers aa p1 p2)
        (.startRound this {:new true})))

    ;; updates from clients
    (update [this evt]
      (log/debug "game engine got an update %s" evt)
      (condp = (:type evt)
        Msgs/NETWORK
        (trap! EventError "Unexpected network event.")

        Msgs/SESSION
        (onSessionMsg this evt stateRef)

        (log/warn "game engine: unhandled msg " evt)))

    (restart [this options]
      (log/debug "restarting game one more time")
      (let [parr (:players @stateAtom)
            m (MapPlayers parr)]
        (dosync (ref-set stateRef m))
        (BCastAll (.container this)
                  Events/RESTART
                  (MapPlayersEx parr))))

    (ready [this room]
      (swap! stateAtom assoc :room room)
      (BCastAll (.container this)
                Events/START
                (MapPlayersEx (:players @stateAtom))))

    (startRound [this cmd]
      (-> (:arena @stateAtom)
          (StartPoint cmd)))

    (endRound [_ obj]
      );;(swap! stateAtom dissoc :arena))

    (container [_] (:room @stateAtom))

    (stop [_] )
    (finz [_] )

    (state [_] @stateAtom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

