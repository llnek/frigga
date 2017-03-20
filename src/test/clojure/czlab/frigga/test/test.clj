;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.frigga.test.test

  (:require [czlab.convoy.nettio.client :as cc])

  (:import [czlab.convoy.nettio WSClientConnect]
           [czlab.loki.net Events])

  (:use [czlab.wabbit.sys.core]
        [czlab.loki.net.core]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.io]
        [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _dburl_
  (str "jdbc:h2:"
       (fpath *tempfile-repo*) "/" (jid<>)
       ";MVCC=TRUE;AUTO_RECONNECT=TRUE"))
(def ^:private _conf_
  {:locale {:country "US" :lang "en"}
   :games
   {:tictactoe
    {:uuid  "bd5f79bbeb414ed5bb442529dc27ed3c"
     :layout  :portrait :height  480 :width  320
     :network {:minp 2 :maxp 2
               :impl :czlab.frigga.tttoe.core/tictactoe }}
    :pong
    {:uuid  "fa0860f976dc41358bc7bd5af3147d55"
     :layout :portrait :height  480 :width  320
     :network {:minp 2 :maxp 2
               :impl  :czlab.frigga.pong.core/pong } }}
   :info {:digest "some-digest"
          :main :czlab.frigga.sys.core/friggaMain
          :encoding "utf-8" }
   :rdbms {:default {:driver "org.h2.Driver"
                     :url _dburl_
                     :user "sa"
                     :passwd ""}}
   :plugins {:web
             {:$pluggable :czlab.wabbit.plugs.io.http/HTTP
              :handler :czlab.loki.sys.core/lokiHandler
              :host "localhost"
              :port 9090
              :wsockPath #{ "/loki/tictactoe" "/loki/pong"}
              :routes
              [{:uri "/loki/(.*)" }] } } })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private con2 (atom nil))
(def ^:private con1 (atom nil))
(def ^:private res2 (atom #{}))
(def ^:private res1 (atom #{}))
(def ^:private info2 (atom nil))
(def ^:private info1 (atom nil))
(def ^:private m2data [0 1 2])
(def ^:private m1data [6 7 8])
(def ^:private moves2 (atom m2data))
(def ^:private moves1 (atom m1data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- makeMove "" [con info moves body]
  (let [{:keys [pnum value color]} @info
        {:keys [grid]} body
        _ (assert (= pnum (:pnum body)))
        c (if-some [m (first @moves)]
            (do (reset! moves (drop 1 @moves)) m))]
    (when c
      (->> {:value value :cell c}
           (privateEvent<> Events/PLAY_MOVE )
           writeJsonStr
           (.write @con)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- concb [id con info moves res c m]
  (let [{:keys [type code body] :as msg}
        (readJsonStrKW (:text m))]
    (cond
      (= Events/PLAYREQ_OK code)
      (do
        ;;{:puid :pnum :game :room }
        (reset! info  body)
        (swap! res conj code))

      (= Events/RESTART code)
      (let [{:keys [puid pnum game room]} @info
            b (get body (keyword puid))]
        ;;pick the extra stuff { :session_number :value :color }
        (swap! info merge b)
        (swap! res conj code))

      (= Events/START code)
      (let [{:keys [puid pnum game room]} @info
            b (get body (keyword puid))]
        ;;pick the extra stuff { :session_number :value :color }
        (swap! info merge b)
        (swap! res conj code))

      (= Events/STOP code)
      (let [s (:status body)]
        (when (or (= s 2)(= s 1))
          (prn!! "CB>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: WINNER= %d" s)
          (swap! res conj Events/GAME_WON)))

      (= Events/POKE_WAIT code)
      (do
        (swap! res conj code))

      (= Events/POKE_MOVE code)
      (do
        (makeMove con info moves body)
        (swap! res conj code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsconn<> "" [cb]
  (cc/wsconnect<> "localhost" 9090 "/loki/tictactoe" cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- req-game "" [^WSClientConnect c user pwd]
  (->> {:type 3 :code 600
        :body {:gameid "bd5f79bbeb414ed5bb442529dc27ed3c"
               :principal user
               :credential pwd}}
       writeJsonStr
       (.write c )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest tcase1

  (is (do->true
        (startViaConfig *tempfile-repo* _conf_)
        (pause 1000)))

  (is (let [h (wsconn<> (partial concb "1" con1 info1 moves1 res1))
            ^WSClientConnect c (deref h 5000 nil)]
        (if (some? c)
          (do->true (reset! con1 c)))))

  (is (let [h (wsconn<> (partial concb "2" con2 info2 moves2 res2))
            ^WSClientConnect c (deref h 5000 nil)]
        (if (some? c)
          (do->true (reset! con2 c)))))

  (is (let []
        (req-game @con2 "mary" "secret")
        (req-game @con1 "joe" "secret")
        (pause 1500)
        (and (contains? @res1 Events/PLAYREQ_OK)
             (contains? @res1 Events/START)
             (contains? @res2 Events/PLAYREQ_OK)
             (contains? @res2 Events/START))))

  (is (let []
        (.write @con2 (writeJsonStr
                        (privateEvent<> Events/STARTED {})))
        (.write @con1 (writeJsonStr
                        (privateEvent<> Events/STARTED {})))
        (pause 1500)
        (and (or (contains? @res1 Events/POKE_MOVE)
                 (contains? @res1 Events/POKE_WAIT))
             (or (contains? @res2 Events/POKE_MOVE)
                 (contains? @res2 Events/POKE_WAIT)))))

  (is (let []
        (pause 2000)
        (and (or (contains? @res1 Events/GAME_WON)
             (or (contains? @res2 Events/GAME_WON))))))

  (is (let []
        (swap! res2 disj Events/GAME_WON Events/POKE_WAIT Events/POKE_MOVE)
        (swap! res1 disj Events/GAME_WON Events/POKE_WAIT Events/POKE_MOVE)
        (reset! moves2 m2data)
        (reset! moves1 m1data)
        (.write @con1 (writeJsonStr
                        (privateEvent<> Events/REPLAY {})))
        (pause 1500)
        (and (contains? @res1 Events/RESTART)
             (contains? @res2 Events/RESTART))))

  (is (let []
        (.write @con2 (writeJsonStr
                        (privateEvent<> Events/STARTED {})))
        (.write @con1 (writeJsonStr
                        (privateEvent<> Events/STARTED {})))
        (pause 1500)
        (and (or (contains? @res1 Events/POKE_MOVE)
                 (contains? @res1 Events/POKE_WAIT))
             (or (contains? @res2 Events/POKE_MOVE)
                 (contains? @res2 Events/POKE_WAIT)))))

  (is (let []
        (pause 2000)
        (and (or (contains? @res1 Events/GAME_WON)
                 (contains? @res2 Events/GAME_WON)))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


