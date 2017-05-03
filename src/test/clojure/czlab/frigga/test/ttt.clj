;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.frigga.test.ttt

  (:require [czlab.loki.xpis :as loki :refer :all]
            [czlab.nettio.client :as cc]
            [czlab.basal.logging :as log])

  (:use [czlab.loki.net.core]
        [czlab.wabbit.xpis]
        [czlab.wabbit.core]
        [czlab.basal.format]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.io]
        [clojure.test])

  (:import [czlab.nettio WSClientConnect]
           [czlab.jasal XData]))

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
             {:$pluggable :czlab.wabbit.plugs.http/HTTP
              :handler :czlab.loki.core/lokiHandler
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
(def ^:private m2draw [0 1 5 6 8])
(def ^:private m1draw [2 3 4 7 8])
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
           (privateEvent<> ::loki/play-move)
           encodeEvent
           (.write ^WSClientConnect @con)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- concb [id con info moves res c m]
  (let [{:keys [type code body] :as msg}
        (decodeEvent (.strit ^XData (:body m)))]
    (log/debug "[JSON-STR]\n%s\n" (prettyEvent msg))
    (swap! res conj code)
    (cond
      (= ::loki/playreq-ok code)
      (do
        ;;{:puid :pnum :game :room }
        (reset! info  body))

      (= ::loki/restart code)
      (let [{:keys [puid pnum game room]} @info
            b (get body (keyword puid))]
        ;;pick the extra stuff { :session_number :value :color }
        (swap! info merge b))

      (= ::loki/start code)
      (let [{:keys [puid pnum game room]} @info
            b (get body (keyword puid))]
        ;;pick the extra stuff { :session_number :value :color }
        (swap! info merge b))

      (= ::loki/game-won code)
      (let [s (:status body)]
        (log/debug "CB>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: WINNER= %d" s))

      (= ::loki/game-tie code)
      (let [s (:status body)]
        (log/debug "CB>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: WINNER= %d" s))

      (= ::loki/play-scrubbed code)
      (let [s (:pnum body)]
        (log/debug "CB>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: QUITTER= %d" s))

      (= ::loki/poke-wait code)
      nil

      (= ::loki/poke-move code)
      (if-not (contains? @res ::loki/quit)
        (makeMove con info moves body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsconn<> "" [cb]
  (cc/wsconnect<> "localhost" 9090 "/loki/tictactoe" cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- req-game "" [^WSClientConnect c user pwd]
  (->> {:type ::loki/private
        :code ::loki/playgame-req
        :body {:gameid "bd5f79bbeb414ed5bb442529dc27ed3c"
               :principal user
               :credential pwd}}
       encodeEvent
       (.write c )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest ^:tictactoe test-tictactoe

  (is (do->true
        (startViaConfig *tempfile-repo* _conf_)
        (pause 1000)))

  (testing "related to: win game"
    (is (let [h (wsconn<> (partial concb "1" con1 info1 moves1 res1))
              ^WSClientConnect c (deref h 5000 nil)]
          (if (some? c)
            (do->true (reset! con1 c)))))

    (is (let [h (wsconn<> (partial concb "2" con2 info2 moves2 res2))
              ^WSClientConnect c (deref h 5000 nil)]
          (if (some? c)
            (do->true (reset! con2 c)))))

    (is (let []
          (req-game @con1 "paul" "secret")
          (req-game @con2 "mary" "secret")
          (pause 1500)
          (and (contains? @res1 ::loki/playreq-ok)
               (contains? @res1 ::loki/start)
               (contains? @res2 ::loki/playreq-ok)
               (contains? @res2 ::loki/start))))

    (is (let []
          (.write ^WSClientConnect @con2
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (pause 1500)
          (and (or (contains? @res1 ::loki/poke-move)
                   (contains? @res1 ::loki/poke-wait))
               (or (contains? @res2 ::loki/poke-move)
                   (contains? @res2 ::loki/poke-wait)))))

    (is (let []
          (pause 2000)
          (and (or (contains? @res1 ::loki/game-won)
               (or (contains? @res2 ::loki/game-won)))))))

  (testing "related to: replay game"
    (is (let []
          (reset! res2 #{})
          (reset! res1 #{})
          (reset! moves2 m2data)
          (reset! moves1 m1data)
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/replay {})))
          (pause 1500)
          (and (contains? @res1 ::loki/restart)
               (contains? @res2 ::loki/restart))))

    (is (let []
          (.write ^WSClientConnect @con2
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (pause 1500)
          (and (or (contains? @res1 ::loki/poke-move)
                   (contains? @res1 ::loki/poke-wait))
               (or (contains? @res2 ::loki/poke-move)
                   (contains? @res2 ::loki/poke-wait)))))

    (is (let []
          (pause 2000)
          (and (or (contains? @res1 ::loki/game-won)
                   (contains? @res2 ::loki/game-won))))))

  (testing "related to: draw! game"
    (is (let []
          (reset! res2 #{})
          (reset! res1 #{})
          (reset! moves2 m2draw)
          (reset! moves1 m1draw)
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/replay {})))
          (pause 1500)
          (and (contains? @res1 ::loki/restart)
               (contains? @res2 ::loki/restart))))

    (is (let []
          (.write ^WSClientConnect @con2
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (pause 1500)
          (and (or (contains? @res1 ::loki/poke-move)
                   (contains? @res1 ::loki/poke-wait))
               (or (contains? @res2 ::loki/poke-move)
                   (contains? @res2 ::loki/poke-wait)))))

    (is (let []
          (pause 2000)
          (and (or (contains? @res1 ::loki/game-tie)
                   (contains? @res2 ::loki/game-tie))))))

  (testing "related to: quit! game"
    (is (let []
          (reset! res2 #{})
          (reset! res1 #{})
          (reset! moves2 m2draw)
          (reset! moves1 m1draw)
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/replay {})))
          (pause 1500)
          (and (contains? @res1 ::loki/restart)
               (contains? @res2 ::loki/restart))))

    (is (let []
          (swap! res2 conj ::loki/quit)
          (swap! res1 conj ::loki/quit)
          (.write ^WSClientConnect @con2
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (.write ^WSClientConnect @con1
                  (encodeEvent (privateEvent<> ::loki/started {})))
          (pause 1000)
          (.write ^WSClientConnect @con2
                  (encodeEvent (privateEvent<> ::loki/quit {})))
          (pause 1500)
          (and (or (contains? @res1 ::loki/play-scrubbed)
                   (contains? @res2 ::loki/play-scrubbed))))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


