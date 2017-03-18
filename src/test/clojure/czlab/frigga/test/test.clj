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

  (:import [czlab.convoy.nettio WSClientConnect])

  (:use [czlab.wabbit.sys.core]
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
               :arena :czlab.frigga.tttoe.core/tictactoe }}
    :pong
    {:uuid  "fa0860f976dc41358bc7bd5af3147d55"
     :layout :portrait :height  480 :width  320
     :network {:minp 2 :maxp 2
               :arena  :czlab.frigga.pong.core/pong } }}
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
(def ^:private res2 (atom nil))
(def ^:private res1 (atom nil))

(defn- con1cb [a b] )



(defn- wsconn "" [cb]
  (cc/wsconnect<> "localhost" 9090 "/loki/tictactoe" cb))

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

  (is (let [h (wsconn<> con1cb)
            ^WSClientConnect c (deref h 5000 nil)]
        (if (some? c)
          (do->true (reset! con1 c)))))

  (is (let [h (wsconn<> con2cb)
            ^WSClientConnect c (deref h 5000 nil)]
        (if (some? c)
          (do->true (reset! con2 c)))))

  (is (let []
        (req-game @con1 "joe" "secret")
        (pause 1000)))




  (pause 3000)

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


