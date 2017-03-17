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

  (:use [czlab.wabbit.sys.core]
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
   :info {:digest "769a36bc3e2c4f1587ec0de7d6e6ba19"
          :encoding "utf-8" }
   :rdbms {:default {:driver "org.h2.Driver"
                     :url _dburl_
                     :user "sa"
                     :passwd ""}}
   :plugins {:web
             {:$pluggable :czlab.wabbit.plugs.io.http/HTTP
              :port 9090
              :wsockPath #{ "/loki/ttt" "/loki/pong"}
              :routes
              [{:handler "czlab.loki.sys.core/lokiHandler"
                :uri "/loki/(.*)" }] } } })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest tcase1

  (is (do->true
        (startViaConfig *tempfile-repo* _conf_)))

  (pause 5000)
  (is (= 1 1))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


