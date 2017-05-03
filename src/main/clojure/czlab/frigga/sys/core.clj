;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.frigga.sys.core

  (:require [czlab.basal.logging :as log]
            [clojure.java.io :as io])

  (:use [czlab.loki.game.core]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn friggaMain "" [exec]

  (-> (preduce<map>
        #(let [[k v] %2]
           (assoc! %1
                   (keyword (:uuid v))
                   (-> (dissoc v :uuid)
                       (assoc :uri (str "/" (name k))))))
        (:games (:conf @exec)))

      initGameRegistry!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


