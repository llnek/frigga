;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.frigga.sys.ecs

  (:require [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol Poolable
  ""
  (p-yield [_] "")
  (p-take! [_] "")
  (p-status [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol ENode
  (checkin [_ com] "")
  (purge [_ ct] "")
  (get-comobj [_ ct] "")
  (has-comobj? [_ ct] "")
  (is-ok? [_] "")
  (get-comobjs [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol System
  ""
  (update [_ dt] "")
  (preamble [_] "")
  (suspend [_] "")
  (restart [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-object SystemObj
  System
  (update [_ dt] )
  (preamble [_] )
  (suspend [_] )
  (restart [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-atomic ENodeObj
  Poolable
  (p-yield [_] "")
  (p-take! [_] "")
  (p-status [_] "")
  ENode
  (checkin [me com] )
  (purge [me ct] )
  (get-comobj [me ct] )
  (has-comobj? [me ct] )
  (is-ok? [me] )
  (get-comobjs [me]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol Engine
  ""
  (get-enodes [_ comids] "")
  (new-enode [_ name take?] "")
  (get-cfg [_] "")
  (get-com-cache [_] "")
  (remove-system [_ s] "")
  (remove-systems [_] "")
  (remove-enode [_ node] "")
  (remove-enodes [_] "")
  (register-system [_ s] "")
  (ignite [_] "")
  (update [_ dt] "")
  (house-keeping [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-atomic EngineObj
  Engine
  (get-enodes [me comids])
  (new-enode [me name take?] )
  (get-com-cache [me] (:ccache @me))
  (get-cfg [me] (:config @me))
  (remove-system [me s])
  (remove-systems [me])
  (remove-enode [me node])
  (remove-enodes [me])
  (register-system [me s])
  (ignite [me])
  (update [me dt])
  (house-keeping [me]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

