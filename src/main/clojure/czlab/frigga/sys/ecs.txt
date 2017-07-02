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

(defprotocol Engine
  ""
  (doHouseKeeping [_] "")
  (generateEid [_] "")
  (initEntities [_] "")
  (initSystems [_] "")
  (getNodes [_ comTypes] "")
  (getNodes [_ comType] "")
  (getNodes [_] "")
  (reifyNode [_ name take?] "")
  (getCfg [_] "")
  (purgeSystem  [_ s] "")
  (purgeSystems [_] "")
  (purgeNode [_ ent] "")
  (purgeNodes [_] "")
  (regoSystem [_ s] "")
  (ignite [_] "")
  (update [_ dt] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-atomic EngineObject
  Engine
  (getNodes [me] (vals (:enodes @me)))
  (getNodes [me comType]
            (.getNodes me [comType]))
  (getNodes [me comTypes]
    ;;find shortest cache, doing an intersection
    (let
      [{:keys [ccache enodes]} @me
       [ccs pmin missed]
       (loop [[cid & more] comTypes
              pmin nil ccs [] missed 0]
         (let [c (get ccache cid)]
           (if (nil? cid)
             [ccs pmin missed]
             (recur more
                    (cond
                      (nil? pmin) c
                      (nil? c) pmin
                      (< (.size c) (.size pmin)) c
                      :else pmin)
                    (if c (conj ccs c) ccs)
                    (if c missed (inc missed))))))]
      ;;use the shortest cache as the baseline
      (when-not (or (pos? missed)
                    (empty? ccs))
        ;; look for intersection
        (c/preduce<vec>
          (fn [acc [eid _]]
            (let [sum
                  (loop [[c & more] ccs sum 0]
                    (if (nil? c)
                      sum
                      (recur more
                             (if (c/in? c eid)
                               (inc sum) sum))))]
              ;; if found in all caches...
              (if (= sum (count ccs))
                (conj! acc (get enodes eid))
                acc)))
          pmin))))
  (reifyNode [me n take?]
    (let [eid (keyword (c/seqint2))
          e (c/object<> me n eid)]
      (swap! me
             update-in [enodes] assoc eid e)
      (if take? (p-take! e))
      e))
  (purgeNode [me e]
    ;; cannot purge twice!
    (assert (p-ok? e))
    (p-die! e)
    (swap! me
           update-in [garbo] conj e)
    (swap! me
           update-in [enodes] dissoc (:id e)))
  (purgeNodes [me]
    (doseq [[_ e] (:enodes @me)] (p-finz e))
    (swap! me assoc enodes {})
    (doHouseKeeping))
  (regoSystem [me s]
    (swap! me
           update-in [systems] conj s))
  (purgeSystem [me s]
    (swap! me
           update-in [systems] disj s))
  (purgeSystems [me]
    (swap! me
           assoc :systems #{}))
  (update [me dt]
    (swap! me
           assoc :updating? true)
    (loop [[s & more] (:systems @me)]
      (if (and (:active? s)
               (not (update s dt)))
        nil
        (recur more)))
    (doHouseKeeping)
    (swap! me assoc :updating? false))
  (ignite [me]
    (initEntities)
    (initSystems)
    (doseq [s (:systems @me)]
      (preamble s)))
  (doHouseKeeping [me]
    (doseq [e (:garbo @me)]
      (finz e))
    (swap! me assoc :garbo #{})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol Poolable
  ""
  (p-yield [_] "")
  (p-take [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol EntityNode
  ""
  (e-die [_] "")
  (e-checkin [_ c] "")
  (e-purge [_ comType] "")
  (e-get [_ comType] "")
  (e-has? [_ comType] "")
  (e-ok? [_] "")
  (e-getAll [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol COMRegistry
  ""
  (get-components [_ componentType] "")
  (bind-component [_ component entity] "")
  (unbind-component [_ component entity] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
Node::Node(not_null<Engine*> e, const sstr &n, NodeId eid) {
  this->_engine= e;
  this->_eid= eid;
  this->_name=n;
}

//////////////////////////////////////////////////////////////////////////////
//
Node::~Node() {
  //printf("Node dtor\n");
  F__LOOP(it, _parts) {
    auto c= it->second;
    _engine->rego()->unbind(c,this);
    // only delete if the object is not a reference-counted
    // type
    if (!c->isAuto())
    delete c;
  }
}

(c/decl-object NodeObject
  EntNode
  (checkin [me com]
    (let [z (:typeId com)]
      (assert (not (has me z)))
      (bind-comobj (get-in me [:engine :rego]) com me)
      (setNode com me)))
  (purge [me comType]
    (unbind (get-in me [:engine :rego]) comType me))
  (get [me comType]
    (get (get-in me [:engine :rego]) comType me))
  (getAll [me]
    (getAll (get-in me [:engine :rego]) me))
  (has? [me comType]
    (get (get-in me [:engine :rego]) comType me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-atomic COMRegistryObject
  COMRegistry
  (get-components [me comType]
    (get @me comType))
  (unbind-component [me com ent]
    (let [eid (:id ent)
          cid (:typeId com)]
      (swap! me
             update-in [cid] dissoc eid)))
  (bind-component [me com ent]
    (let [cid (:typeId com)
          eid (:id ent)]
      (swap! me
             update-in [cid] assoc eid com))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

