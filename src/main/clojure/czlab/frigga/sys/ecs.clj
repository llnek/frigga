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
(defmacro ^:private genEID "" [] `(keyword (c/seqint2)))
(defn- concatv [x y] (into [] (concat x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pre-populate a bunch of objects in the pool
(defn- presetPool "" [pool]
  (let [{:keys [ctor batch]} @pool]
    (->> (c/preduce<vec>
           #(let [rc (ctor) _ %2]
              (if rc (conj! %1 rc) %1)) (range batch))
         (swap! pool update-in [:nodes] concatv))
    pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- p-yield! "" [pobj] (c/setf! pobj :active? false) pobj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- p-take! "" [pobj] (c/setf! pobj :active? true) pobj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn objPool<> "" [func cnt]
  (presetPool (atom {:batch (or cnt 10)
                     :ctor func
                     :nodes []})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn enode<> ""
  ([ecsdb name] (enode<> ecsdb name nil))
  ([ecsdb name take?]
   (let [eid (genEID)
         e (atom {:active? false
                  :stale? false
                  :id eid
                  :name name})]
     (swap! ecsdb update-in [:enodes] assoc eid e)
     (if take? (p-take! e))
     e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ecsdb<> ""
  ([] (ecsdb<> nil))
  ([cfg]
   (atom {:config cfg
          :systems #{}
          :enodes {}
          :ccache {}
          :sorted-systems []})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;find an object by applying this filter
(defn selectFromPool [pool selector]
  (some #(if (selector %1) %1) (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get a free object from the pool.  More like a peek
(defn getFromPool ""
  ([pool] (getFromPool pool nil))
  ([pool create?]
   (let [{:keys [nodes ctor batch]} @pool
         e (some #(if-not (:active? %1) %1) nodes)]
    (if (some? e)
      e
      (if create?
        (-> (presetPool pool) getFromPool ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get a free object from the pool and set it's status to true
(defn takeFromPool ""
  ([pool] (takeFromPool pool nil))
  ([pool create?]
   (c/do-with [rc (getFromPool pool create?)]
     (if rc (p-take! rc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bindComponentToNode [node co]
  (let [{:keys [ecs]} @node
        {:keys [ccache]} @ecs
        eid (:id node)
        z (:typeId co)
        h (get @ccache z)]
    ;;a node cannot have many components of the same type
    (assert (nil? (get h eid)))
    (c/setf! co :node node)
    (swap! ccache update-in [z] assoc eid co)
    node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn clearPool "" [pool]
  (swap! pool assoc :nodes []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getUsedFromPool "" [pool]
  (filterv #(:active? %1) (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get the count of active objects
(defn countUsedInPool "" [pool]
  (reduce
    #(if (:active? %2) (inc %1) %1) 0 (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn foreachInPool [pool func]
  (doseq [n (:nodes @pool)] (func n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn someInPool? [pool func]
  (some? (some #(if (func %1) %1) (:nodes @pool))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;hibernate (status off) all objects in the pool
(defn resetPool "" [pool]
  (doseq [n (:nodes @pool)] (p-yield! n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sortSystems "" [arr]
  (c/sortby :priority
            (fn [^long t1 ^long t2]
              (.compareTo (Long/valueOf t1) t2)) arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getNodesFromECS "" [ecsdb & comids]
  (let
    [{:keys [ccache enodes]} @ecsdb
     cc @ccache
     [ccs pmin missed]
     (loop [[cid & more] comids
            pmin nil ccs [] missed 0]
       (let [c (get cc cid)]
         (if (nil? cid)
           [ccs pmin missed]
           (recur more
                  (cond
                    (nil? pmin) c
                    (nil? c) pmin
                    (< (count c)
                       (count pmin)) c
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getConfig "" [ecsdb] (:config @ecsdb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn removeSystem "" [ecsdb s]
  (let [m (disj (:systems @ecsdb) s)
        a (sortSystems m)]
    (swap! ecsdb merge {:systems m :sorted-systems a})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn removeSystems "" [ecsdb]
  (swap! ecsdb merge {:systems #{} :sorted-systems []}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn removeEnode "" [ecsdb node]
  (swap! ecsdb update-in [:enodes] dissoc (:id node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn removeEnodes "" [ecsdb]
  (swap! ecsdb assoc :enodes {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn registerSystems "" [ecsdb & ss]
  (swap! ecsdb
         merge {:systems (into #{} ss)
                :sorted-systems (sortSystems ss)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn igniteEngine! "" [ecsdb func])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

