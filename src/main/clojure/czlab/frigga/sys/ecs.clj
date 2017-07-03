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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pre-populate a bunch of objects in the pool
(defn presetPoolNodes "" [pool func cnt]
  (swap! pool merge {:batch cnt :ctor func})
  (->> (c/preduce<vec>
         #(let [rc (func) _ %2]
            (if rc (conj! %1 rc) %1)) (range cnt))
       (swap! pool update-in [:nodes] concat))
  pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;find an object by applying this filter
(defn selectFromPool [pool selector]
  (some #(if (and (:active? %1)
                  (selector %1)) %1) (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get a free object from the pool and set it's status to true
(defn takeFromPool ""
  ([pool] (takeFromPool pool nil))
  ([pool create?]
   (do-with [rc (getFromPool pool create?)]
     (if rc (p-take! rc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get a free object from the pool.  More like a peek
(defn getFromPool ""
  ([pool] (getFromPool pool nil))
  ([pool create?]
   (let [e (some #(if-not (:active? %1) %1) (:nodes @pool))
        {:keys [ctor batch]} @pool]
    (if (some? e)
      e
      (when (and create? ctor)
        (presetPoolNodes pool ctor batch)
        (getFromPool me))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn checkinPool [pool c]
  (if c (swap! pool update-in [:nodes] conj c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn clearPool "" [pool]
  (swap! pool assoc :nodes []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getUsed "" [pool]
  (filterv #(:active? %1) (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get the count of active objects
(defn countUsed "" [pool]
  (reduce
    #(if (:active? %2) (inc %1) %1) 0 (:nodes @pool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn foreachInPool [pool func]
  (doseq [n (:nodes @pool)] (func n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn someInPool? [me func]
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
(defn getEnodes "" [ecsdb & comids]
  (let
    [{:keys [ccache enodes]} @ecsdb
     [ccs pmin missed]
     (loop [[cid & more] comids
            pmin nil ccs [] missed 0]
       (let [c (get ccache cid)]
         (if (nil? cid)
           [ccs pmin missed]
           (recur more
                  (cond
                    (nil? pmin) c
                    (nil? c) pmin
                    (< (.size c)
                       (.size pmin)) c
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
(defn newEnode ""
  ([ecsdb name] (newEnode ecsdb name nil))
  ([ecsdb name take?]
   (let [eid (genEID)]
     (do-with [e (c/object<> ENodeObj
                              {:name name :id eid})]
        (swap! ecsdb update-in [:enodes] assoc eid e)
        (if take? (p-take! e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getComCache "" [ecsdb] (:ccache @ecsdb))
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

