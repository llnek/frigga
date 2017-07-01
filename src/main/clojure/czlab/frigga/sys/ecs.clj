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
(c/decl-object EngineObject
  Engine
  (getNodes [me comTypes]
    (let [pmin Integer/MAX_VALUE
          missed false
          pm nil]
      ;;find shortest cache, doing an intersection
  F__LOOP(it,cs) {
    auto &cid= *it;
    auto c= _types->getCache(cid);
    if (E_NIL(c)) {
      //CCLOG("cache missed when looking for intersection on %s", cid.c_str());
      missed=true;
      break;
    }
    if (c->size() < pmin) {
      pmin= c->size();
      pm=c;
    }
    ccs.push_back(c);
  }

  if (missed) {
  return; }

  //CCLOG("intesection on %d caches", (int)ccs.size());

  if (ccs.size() > 0) {
    //use the shortest cache as the baseline
    F__POOP(it,pm) {
      auto eid= it->first;
      auto sum=0;

      // look for intersection
      F__LOOP(it2,ccs) {
        auto c= *it2;
        if (c==pm) { ++sum; continue;}
        auto it3= c->find(eid);
        if (it3 != c->end()) {
          ++sum;
        }
      }

      // if found in all caches...
      if (sum == ccs.size()) {
        // all matched
        auto it4= _ents.find(eid);
        if (it4 != _ents.end()) {
          rc.push_back(it4->second);
        }
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
//
const s_vec<Node*> Engine::getNodes(const s_vec<COMType> &cs) {
  s_vec<Node*> rc;
  getNodes(cs, rc);
  return rc;
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::getNodes(const COMType &c, s_vec<Node*> &rc) {
  auto cc= _types->getCache(c);
  if (cc) F__POOP(it,cc) {
    auto z= it->first;
    auto it2= _ents.find(z);
    if (it2 != _ents.end()) {
      rc.push_back(it2->second);
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
//
const s_vec<Node*> Engine::getNodes(const COMType &c) {
  s_vec<Node*> rc;
  getNodes(c,rc);
  return rc;
}


//////////////////////////////////////////////////////////////////////////////
//
void Engine::getNodes(s_vec<Node*> &rc) {
  F__LOOP(it, _ents) {
    rc.push_back(it->second);
  }
}

//////////////////////////////////////////////////////////////////////////////
//
const s_vec<Node*> Engine::getNodes() {
  s_vec<Node*> rc;
  getNodes(rc);
  return rc;
}

//////////////////////////////////////////////////////////////////////////////
//
NodeId Engine::generateEid() {
  auto rc= ++_lastId;
  if (rc < INT_MAX) {} else {
    throw "too many entities";
  }
  return rc;
}

//////////////////////////////////////////////////////////////////////////////
//
Node* Engine::reifyNode(const sstr &n, bool take) {
  auto eid= this->generateEid();
  auto e= mc_new3(Node, this, n, eid);
  _ents.insert(S__PAIR(NodeId,Node*,eid,e));
  if (take) {e->take();}
  return e;
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::purgeNode(not_null<Node*> e) {
  // cannot purge twice!
  assert(e->isOk());
  e->die();
  _garbo.push_back(e);

  auto it= _ents.find(e->getEid());
  if (it != _ents.end()) {
    _ents.erase(it);
  }
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::purgeNodes() {
  F__LOOP(it, _ents) {
    delete it->second;
  }
  _ents.clear();
  doHouseKeeping();
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::regoSystem(not_null<System*> s) {
  _systemList.add(s);
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::purgeSystem(not_null<System*> s ) {
  _systemList.purge(s);
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::purgeSystems() {
  _systemList.clear();
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::update(float time) {
  _updating = true;
  for (auto s= _systemList._head; N_NIL(s); s= s->_next) {
    if (s->isActive()) {
      if (! s->update(time)) { break; }
    }
  }
  doHouseKeeping();
  _updating = false;
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::ignite() {
  initEntities();
  initSystems();
  for (auto s= _systemList._head; N_NIL(s); s=s->_next) {
    s->preamble();
  }
}

//////////////////////////////////////////////////////////////////////////////
//
void Engine::doHouseKeeping() {
  F__LOOP(it, _garbo) {
    delete *it;
  }
  _garbo.clear();
}




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

