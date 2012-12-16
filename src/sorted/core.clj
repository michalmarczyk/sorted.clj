(ns sorted.core
  (:refer-clojure :exclude [sorted-set sorted-set-by
                            sorted-map sorted-map-by])
  (:import (clojure.lang RT Util APersistentMap Box MapEntry SeqIterator
                         IPersistentMap IPersistentSet IPersistentStack IFn)
           (java.util Comparator Collections ArrayList)))

(gen-interface
 :name sorted.core.INode
 :methods
 [[isRed   [] boolean]
  [redden  [] sorted.core.INode]
  [blacken [] sorted.core.INode]

  [left    [] sorted.core.INode]
  [right   [] sorted.core.INode]
  [key     [] Object]
  [val     [] Object]

  [addLeft      [sorted.core.INode] sorted.core.INode]
  [addRight     [sorted.core.INode] sorted.core.INode]
  [removeLeft   [sorted.core.INode] sorted.core.INode]
  [removeRight  [sorted.core.INode] sorted.core.INode]
  [replace [Object Object sorted.core.INode sorted.core.INode] sorted.core.INode]
  [balanceLeft  [sorted.core.INode] sorted.core.INode]
  [balanceRight [sorted.core.INode] sorted.core.INode]

  [kvreduce [clojure.lang.IFn Object] Object]])

(import sorted.core.INode)

(defmacro ^:private caching-hash
  [coll hash-fn hash-key]
  `(let [h# ~hash-key]
     (if-not (== h# (int -1))
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key (int h#))
         h#))))

(defn ^:private equiv-sequential
  "Assumes x is sequential. Returns true if x equals y, otherwise
  returns false."
  [x y]
  (boolean
   (when (sequential? y)
     (loop [xs (seq x) ys (seq y)]
       (cond (nil? xs) (nil? ys)
             (nil? ys) false
             (= (first xs) (first ys)) (recur (next xs) (next ys))
             :else false)))))

(defn ^:private tree-map-seq-push
  [^INode node stack ascending?]
  (loop [t node stack stack]
    (if-not (nil? t)
      (recur (if ascending? (.left t) (.right t))
             (conj stack t))
      stack)))

(defn ^:private hash-seq
  [s]
  (loop [h (int 1) s (seq s)]
    (if s
      (recur (unchecked-add-int (unchecked-multiply-int (int 31) h)
                                (if (nil? (first s))
                                  (int 0)
                                  (.hashCode ^Object (first s))))
             (next s))
      h)))

(deftype PersistentTreeMapSeq [^IPersistentMap _meta
                               ^IPersistentStack stack
                               ^boolean ascending?
                               ^int cnt
                               ^:unsynchronized-mutable ^int __hash
                               ^:unsynchronized-mutable ^int __hasheq]
  :no-print true

  Object
  (toString [this]
    (RT/printString this))

  (hashCode [coll]
    (caching-hash coll hash-seq __hash))

  clojure.lang.IHashEq
  (hasheq [this]
    (if (== __hasheq (int -1))
      (loop [h 1 s (seq this)]
        (if s
          (recur (unchecked-add-int (unchecked-multiply-int (int 31) h)
                                    (Util/hasheq (first s)))
                 (next s))
          (do (set! __hasheq (int h))
              h)))
      __hasheq))

  clojure.lang.Seqable
  (seq [this]
    this)

  clojure.lang.Sequential
  clojure.lang.ISeq
  (first [this]
    (let [t ^INode (peek stack)]
      (MapEntry. (.key t) (.val t))))

  (more [this]
    (let [t ^INode (first stack)
          next-stack (tree-map-seq-push (if ascending? (.right t) (.left t))
                                        (next stack)
                                        ascending?)]
      (if-not (nil? next-stack)
        (PersistentTreeMapSeq. nil
                               next-stack
                               ascending?
                               (unchecked-dec-int cnt)
                               -1
                               -1)
        ())))

  (next [this]
    (seq (.more this)))

  clojure.lang.Counted
  (count [coll]
    (if (neg? cnt)
      (unchecked-inc-int (count (next coll)))
      cnt))

  clojure.lang.IPersistentCollection
  (cons [coll o]
    (cons o coll))

  (equiv [coll other]
    (equiv-sequential coll other))

  (empty [coll]
    (with-meta () meta))

  clojure.lang.IMeta
  (meta [coll]
    _meta)

  clojure.lang.IObj
  (withMeta [coll meta]
    (PersistentTreeMapSeq. meta stack ascending? cnt __hash __hasheq))

  java.io.Serializable

  java.util.List
  (toArray [this]
    (RT/seqToArray (seq this)))

  (toArray [this a]
    (RT/seqToPassedArray (seq this) a))

  (add [this o]
    (throw (UnsupportedOperationException.)))

  (^boolean remove [this o]
    (throw (UnsupportedOperationException.)))

  (addAll [this c]
    (throw (UnsupportedOperationException.)))

  (clear [this]
    (throw (UnsupportedOperationException.)))

  (retainAll [this c]
    (throw (UnsupportedOperationException.)))

  (removeAll [this c]
    (throw (UnsupportedOperationException.)))

  (containsAll [this c]
    (every? #(.contains this %) (iterator-seq (.iterator c))))

  (size [this]
    (count this))

  (isEmpty [this]
    (nil? (seq this)))

  (contains [this o]
    (or (some #(Util/equiv % o) this) false))

  (iterator [this]
    (SeqIterator. this))

  (subList [this from to]
    (.subList (Collections/unmodifiableList (ArrayList. this)) from to))

  (set [this i e]
    (throw (UnsupportedOperationException.)))

  (remove [this ^int i]
    (throw (UnsupportedOperationException.)))

  (indexOf [this o]
    (loop [i (int 0) s (seq this)]
      (if s
        (if (Util/equiv (first s) o)
          i
          (recur (unchecked-inc-int i) (next s)))
        (int -1))))

  (lastIndexOf [this o]
    (.lastIndexOf (ArrayList. this) o))

  (listIterator [this]
    (.listIterator (Collections/unmodifiableList (ArrayList. this))))

  (listIterator [this i]
    (.listIterator (Collections/unmodifiableList (ArrayList. this)) i))

  (get [this i]
    (RT/nth this i))

  (add [this i e]
    (throw (UnsupportedOperationException.))))

(defmethod print-method ::PersistentTreeMapSeq [ptm-seq w]
  ((get (methods print-method) clojure.lang.ISeq) ptm-seq w))

(defn ^:private create-tree-map-seq
  [tree ascending? cnt]
  (PersistentTreeMapSeq.
   nil (tree-map-seq-push tree nil ascending?) ascending? cnt -1 -1))

(declare ^:private ->RedNode ^:private ->BlackNode)

(defn ^:private red?
  [^INode node]
  (boolean (if node (.isRed node))))

(defn ^:private black?
  [^INode node]
  (boolean (if node (not (.isRed node)))))

(defn ^:private balance-left
  [key val ^INode ins ^INode right]
  (if (red? ins)
    (cond
      (red? (.left ins))
      (->RedNode (.key ins) (.val ins)
                 (.blacken (.left ins))
                 (->BlackNode key val (.right ins) right)
                 nil)

      (red? (.right ins))
      (->RedNode (.. ins right key) (.. ins right val)
                 (->BlackNode (.key ins) (.val ins)
                              (.left ins)
                              (.. ins right left))
                 (->BlackNode key val
                              (.. ins right right)
                              right))

      :else
      (->BlackNode key val ins right))
    (->BlackNode key val ins right)))

(defn ^:private balance-right
  [key val ^INode left ^INode ins]
  (if (red? ins)
    (cond
      (red? (.right ins))
      (->RedNode (.key ins) (.val ins)
                (->BlackNode key val left (.left ins))
                (.blacken (.right ins)))

      (red? (.left ins))
      (->RedNode (.. ins left key) (.. ins left val)
                (->BlackNode key val left (.. ins left left))
                (->BlackNode (.key ins) (.val ins)
                            (.. ins left right)
                            (.right ins)))

      :else
      (->BlackNode key val left ins))
    (->BlackNode key val left ins)))

(defn ^:private balance-left-del
  [key val ^INode del ^INode right]
  (cond
    (red? del)
    (->RedNode key val (.blacken del) right)

    (black? right)
    (balance-right key val del (.redden right))

    (and (red? right) (black? (.left right)))
    (->RedNode (.. right left key) (.. right left val)
               (->BlackNode key val del (.. right left left))
               (balance-right (.key right) (.val right)
                              (.. right left right)
                              (.redden (.right right))))

    :else
    (throw (ex-info "red-black tree invariant violation" {}))))

(defn ^:private balance-right-del
  [key val ^INode left ^INode del]
  (cond
    (red? del)
    (->RedNode key val left (.blacken del))

    (black? left)
    (balance-left key val (.redden left) del)

    (and (red? left) (black? (.right left)))
    (->RedNode (.. left right key) (.. left right val)
              (balance-left (.key left) (.val left)
                            (.redden (.left left))
                            (.. left right left))
              (->BlackNode key val (.. left right right) del))

    :else
    (throw (ex-info "red-black tree invariant violation" {}))))

(defn ^:private tree-map-kv-reduce
  [^INode node f init]
  (let [init (if-not (nil? (.left node))
               (tree-map-kv-reduce (.left node) f init)
               init)]
    (if (reduced? init)
      @init
      (let [init (f init (.key node) (.val node))]
        (if (reduced? init)
          @init
          (let [init (if-not (nil? (.right node))
                       (tree-map-kv-reduce (.right node) f init)
                       init)]
            (if (reduced? init)
              @init
              init)))))))

(deftype BlackNode [key
                    val
                    ^INode left
                    ^INode right]
  INode
  (isRed [node]
    false)

  (key [node]
    key)

  (val [node]
    val)

  (left [node]
    left)

  (right [node]
    right)

  (addLeft [node ins]
    (.balanceLeft ins node))

  (addRight [node ins]
    (.balanceRight ins node))

  (removeLeft [node del]
    (balance-left-del key val del right))

  (removeRight [node del]
    (balance-right-del key val left del))

  (blacken [node]
    node)

  (redden [node]
    (->RedNode key val left right))

  (balanceLeft [node parent]
    (->BlackNode (.key parent) (.val parent) node (.right parent)))

  (balanceRight [node parent]
    (->BlackNode (.key parent) (.val parent) (.left parent) node))

  (replace [node key val left right]
    (->BlackNode key val left right))

  (kvreduce [node f init]
    (tree-map-kv-reduce node f init)))

(deftype RedNode [key
                  val
                  ^INode left
                  ^INode right]
  INode
  (isRed [node]
    true)

  (key [node]
    key)

  (val [node]
    val)

  (left [node]
    left)

  (right [node]
    right)

  (addLeft [node ins]
    (->RedNode key val ins right))

  (addRight [node ins]
    (->RedNode key val left ins))

  (removeLeft [node del]
    (->RedNode key val del right))

  (removeRight [node del]
    (->RedNode key val left del))

  (blacken [node]
    (->BlackNode key val left right))

  (redden [node]
    (throw (ex-info "red-black tree invariant violation" {})))

  (balanceLeft [node parent]
    (cond
      (red? left)
      (->RedNode key val
                 (.blacken left)
                 (->BlackNode (.key parent) (.val parent) right (.right parent)))

      (red? right)
      (->RedNode (.key right) (.val right)
                 (->BlackNode key val left (.left right))
                 (->BlackNode (.key parent) (.val parent)
                              (.right right)
                              (.right parent)))

      :else
      (->BlackNode (.key parent) (.val parent) node (.right parent))))

  (balanceRight [node parent]
    (cond
      (red? right)
      (->RedNode key val
                 (->BlackNode (.key parent) (.val parent)
                              (.left parent)
                              left)
                 (.blacken right))

      (red? left)
      (->RedNode (.key left) (.val left)
                 (->BlackNode (.key parent) (.val parent)
                              (.left parent)
                              (.left left))
                 (->BlackNode key val (.right left) right))

      :else
      (->BlackNode (.key parent) (.val parent) (.left parent) node)))

  (replace [node key val left right]
    (->RedNode key val left right))

  (kvreduce [node f init]
    (tree-map-kv-reduce node f init)))

(defn ^:private ^INode tree-map-add
  [^Comparator comp ^INode tree k v ^Box found]
  (if (nil? tree)
    (->RedNode k v nil nil)
    (let [c (.compare comp k (.key tree))]
      (cond
        (zero? c)
        (do (set! (.-val found) tree)
            nil)

        (neg? c)
        (let [ins (tree-map-add comp (.left tree) k v found)]
          (if-not (nil? ins)
            (.addLeft tree ins)))

        :else
        (let [ins (tree-map-add comp (.right tree) k v found)]
          (if-not (nil? ins)
            (.addRight tree ins)))))))

(defn ^:private ^INode tree-map-append
  [^INode left ^INode right]
  (cond
    (nil? left)
    right

    (nil? right)
    left

    (red? left)
    (if (red? right)
      (let [app (tree-map-append (.right left) (.left right))]
        (if (red? app)
          (->RedNode (.key app) (.val app)
                     (->RedNode (.key left) (.val left)
                                (.left left)
                                (.left app))
                     (->RedNode (.key right) (.val right)
                                (.right app)
                                (.right right)))
          (->RedNode (.key left) (.val left)
                     (.left left)
                     (->RedNode (.key right) (.val right) app (.right right)))))
      (->RedNode (.key left) (.val left)
                 (.left left)
                 (tree-map-append (.right left) right)))

    (red? right)
    (->RedNode (.key right) (.val right)
               (tree-map-append left (.left right))
               (.right right))

    :else
    (let [app (tree-map-append (.right left) (.left right))]
      (if (red? app)
        (->RedNode (.key app) (.val app)
                   (->BlackNode (.key left) (.val left)
                                (.left left)
                                (.left app))
                   (->BlackNode (.key right) (.val right)
                                (.right app)
                                (.right right)))
        (balance-left-del (.key left) (.val left)
                          (.left left)
                          (->BlackNode (.key right) (.val right)
                                       app
                                       (.right right)))))))

(defn ^:private ^INode tree-map-remove
  [^Comparator comp ^INode tree k ^Box found]
  (if-not (nil? tree)
    (let [c (.compare comp k (.key tree))]
      (cond
        (zero? c)
        (do (set! (.-val found) tree)
            (tree-map-append (.left tree) (.right tree)))

        (neg? c)
        (let [del (tree-map-remove comp (.left tree) k found)]
          (if (or (not (nil? del)) (not (nil? (.-val found))))
            (if (black? (.left tree))
              (balance-left-del (.key tree) (.val tree) del (.right tree))
              (->RedNode (.key tree) (.val tree) del (.right tree)))))

        :else
        (let [del (tree-map-remove comp (.right tree) k found)]
          (if (or (not (nil? del)) (not (nil? (.-val found))))
            (if (black? (.right tree))
              (balance-right-del (.key tree) (.val tree) (.left tree) del)
              (->RedNode (.key tree) (.val tree) (.left tree) del))))))))

(defn ^:private ^INode tree-map-replace
  [^Comparator comp ^INode tree k v]
  (let [tk (.key tree)
        c  (.compare comp k tk)]
    (cond (zero? c) (.replace tree tk v (.left tree) (.right tree))
          (neg? c)  (.replace tree tk (.val tree)
                              (tree-map-replace comp (.left tree) k v)
                              (.right tree))
          :else     (.replace tree tk (.val tree)
                              (.left tree)
                              (tree-map-replace comp (.right tree) k v)))))

(defn ^:private hash-imap
  [^IPersistentMap m]
  (APersistentMap/mapHash m))

(def ^:private never-equiv (Object.))

(defn ^:private equiv-map
  "Assumes y is a map. Returns true if x equals y, otherwise returns
  false."
  [x y]
  (boolean
    (when (map? y)
      ; assume all maps are counted
      (when (== (count x) (count y))
        (every? identity
                (map (fn [xkv] (= (get y (first xkv) never-equiv)
                                  (second xkv)))
                     x))))))

(declare ^:private empty-tree-map)

(deftype PersistentTreeMap [^Comparator comp
                            ^INode tree
                            ^int cnt
                            ^IPersistentMap _meta
                            ^:unsynchronized-mutable ^int __hash
                            ^:unsynchronized-mutable ^int __hasheq]
  Object
  (toString [this]
    (RT/printString this))

  (hashCode [coll]
    (caching-hash coll hash-imap __hash))

  (equals [this that]
    (APersistentMap/mapEquals this that))

  clojure.lang.IHashEq
  (hasheq [this]
    (if (== __hasheq (int -1))
      (let [h (APersistentMap/mapHasheq this)]
        (set! __hasheq h)
        h)
      __hasheq))

  clojure.lang.IMeta
  (meta [coll]
    _meta)

  clojure.lang.IObj
  (withMeta [coll meta]
    (PersistentTreeMap. comp tree cnt meta __hash __hasheq))

  clojure.lang.Counted
  (count [coll]
    cnt)

  clojure.lang.IPersistentCollection
  (cons [coll entry]
    (if (vector? entry)
      (assoc coll (nth entry 0) (nth entry 1))
      (reduce conj
              coll
              entry)))

  (empty [coll]
    (with-meta empty-tree-map meta))

  (equiv [coll other]
    (equiv-map coll other))

  clojure.core.protocols/IKVReduce
  (kv-reduce [coll f init]
    (if-not (nil? tree)
      (tree-map-kv-reduce tree f init)
      init))

  IFn
  (invoke [coll k]
    (.valAt coll k))

  (invoke [coll k not-found]
    (.valAt coll k not-found))

  clojure.lang.Seqable
  (seq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree true cnt)))

  clojure.lang.Reversible
  (rseq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree false cnt)))

  clojure.lang.ILookup
  (valAt [coll k]
    (.valAt coll k nil))

  (valAt [coll k not-found]
    (let [n (.entryAt coll k)]
      (if-not (nil? n)
        (.val n)
        not-found)))

  clojure.lang.Associative
  (assoc [coll k v]
    (let [found (Box. nil)
          t     (tree-map-add comp tree k v found)]
      (if (nil? t)
        (let [found-node ^INode (.-val found)]
          (if (= v (.val found-node))
            coll
            (PersistentTreeMap.
             comp (tree-map-replace comp tree k v) cnt _meta -1 -1)))
        (PersistentTreeMap.
         comp (.blacken t) (unchecked-inc-int cnt) _meta -1 -1))))

  (containsKey [coll k]
    (not (nil? (.entryAt coll k))))

  (entryAt [coll k]
    (loop [t tree]
      (if-not (nil? t)
        (let [c (.compare comp k (.key t))]
          (cond (zero? c) (MapEntry. (.key t) (.val t))
                (neg? c)  (recur (.left t))
                :else     (recur (.right t)))))))

  clojure.lang.MapEquivalence
  clojure.lang.IPersistentMap
  (without [coll k]
    (let [found (Box. nil)
          t     (tree-map-remove comp tree k found)]
      (if (nil? t)
        (if (nil? (.-val found))
          coll
          (PersistentTreeMap. comp nil 0 _meta -1 -1))
        (PersistentTreeMap.
         comp (.blacken t) (unchecked-dec-int cnt) _meta -1 -1))))

  (assocEx [coll k v]
    (let [found (Box. nil)
          t     (tree-map-add comp tree k v found)]
      (if (nil? t)
        (throw (ex-info "key already present" {}))
        (PersistentTreeMap.
         comp (.blacken t) (unchecked-inc-int cnt) _meta -1 -1))))

  clojure.lang.Sorted
  (seq [coll ascending?]
    (if (pos? cnt)
      (create-tree-map-seq tree ascending? cnt)))

  (seqFrom [coll k ascending?]
    (if (pos? cnt)
      (loop [stack nil t tree]
        (if-not (nil? t)
          (let [c (.compare comp k (.key t))]
            (cond
              (zero? c)  (PersistentTreeMapSeq.
                          nil (conj stack t) ascending? -1 -1 -1)
              ascending? (if (neg? c)
                           (recur (conj stack t) (.left t))
                           (recur stack          (.right t)))
              :else      (if (pos? c)
                           (recur (conj stack t) (.right t))
                           (recur stack          (.left t)))))
          (if-not (nil? stack)
            (PersistentTreeMapSeq. nil stack ascending? -1 -1 -1))))))

  (entryKey [coll entry]
    (key entry))

  (comparator [coll]
    comp)

  java.io.Serializable

  java.util.Map
  (clear [this]
    (throw (UnsupportedOperationException.)))

  (containsValue [this v]
    (.. this values (contains v)))

  (entrySet [this]
    (set (seq this)))

  (put [this k v]
    (throw (UnsupportedOperationException.)))

  (putAll [this m]
    (throw (UnsupportedOperationException.)))

  (remove [this k]
    (throw (UnsupportedOperationException.)))

  (size [this]
    cnt)

  (values [this]
    (vals this)))

(def ^:private empty-tree-map (PersistentTreeMap. compare nil 0 nil 0 0))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings."
  ([& keyvals]
     (loop [in (seq keyvals) out empty-tree-map]
       (if in
         (recur (nnext in) (assoc out (first in) (second in)))
         out))))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied comparator."
  ([^Comparator comparator & keyvals]
     (loop [in (seq keyvals)
            out (PersistentTreeMap. comparator nil 0 nil 0 0)]
       (if in
         (recur (nnext in) (assoc out (first in) (second in)))
         out))))

(defn ^:private hash-iset [^IPersistentSet s]
  ;; a la clojure.lang.APersistentSet
  (loop [h (int 0) s (seq s)]
    (if s
      (let [e (first s)]
        (recur (unchecked-add-int h (hash e))
               (next s)))
      h)))

(declare ^:private empty-tree-set)

(deftype PersistentTreeSet [^IPersistentMap _meta
                            ^PersistentTreeMap tree-map
                            ^:unsynchronized-mutable ^int __hash
                            ^:unsynchronized-mutable ^int __hasheq]
  Object
  (toString [this]
    (RT/printString this))

  (hashCode [coll]
    (caching-hash coll hash-iset __hash))

  clojure.lang.IHashEq
  (hasheq [this]
    (if (== __hasheq (int -1))
      (loop [h (int 0) s (seq this)]
        (if s
          (recur (unchecked-add-int h (Util/hasheq (first s)))
                 (next s))
          (do (set! __hasheq (int h))
              h))) 
      __hasheq))

  clojure.lang.IMeta
  (meta [coll]
    _meta)

  clojure.lang.IObj
  (withMeta [coll meta]
    (PersistentTreeSet. meta tree-map __hash __hasheq))

  clojure.lang.Counted
  (count [coll]
    (count tree-map))

  clojure.lang.IPersistentCollection
  (cons [coll o]
    (PersistentTreeSet. _meta (assoc tree-map o nil) -1 -1))

  (empty [coll]
    (with-meta empty-tree-set _meta))

  (equiv [coll other]
    (and
     (set? other)
     (== (count coll) (count other))
     (every? #(contains? coll %)
             other)))

  clojure.lang.Seqable
  (seq [coll]
    (keys tree-map))

  clojure.lang.Sorted
  (seq [coll ascending?]
    (RT/keys (.seq tree-map ascending?)))

  (seqFrom [coll k ascending?]
    (RT/keys (.seqFrom tree-map k ascending?)))

  (entryKey [coll entry]
    entry)

  (comparator [coll]
    (.comparator tree-map))

  clojure.lang.Reversible
  (rseq [coll]
    (map key (rseq tree-map)))

  clojure.lang.ILookup
  (valAt [coll v]
    (.valAt coll v nil))

  (valAt [coll v not-found]
    (let [n (.entryAt tree-map v)]
      (if-not (nil? n)
        (.key n)
        not-found)))

  clojure.lang.IPersistentSet
  (disjoin [coll v]
    (PersistentTreeSet. meta (dissoc tree-map v) -1 -1))

  (contains [coll k]
    (contains? tree-map k))

  (get [coll k]
    (get tree-map k))

  IFn
  (invoke [coll k]
    (.valAt coll k))

  (invoke [coll k not-found]
    (.valAt coll k not-found))

  java.io.Serializable

  java.util.Set
  (add [this o]
    (throw (UnsupportedOperationException.)))

  (remove [this o]
    (throw (UnsupportedOperationException.)))

  (addAll [this c]
    (throw (UnsupportedOperationException.)))

  (clear [this]
    (throw (UnsupportedOperationException.)))

  (retainAll [this c]
    (throw (UnsupportedOperationException.)))

  (removeAll [this c]
    (throw (UnsupportedOperationException.)))

  (containsAll [this c]
    (every? #(.contains this %) (iterator-seq (.iterator c))))

  (size [this]
    (count this))

  (isEmpty [this]
    (zero? (count this)))

  (iterator [this]
    (SeqIterator. (seq this)))

  (toArray [this]
    (RT/seqToArray (seq this)))

  (toArray [this a]
    (RT/seqToPassedArray (seq this) a)))

(def ^:private empty-tree-set (PersistentTreeSet. nil empty-tree-map 0 0))

(defn sorted-set
  "Returns a new sorted set with supplied keys."
  ([& keys]
     (reduce conj empty-tree-set keys)))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied comparator."
  ([^Comparator comparator & keys]
     (reduce conj
             (PersistentTreeSet. nil (sorted-map-by comparator) 0 0)
             keys)))

(doseq [v [#'->PersistentTreeMapSeq #'->PersistentTreeMap #'->PersistentTreeSet
           #'->RedNode #'->BlackNode]]
  (alter-meta! v assoc :private true))
