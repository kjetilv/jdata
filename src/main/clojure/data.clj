(ns data
  (:gen-class
    :name data.ClojureBuildersProvider
    :implements [jdata.core.BuildersProvider])
  (:require [clojure.string :as str]))

(defmacro defn-memoized [fn-name args & body]
  `(def ~fn-name (memoize (fn ~args ~@body))))

(defn-memoized -built-class [builder-class]
  (first (. (first (. builder-class getGenericInterfaces)) getActualTypeArguments)))

(defn-memoized -method-attributes [clazz]
  (letfn [(bean-name [name]
            (letfn [(prefixed? [pref] (. name startsWith pref))]
              (cond
                (prefixed? "is") (. name substring 2)
                (prefixed? "get") (. name substring 3)
                (prefixed? "set") (. name substring 3)
                :else nil)))
          (normalize [name]
            (. java.beans.Introspector decapitalize name))]
    (let [methods (. clazz getDeclaredMethods)
          method-names (map #(. %1 getName) methods)
          accessed-names (remove nil? (map bean-name method-names))
          attribute-names (map normalize accessed-names)]
      (zipmap method-names attribute-names))))

(defmacro defdom [& names]
  (letfn [(declared-fields-vector [intf]
            (apply vector (map symbol (vals (-method-attributes intf)))))
          (defrecord-form [intf]
            (let [record-name (symbol (. intf getSimpleName))
                  field-names (declared-fields-vector intf)]
              `(defrecord ~record-name ~field-names)))]
    (let [builder-classes (map eval names)
          classes (map -built-class builder-classes)
          defrecord-forms (map defrecord-form classes)]
      `(do ~@defrecord-forms))))

(defmacro build [builder [& methodCalls]]
  (letfn [(setter [smb]
            (let [strng (str smb)]
              (symbol (str "set" (str/capitalize (first strng)) (str/join "" (rest strng))))))
          (to-call [[attribute value]]
            `(. ~builder ~(setter attribute) ~value))]
    `(do
       ~@(map to-call (partition 2 methodCalls))
       (. ~builder build))))

(defmacro type-instance [type-name instance instance-map]
  (let [target-class (eval type-name)
        get-meth-attrs (-method-attributes target-class)
        get-method (fn [[methodName attrName]]
                     (let [this (gensym)]
                       `(~(symbol methodName) [~this] (~(keyword attrName) ~instance))))
        get-methods (map get-method get-meth-attrs)]
    `(reify ~type-name
       ~@get-methods
       (toString [this] (str ~instance-map))
       (get [this] ~instance))))

(defmacro type-builder [builder]
  (let [builder-class (cond 
                        (class? builder) builder
                        :else (. Class forName (str builder)))
        builder-class-symbol (symbol (. builder-class getName))
        inst (gensym)
        builder (gensym)
        this (gensym)
        value (gensym)
        set-method-attrs (-method-attributes builder-class)
        set-method-maker (fn [[method-name keyword-field]]
                           (let [method-symbol (symbol method-name)
                                 field-keyword (keyword (symbol keyword-field))]
                             `(~method-symbol [~this ~value] (~builder (assoc ~inst ~field-keyword ~value)))))
        set-methods (map set-method-maker set-method-attrs) 
        target-class (-built-class builder-class)
        target-class-symbol (symbol (. target-class getName))
        mapctr (symbol (str "map->" (. target-class getSimpleName)))]
    `(letfn [(~builder [~inst]
               (reify ~builder-class-symbol
                 ~@set-methods
                 (toString [this] (str (assoc ~inst :builder ~target-class-symbol)))
                 (build [this] (type-instance ~target-class-symbol (~mapctr ~inst) ~inst))))]
       (~builder {}))))

;(defdom
;  jdata.examples.PersonBuilder
;  jdata.examples.NameBuilder
;  jdata.examples.AddressBuilder)

(defmacro builders [& builder-symbols]
  (let [ms (letfn [(append-symbols [list s]
                     (conj list s `(type-builder ~s)))]
             (reverse (reduce append-symbols (list) builder-symbols)))
        m (gensym)
        this (gensym)
        class-object (gensym)]
    `(let [~m { ~@ms ~@ms }]
       (reify jdata.core.Builders
         (getBuilder [~this ~class-object]
           (get ~m ~class-object))))))

(defn -getBuilders [this classes]
  (let [symbols (map symbol (map (fn [cl] (. cl getName)) (seq classes)))
        defdom-forms (cons 'data/defdom symbols)
        builders-form (cons 'data/builders symbols)]
    (eval defdom-forms)
    (eval builders-form)))

;(defn -main []
;  (println (macroexpand-1 '(defdom
;                             jdata.examples.PersonBuilder
;                             jdata.examples.NameBuilder
;                             jdata.examples.AddressBuilder)))
;  (println (map->Person {}))
;  (let [person (new Person
;                 (new Name "Kjetil" nil "Valstadsve")
;                 (new Address "Nonnegata" "21" "0656" "Oslo"))]
;    (println person))
;  (let [kjetil (build (type-builder jdata.examples.NameBuilder)
;                 [firstName "Kjetil"
;                  middleName "Jamne"
;                  lastName "Valstadsve"])]
;    (println (. kjetil toString))
;    (println (type kjetil))
;    (println (. kjetil getFirstName))
;    (println (:firstName kjetil))
;    (let [nb (type-builder jdata.examples.NameBuilder)
;          n1 (. (. (. nb setFirstName "Kjetil") setLastName "V") build)
;          n2 (. (. (. nb setFirstName "Thomas") setLastName "J") build)]
;      (println n1)
;      (println n2)
;      (println (type n1))
;      (println (type n2))
;      (println (. n1 getFirstName))
;      (println (. n1 get)))
;  (println (macroexpand '(builders
;                             jdata.examples.PersonBuilder
;                             jdata.examples.NameBuilder
;                             jdata.examples.AddressBuilder)))
;    (let [bs (builders "jdata.examples.PersonBuilder"
;                        jdata.examples.NameBuilder
;                        "jdata.examples.AddressBuilder")]
;      (println (. bs getBuilder jdata.examples.NameBuilder)))))

;(-main)
