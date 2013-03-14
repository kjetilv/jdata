(ns data
  (:require [clojure.string :as str]))

(defn built-class [builder-class]
  (first (. (first (. builder-class getGenericInterfaces)) getActualTypeArguments)))

(defn method-attributes [clazz]
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

;(defmacro reifier-body [builder-class target-class instance]
;  (let [->instance (symbol (str "map->" (. target-class getSimpleName)))
;        set-method (fn [[methodName attrName]]
;                     `(~(symbol methodName) [this value] 
;                        (reifier-body ~builder-class ~target-class (assoc ~instance ~(keyword attrName) ~value))))
;        get-method (fn [[methodName attrName]]
;                     `(~(symbol methodName) [this]
;                        (~(keyword attrName) ~instance)))
;        set-methods (map set-method set-meth-attrs)
;        get-methods (map get-method get-meth-attrs)]
;    `(reify ~builder-class
;       ~@set-methods
;       (toString [this] (str ~instance)) 
;       (build [this] (reify target-class
;                       ~@get-methods
;                       (toString [this] (str ~instance)))))))
;
;(defmacro defbuilder [builder-class-symbol]
;  (let [builder-class (eval builder-class-symbol)
;        target-class (built-class builder-class)
;        target-class-symbol (symbol (. target-class getName))
;        instance (gensym)
;        ]
;    (refifier-body builder-class target-class nil)))

(defmacro defbuilder [builder-class-symbol]
  (let [builder-class (eval builder-class-symbol)
        target-class (built-class builder-class)
        target-class-symbol (symbol (. target-class getName))
        set-meth-attrs (seq (method-attributes builder-class))
        instance (gensym)
        set-method (fn [[methodName attrName]]
                     (let [this (gensym) 
                           value (gensym)]
                       `(~(symbol methodName) [~this ~value] 
                          (do 
                            (def ~instance (assoc ~instance ~(keyword attrName) ~value))
                            ~this))))
        set-methods (map set-method set-meth-attrs)
        get-meth-attrs (seq (method-attributes target-class))
        get-method (fn [[methodName attrName]]
                     (let [this (gensym)]
                       `(~(symbol methodName) [~this]
                          (~(keyword attrName) ~instance))))
        get-methods (map get-method get-meth-attrs)
        mapctr (symbol (str "map->" (. target-class getSimpleName)))]
    `(do 
       (def ~instance (~mapctr {})) 
       (reify ~builder-class-symbol
         ~@set-methods
         (toString [this] (. ~instance toString))
         (build [this] (reify ~target-class-symbol 
                         ~@get-methods
                         (toString [this] (. ~instance toString))))))))

(defmacro type-instance [type-name type-instance]
  (let [target-class (eval type-name)
        get-meth-attrs (seq (method-attributes target-class))
        get-method (fn [[methodName attrName]]
                     (let [this (gensym)]
                       `(~(symbol methodName) [~this]
                          (~(keyword attrName) ~type-instance))))
        get-methods (map get-method get-meth-attrs)]
    `(reify ~type-name 
       ~@get-methods
       (toString [this] (. ~type-instance toString)))))

(defmacro type-builder [builder-class-symbol]
  (let [builder-class (eval builder-class-symbol)
        target-class (built-class builder-class)
        target-class-symbol (symbol (. target-class getName))
        set-meth-attrs (seq (method-attributes builder-class))
        instance (gensym)
        set-method (fn [[methodName attrName]]
                     (let [this (gensym) 
                           value (gensym)]
                       `(~(symbol methodName) [~this ~value] 
                          (do 
                            (def ~instance (assoc ~instance ~(keyword attrName) ~value))
                            ~this))))
        set-methods (map set-method set-meth-attrs)
        mapctr (symbol (str "map->" (. target-class getSimpleName)))]
    `(fn [~instance]  
       (reify ~builder-class-symbol
         ~@set-methods
         (toString [this] (. ~instance toString))
         (build [this] (type-instance ~target-class-symbol ~instance))))))

(defmacro defdom [& names]
  (letfn [(declared-fields-vector [intf]
            (apply vector (map symbol (vals (method-attributes intf)))))
          (defrecord-form [intf]
            (let [record-name (symbol (. intf getSimpleName))
                  field-names (declared-fields-vector intf)]
              `(defrecord ~record-name ~field-names)))]
    (let [builder-classes (map eval names)
          classes (map built-class builder-classes)
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

(defdom
  jdata.examples.PersonBuilder
  jdata.examples.NameBuilder
  jdata.examples.AddressBuilder)

(defn -main []
  (println (macroexpand-1 '(defdom
                             jdata.examples.PersonBuilder
                             jdata.examples.NameBuilder
                             jdata.examples.AddressBuilder)))
  (println (map->Person {}))
  (println (macroexpand-1 '(builder jdata.examples.PersonBuilder)))
  (let [person (new Person
                 (new Name "Kjetil" nil "Valstadsve")
                 (new Address "Nonnegata" "21" "0656" "Oslo"))]
    (println person))
  (println (macroexpand-1 '(defbuilder jdata.examples.NameBuilder)))
  (let [kjetil (build (defbuilder jdata.examples.NameBuilder)
                 [firstName "Kjetil"
                  middleName "Jamne"
                  lastName "Valstadsve"])]
    (println (. kjetil toString))
    (println (type kjetil))
    (println (. kjetil getFirstName))
    (println (:firstName kjetil))))
(-main)
