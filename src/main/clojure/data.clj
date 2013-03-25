(ns data
  (:require [clojure.string :as str]))

(def -built-class (memoize (fn [builder-class]
                             (first (. (first (. builder-class getGenericInterfaces)) getActualTypeArguments)))))

(def -method-attributes (memoize (fn [clazz]
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
                                       (zipmap method-names attribute-names))))))

(defmacro type-instance [type-name type-instance]
  (let [target-class (eval type-name)
        get-meth-attrs (seq (-method-attributes target-class))
        get-method (fn [[methodName attrName]]
                     (let [this (gensym)]
                       `(~(symbol methodName) [~this]
                          (~(keyword attrName) ~type-instance))))
        get-methods (map get-method get-meth-attrs)]
    `(reify ~type-name
       ~@get-methods
       (get [this] ~type-instance))))

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

(defmacro type-builder [builder-class-symbol]
  (let [builder-class (eval builder-class-symbol)
        target-class (-built-class builder-class)
        target-class-symbol (symbol (. target-class getName))
        inst (gensym)
        builder (gensym)
        this (gensym)
        value (gensym)
        set-method-attrs (seq (-method-attributes builder-class))
        set-method-maker (fn [[method-name keyword-field]]
                           (let [method-symbol (symbol method-name)
                                 field-keyword (keyword (symbol keyword-field))]
                             `(~method-symbol [~this ~value] (~builder (assoc ~inst ~field-keyword ~value)))))
        mapctr (symbol (str "map->" (. target-class getSimpleName)))]
    `(letfn [(~builder [~inst]
               (reify ~builder-class-symbol
                 ~@(map set-method-maker set-method-attrs)
                 (build [this] (type-instance ~target-class-symbol (~mapctr ~inst)))))]
       (~builder {}))))

(defdom
  jdata.examples.PersonBuilder
  jdata.examples.NameBuilder
  jdata.examples.AddressBuilder)

(defn builders []
  (let [m { jdata.examples.NameBuilder (type-builder jdata.examples.NameBuilder)
            jdata.examples.AddressBuilder (type-builder jdata.examples.AddressBuilder)
            jdata.examples.PersonBuilder (type-builder jdata.examples.PersonBuilder)}]
    (reify jdata.core.Builders
      (getBuilder [this class-object]
        (get m class-object)))))
          
(defn -main []
  (println (macroexpand-1 '(defdom
                             jdata.examples.PersonBuilder
                             jdata.examples.NameBuilder
                             jdata.examples.AddressBuilder)))
  (println (map->Person {}))
  (let [person (new Person
                 (new Name "Kjetil" nil "Valstadsve")
                 (new Address "Nonnegata" "21" "0656" "Oslo"))]
    (println person))
  (let [kjetil (build (type-builder jdata.examples.NameBuilder)
                 [firstName "Kjetil"
                  middleName "Jamne"
                  lastName "Valstadsve"])]
    (println (. kjetil toString))
    (println (type kjetil))
    (println (. kjetil getFirstName))
    (println (:firstName kjetil))
    (let [nb (type-builder jdata.examples.NameBuilder)
          n1 (. (. (. nb setFirstName "Kjetil") setLastName "V") build)
          n2 (. (. (. nb setFirstName "Thomas") setLastName "J") build)]
      (println n1)
      (println n2)
      (println (type n1))
      (println (type n2))
      (println (. n1 getFirstName))
      (println (. n1 get)))
;  (println (macroexpand-1 '(builders
;                             jdata.examples.PersonBuilder
;                             jdata.examples.NameBuilder
;                             jdata.examples.AddressBuilder))))
    (let [bs (builders)]
;               jdata.examples.PersonBuilder
;               jdata.examples.NameBuilder
;               jdata.examples.AddressBuilder)]
      (println (. bs getBuilder jdata.examples.NameBuilder)))))

(-main)
