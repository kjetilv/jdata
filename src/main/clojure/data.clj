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

(defmacro defbuilder [builder-class-symbol]
  (let [builder-class (eval builder-class-symbol)
        target-class (built-class builder-class)
        meth-attrs (seq (method-attributes builder-class))
        instance (gensym)
        set-method (fn [[methodName attrName]]
                     (let [this (gensym)
                           value (gensym)]
                       `(~(symbol methodName) [~this ~value] (do (def ~instance (assoc ~instance ~attrName ~value)) ~this))))
        set-methods (map set-method meth-attrs)]
    `(do 
       (def ~instance (~(symbol (str 'map-> (. target-class getSimpleName))) {})) 
       (reify ~builder-class-symbol
         ~@set-methods
         (build [this] ~instance)))))

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
  (letfn [(setter [symb]
            (let [string (str symb)]
              (symbol (str "set" (str/capitalize (first string)) (str/join "" (rest string))))))
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
  (let [bldr (defbuilder jdata.examples.NameBuilder)
        kjetil (build bldr
                 [firstName "Kjetil"
                  middleName "Jamne"
                  lastName "Valstadsve"])]
    (println kjetil)
    (println (type kjetil))
    (println (get kjetil 'firstName))))
(-main)
