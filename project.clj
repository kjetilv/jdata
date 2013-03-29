(defproject jdata "0.1.0-SNAPSHOT"

  :description "Data in Java? Jure!"
  :url "https://github.com/kjetilv/jdata"

  :aot :all

  :source-paths      [ "src/main/clojure", "src/test/clojure" ]
  :java-source-paths [ "src/main/java", "src/test/java" ]
  :javac-options     [ "-target" "1.7" "-source" "1.7" ]

  :plugins [[lein-junit "1.1.2"]]
  :junit ["src/test/java"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [joda-time/joda-time "2.1"]
                 [junit/junit "4.11" :scope "test"]]

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"})
