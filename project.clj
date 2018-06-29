(defproject ebay-tools "0.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/data.zip "0.1.2"]
                 [clj-http "3.9.0"]
                 [clj-time "0.14.4"]
                 [hickory "0.7.1"]]
  :pedantic :abort
  :main ebay-tools.core
  :profiles {:uberjar {:aot :all
                       :uberjar-name "ebay-tools.jar"}})
