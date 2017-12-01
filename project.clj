(defproject net.thegeez.advent-2017 "0.0.1"
  :dependencies [[org.clojure/clojure "1.9.0-RC2"]
                 [org.clojure/clojure "1.9.0-RC1"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.specs.alpha "0.1.24"]

                 [org.clojure/core.async "0.3.442"]

                 [net.cgrand/xforms "0.15.0"]
                 ]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
