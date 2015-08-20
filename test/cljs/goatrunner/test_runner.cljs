(ns goatrunner.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [goatrunner.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'goatrunner.core-test))
    0
    1))
