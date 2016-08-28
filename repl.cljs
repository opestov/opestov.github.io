(ns o8v.repl
  [:require clojure.browser.repl])

(defonce conn
  (clojure.browser.repl/connect "http://localhost:9000/repl"))
