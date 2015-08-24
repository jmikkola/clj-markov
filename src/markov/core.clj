(ns markov.core
  (:gen-class))

(require '[markov.markov :as m])

(defn -main [& args]
  (println (m/render-n-words 50 2 m/example-chain)))

(defn words-in-file [filename]
  (with-open [file (clojure.java.io/reader filename)]
    (re-seq #"/s+" file)))
