(ns markov.core
  (:gen-class))

(require '[markov.markov :as m])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (m/render-n-words 50 m/example-chain)))
