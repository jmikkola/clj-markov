(ns markov.core
  (:gen-class))

(require '[clojure.string :as str])
(require '[markov.markov :as m])

(defn print-demo []
  (println (m/render-n-words 50 2 m/example-chain)))

(defn lazy-cat-map [f items]
  (if (empty? items)
    '()
    (lazy-cat
     (f (first items))
     (lazy-cat-map f (rest items)))))

(defn split-words [s]
  (filter #(> (.length %) 0)
          (str/split s #"\s+")))

(defn with-words-in-file [filename f]
  (with-open [file (clojure.java.io/reader filename)]
    (f (lazy-cat-map split-words (line-seq file)))))

(defn handle-file [filename]
  (let [window-size 3
        n-words 200]
    (with-words-in-file filename
      (fn [words] (->> words
                       (m/words-to-chain window-size)
                       (m/render-n-words n-words window-size)
                       println)))))

(defn -main [& args]
  (if (empty? args)
    (print-demo)
    (handle-file (first args))))
