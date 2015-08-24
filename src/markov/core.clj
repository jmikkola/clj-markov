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

(defn run-chain [n-words window-size chain]
  (println (m/render-n-words n-words window-size chain)))

(defn file-to-chain [window-size filename]
  (with-words-in-file filename
    #(m/words-to-chain window-size %)))

(defn files-to-chain [window-size filenames]
  (m/combine-chains (map #(file-to-chain window-size %) filenames)))

(defn handle-file [filenames]
  (let [window-size 4
        n-words 1000]
    (->> filenames
         (files-to-chain window-size)
         (run-chain n-words window-size))))

(defn -main [& args]
  (if (empty? args)
    (print-demo)
    (handle-file args)))
