(ns markov.markov)

(require '[clojure.string :as str])

;; Chain building
(def empty-chain {:end {:start 1}})

(defn add-pair [from to chain]
  (update-in chain [from to] (fnil inc 0)))

(defn heads [items]
  "Return a sequence of lists of heads (e.g. (0) (0 1) (0 1 2)...)."
  (if (empty? items)
    []
    (let [head (first items)]
      (cons (list head)
            (lazy-seq (map (partial cons head) (heads (rest items))))))))

(defn tails [items]
  "Returns a list of tail lists, E.G. (0 1 2) (1 2) (2)."
  (if (empty? items)
    '()
    (cons items (tails (rest items)))))

(defn groupings [size items]
  (if (empty? items)
    (list)
    (lazy-cat
     (rest (take size (heads items)))
     (groupings size (rest items)))))

(defn add-tokens [window-size tokens chain]
  (reduce
   (fn [chain grouping]
     (add-pair (drop-last grouping) (last grouping) chain))
   chain
   (groupings window-size tokens)))

(defn last-char [s]
  (.charAt s (dec (.length s))))

(defn ends-sent? [word]
  (= (last-char word) \.))

(defn s-to-words [s]
  (map str/lower-case (filter #(< 0 (.length %)) (str/split s #" "))))

(defn period? [word] (= word "."))

(declare read-sentance)

(defn start-sentance [words]
  "Handles tokens when a sentance has not yet started"
  (if-let [first-word (first words)]
    (if (period? first-word)
      (start-sentance (rest words))
      (cons :start (read-sentance words)))
    '()))

(defn clean-word [word]
  (str/replace word #"\W+$" ""))

(defn read-sentance [words]
  "handles tokens when a sentance has already started"
  (if-let [first-word (first words)]
    (if (ends-sent? first-word)
      (cons (clean-word first-word) (cons :end (start-sentance (rest words))))
      (cons first-word (read-sentance (rest words))))
    '(:end)))

(defn words-to-tokens [words]
  (start-sentance words))

(defn tokens-to-chain [tokens window-size]
  (add-tokens window-size tokens empty-chain))

(defn words-to-chain [window-size words]
  (-> words
       words-to-tokens
       (tokens-to-chain window-size)))

(defn s-to-chain [window-size s]
  (-> s
      s-to-words
      words-to-tokens
      (tokens-to-chain window-size)))

(def example-chain (s-to-chain 3 "I do not like eggs in the file. I do not like them in any style. I will not take them fried or boiled. I will not take them poached or broiled. I will not take them soft or scrambled, Despite an argument well-rambled. No fan I am of the egg at hand."))

;; Output generation
(defn rand-range [range]
  (Math/round (* range (rand))))

(defn pick-weighted [choice weighted-options]
  (let [[option weight] (first weighted-options)]
    (if (< choice weight)
      option
      (pick-weighted (- choice weight) (rest weighted-options)))))

(defn rand-choice [weighted-options]
  (let [weight (reduce + (vals weighted-options))
        choice (rand-range (dec weight))]
    (pick-weighted choice (seq weighted-options))))

(defn next-options [recent-tokens chain]
  (->> recent-tokens
       tails
       (map chain)
       (remove nil?)
       first))

(defn next-token [recent-tokens chain]
  (rand-choice (next-options recent-tokens chain)))

(defn add-recent [token recent-tokens window-size]
  (let [tokens (conj recent-tokens token)]
    (if (> (count tokens) window-size)
      (subvec tokens 1)
      tokens)))

(defn token-seq [recent-tokens window-size chain]
  (let [token (next-token recent-tokens chain)
        recent-tokens' (add-recent token recent-tokens window-size)]
    (cons token (lazy-seq (token-seq recent-tokens' window-size chain)))))

(defn format-tokens [tokens]
  (if (empty? tokens)
    '()
    (let [fst (first tokens)
          snd (second tokens)]
      (cond
       (nil? snd) (if (= fst :start) '() (list fst))
       (= fst :start) (format-tokens
                       (cons (str/capitalize snd) (drop 2 tokens)))
       (= snd :end) (cons (.concat fst ".") (format-tokens (drop 2 tokens)))
       :else (cons fst (format-tokens (rest tokens)))))))

(defn render [tokens]
  (str/join " " (format-tokens tokens)))

(defn tokens [window-size chain]
  (cons :start (token-seq [:start] window-size chain)))

(defn render-n-words [n window-size chain]
  (render (take n (tokens window-size chain))))
