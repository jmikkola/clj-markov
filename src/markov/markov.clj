(ns markov.markov)

(require '[clojure.string :as str])

;; Chain building
(def empty-chain {:end {:start 1}})

(defn add-pair [from to chain]
  (update-in chain [from to] (fnil inc 0)))

(defn add-tokens [tokens chain]
  (let [[from to] tokens]
    (if to
      (add-tokens (rest tokens) (add-pair from to chain))
      chain)))

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

(defn tokens-to-chain [tokens]
  (add-tokens tokens empty-chain))

(def s-to-chain (comp tokens-to-chain words-to-tokens s-to-words))

(def example-chain (s-to-chain "I do not like eggs in the file. I do not like them in any style. I will not take them fried or boiled. I will not take them poached or broiled. I will not take them soft or scrambled, Despite an argument well-rambled. No fan I am of the egg at hand."))

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

(defn next-token [last-token chain]
  (rand-choice (chain last-token)))

(defn token-seq [last-token chain]
  (cons last-token (lazy-seq (token-seq (next-token last-token chain) chain))))

(defn format-tokens [tokens]
  (if (empty? tokens)
    '()
    (let [fst (first tokens)
          snd (second tokens)]
      (cond
       (nil? snd) (if (= fst :start) '() (list fst))
       (= fst :start) (format-tokens (cons (str/capitalize snd) (drop 2 tokens)))
       (= snd :end) (cons (.concat fst ".") (format-tokens (drop 2 tokens)))
       :else (cons format (fst-tokens (rest tokens)))))))

(defn render [tokens]
  (str/join " " (format-tokens tokens)))

(defn render-n-words [n chain]
  (render (take n (token-seq :start chain))))
