(ns clojure-lazy-sequences.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; =============================================================================
;; Finding Inflection Points

(def sample-data
  [[24.2 420031]
   [25.8 492657]
   [25.9 589014]
   [23.8 691995]
   [24.7 734902]
   [23.2 794243]
   [23.1 836204]
   [23.5 884120]])

(defn local-max? [[[a _] [b _] [c _]]]
  (and (< a b) (< c b)))

(defn local-min? [[[a _] [b _] [c _]]]
  (and (> a b) (> c b)))

(defn inflection-points [data]
  (lazy-seq 
   (let [current-series (take 3 data)]
     (cond (< (count current-series) 3) '()
           (local-max? current-series)
           (cons
            (conj (second current-series) :peak)
            (inflection-points (rest data)))
           (local-min? current-series)
           (cons 
            (conj (second current-series) :valley)
            (inflection-points (rest data)))
           :else
           (inflection-points (rest data))))))

;; Calculating a Running Average

(def endless-potatoes (repeatedly (fn [] (+ 10 (rand-int 390)))))

(defn average-potatoes [prev arrivals]
  (lazy-seq 
   (if-not arrivals
     '()
     (let [[_ n total] prev
           current [(first arrivals)
                    (inc (or n 0))
                    (+ (first arrivals) (or total 0))]]
       (cons current (average-potatoes current (next arrivals)))))))

;; In repl
;; (take 3 (average-potatoes '() endless-potatoes))
;; => ([321 1 321] [338 2 659] [318 3 977])
;; 
;; (last (take 500000 (average-potatoes '() endless-potatoes)))
;; => [43 500000 102132749]
