(ns assignments.util)

(defn implemented? [f]
  (-> f resolve meta :implemented?))

(defn insert-println-if-implemented [form]
  (let [f (first form)]
    (when (and (resolve f) (implemented? f))
      (list '(println "---")
            `(println "Running: " '~form)
            `(println "Result:  " ~form)))))

(defmacro print-and-do [& forms]
  (let [forms-with-println (mapcat insert-println-if-implemented forms)]
    (list* 'do forms-with-println)))

(defn divisible-by? [dividend divisor] (zero? (mod dividend divisor)))

(defn is-sequence-present-once [subcoll supercoll]
    (= subcoll (filter (set subcoll) supercoll)))

(def trans (partial apply map vector))

(defn is-duplicate-entry [coll]
  (not= (count (set coll)) 9))

(defn get-grid-format [coll]
  (partition 9 (flatten coll)))

(defn validate-sets [grid]
  (zero? (count (filter is-duplicate-entry grid))))

(defn get-small-sudokus [grid]
  (partition 9 (flatten (trans (map (partial partition 3) grid)))))

(defn validate-columns [grid]
  (validate-sets (apply map vector grid)))

(defn validate-small-sudokus [grid]
  (validate-sets (get-small-sudokus grid)))