(ns clojure-exercices-ch4.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn map-with-reduce
  [f old-seq]
  (reduce
   (fn [new-seq val]
     (concat new-seq [(f val)]))
   ()
   old-seq))

(map-with-reduce inc '(1 2 3))

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spider-Man" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}])

(map-with-reduce :real identities)

(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats
  [numbers]
  (map-with-reduce #(% numbers) [sum count avg]))

(stats [3 4 10])

(stats [80 1 44 13 6])

(defn filter-with-reduce
  [f old-seq]
  (reduce
   #(if (f %2)
      (concat %1 [%2])
      %1)
   ()
   old-seq))

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(filter-with-reduce #(< (:human %) 5) food-journal)
(filter-with-reduce #(< (:month %) 3) food-journal)

(defn some-with-reduce
  [f coll]
  (reduce
   (fn [res val]
     (let [match (f val)]
       (if (and (nil? res) match)
         match
         res)))
   nil
   coll))

(some-with-reduce #(> (:critter %) 5) food-journal)

(some-with-reduce #(> (:critter %) 3) food-journal)

(some-with-reduce #(and (> (:critter %) 3) %) food-journal)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})
(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(convert :glitter-index "3")

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(parse (slurp filename))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (map
   #(:name %)
   (filter #(>= (:glitter-index %) minimum-glitter) records)))


(glitter-filter 3 (mapify (parse (slurp filename))))

(defn validate
  [vamp-keys record]
  (reduce
   (fn [val vamp-key]
     (if (nil? (vamp-key record))
       false
       val))
   true
   vamp-keys))

(defn append
  [suspect-name suspect-glitter]
  (let [to-append (into {} (map vector vamp-keys [suspect-name suspect-glitter]))]
    (conj (mapify (parse (slurp filename))) to-append)))

(def vamp-list (append "Cazuza" 30))

(defn to-csv
  [mapped-list]
  (clojure.string/join "\n"
                       (map #(clojure.string/join ","
                                                  (reduce (fn [row vamp-key]
                                                            (conj row (vamp-key %))) [] vamp-keys))
                            mapped-list)))


(with-open [wrtr (clojure.java.io/writer filename :append true)]
  (.write wrtr (to-csv vamp-list)))
