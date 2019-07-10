(ns fwpd.core)

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

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps from unmapped rows in a collection"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))


(defn append
  "Appends a new name to a list of names"
  [coll new-name]
  (apply conj (list new-name) coll))

(defn validate
  "Checks that stated keywords are present in mapped record"
  [record]
  (and (:name record) (:glitter-index record)))

(def vamp-list (glitter-filter 3 (mapify (parse (slurp filename)))))

(defn revert
  [map-list]
  (clojure.string/join "\n" (map #(clojure.string/join "," [(:name %) (:glitter-index %)]) map-list)))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

(defn my-comp
  ([single-fn] single-fn)
  ([f & fns]
    (fn [& args]
      (f (apply (apply my-comp fns) args)))))