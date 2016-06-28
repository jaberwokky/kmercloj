(ns kmer.core
  (:gen-class))


(defn count-substring [txt sub]
  "Function to count occuriencies of substring in string with regular expressions"
  (count (re-seq (re-pattern sub) txt)))

(defn kmers-parallel [txt k]
  "The most freuet kmer function"
  (let [theset 
        (filter 
          #(= (count %) k) 
          (set (clojure.string/split (clojure.string/replace txt (re-pattern (str "(?=(\\w{" k "})).")) "$1#") #"#")))
        mytmp 
        (zipmap
          theset
          (pmap #(count-substring txt (str "(?=" % ")"))
                theset) )]
    (let [apmax (val (apply max-key val mytmp)) thefil (filter (comp #{apmax} mytmp) (keys mytmp))]
      (pmap list (repeat 
                   (count thefil) 
                   apmax)
            thefil))))


(defn -main [& args]
  (prn (format "args=%s" args))
  (if (not (empty? args))
    (do
      (def txt (slurp (nth args 0)))
      (def answer (for [i (range 1 (inc (read-string (nth args 1))))]
                    (conj [i] (kmers-parallel txt i))))
      (prn answer))))
