; https://www.hackerrank.com/challenges/simple-array-sum
(use '[clojure.string :only (split triml)])

(let [arrayLengthString (read-line) 
      arrayLength (Integer/parseInt arrayLengthString) 
      arrayLine (read-line)
      arrayString (split arrayLine #"\s+") 
      array (map #(Integer/parseInt %) arrayString)]
  (println(reduce + array)))
