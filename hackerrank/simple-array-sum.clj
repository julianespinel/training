; https://www.hackerrank.com/challenges/simple-array-sum
(use '[clojure.string :only (split triml)])

(let [n_t (read-line) 
      n (Integer/parseInt n_t) 
      arr_temp (read-line)
      arr_t (split arr_temp #"\s+") 
      arr (map #(Integer/parseInt %) arr_t)]
  (println(reduce + arr)))
