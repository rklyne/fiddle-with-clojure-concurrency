(ns teach-clojure.core
  (:require [clojure.tools.logging :as log]))

(comment "Parallel prime search")

(defn memo [f]
  (let [r (ref {})
        g (fn [& args]
            (dosync
              (if (contains? (ensure r) args)
                (get @r args)
                (let [value (apply f args)]
                  (alter r #(assoc % args value))
                  value))))]
    g))


(defn is-prime? [n]
  (comment (= 2 (count (filter #(= 0 (mod n %))
                               (range 1 (inc n))))))
  (->> (filter is-prime? (range 2 (Math/sqrt n)))
       (some #(not (= 0 (mod n %))))
    )
  )

;(def is-prime? (memo is-prime?))

(defmacro time-of [& body]
  (let [start (gensym)]
    `(let [~start (System/currentTimeMillis)]
       (do ~@body)
       (- (System/currentTimeMillis) ~start))))




(defn primes-up-to [N]
  (comment (map second
        (filter #(deref (first %))
                (map #(vector (delay (is-prime? %)) %)
                     (range 1 (inc N))))))
  (->> (range 1 (inc N))
       (map #(vector (future (is-prime? %)) %))
       (filter #(deref (first %)))
       (map second))
  )










(defn ref-test []
  (let [NTHREADS 10
        T (fn [] (int (rand 100)))
        in (map ref (partition NTHREADS (range 100)))
        out (ref [])]
    (dorun (pmap
             (fn [in_list]
               (dosync
                 (let [value (first @in_list)]
                   (Thread/sleep (T))
                   (alter in_list rest)
                   (commute out #(conj % value))))
               (when-not (empty? @in_list)
                 (recur in_list)))
             in))
    @out
    ))
    
































(comment "Parallel factor search on bulk input")

(defn factors [n]
  (->> (range 2 (inc n))
       (filter #(= 0 (mod n %)))
       (cons 1)
       ))


(defn process-file [name]
  (let [process-line
        (fn [line] (factors (Integer/parseInt line)))]
    (with-open [rdr (clojure.java.io/reader name)]
      (doall 
        (pmap process-line
              (filter #(not (= "" %)) (line-seq rdr)))))))






