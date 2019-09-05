(ns projecteuler.solutions
  (:require [projecteuler.tools :as t])
  (:require [clojure.math.combinatorics :refer [permutations]])
  (:require [clojure.set :refer [union difference]]))
  
; Problem Num.2 
(defn problem-num-02 []
  (reduce  +
           (take-while #(< % 4000000)
                       (filter even?
                               (map #(first %)
                                    (iterate #(vector (+ (first %) (second %) ) (first %))
                                             [1 1]))))))

;  Problem Num.3 
; simple!

;  Problem Num.4 
(defn palin-check [n]
  (let [ [a r ap] [(int (/ n 100000)) (rem n 100000) (rem n 10)]
         [b s bp] [(int (/ r 10000))   (rem r 10000)  (int (/ (rem n 100) 10))]
         [c t cp] [(int (/ s 1000))    (rem s 1000)   (int (/ (rem n 1000) 100))]]
        (if (and (= a ap) (= b bp) (= c cp))  true)  false))

(defn problem-num-04 []
  (first
   (sort >
         (take 100
               (filter palin-check
                       (map #(* (first %) (second %))
                            (iterate #(if (= 100  (first %))
                                        [100 100]
                                        (if (= 100  (second %))
                                          [(dec (first %)) (dec (first %))]
                                          [(first %) (dec (second %))]))
                                     [999 999])))))))
;  Problem Num.5 
(defn problem-num-05 []
  (reduce t/lcm (range 1 20)))

;  Problem Num.6 
(defn problem-num-06 []
  (/ (* 99 100 101 302) 12)) ; 25164150

;  Problem Num.7 
(defn problem-num-07 []
  (last (nth (iterate #(conj % (t/NextPrime %)) [2 3]) 9999))) ;104743

;  Problem Num.8 
(def n1000 (vec
            (map #(Integer/parseInt %)
                 (map str (seq "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")))))

(defn problem-num-08 []
  (apply max
         (map  #(reduce * %)
               (map  #(subvec n1000 % (+ % 5))
                     (range 995)))))

; Problem Num.9  
(defn problem-num-09 []
  (reduce *
          (loop [a 1 b 2 ]
            (let [c (Math/sqrt (+ (Math/pow  a 2) (Math/pow  b 2)))]
              (if (and (zero? (- c (int c))) (= 1000 (+ a b (int c))))
                [a b (int c)]
                (if (>= b 500)
                  (recur (inc a) (+ a 2))
                  (recur a (inc b))))))))

; Problem Num.10 ---
(defn problem-num-10 []
  (reduce + (t/Primes 2000000)))

; Problem Num.11 ---
; First Day of 1901 is Tuesday (because 364th=7*52 day is Monday!)
; Date format is [mm dd yy]

(defn leap-check [v]
  (let [year (v 1)]
    (if (or (and (not= (rem year 100) 0) (= (rem year 4) 0)) (= (rem year 400) 0)) (update-in v [0 1] inc)  v)))

(defn sunday [Date1 WeekDay  Date2]
  (let [mdy [[31 28 31 30 31 30 31 31 30 31 30 31] (Date1 2)]
        allm (reduce #(into % (%2 0)) [] (take-while #(<= (second %) (Date2 2)) (iterate (comp leap-check #(update-in % [1] inc)) mdy)))
        fdm (filter #(not (zero? (second %))) (map-indexed vector (reduce into [] (map #(cons 1 (take (dec %) (repeat 0))) allm))))
        alldw (reduce into [] (take (quot (reduce + allm) 7) (repeat (map #(rem % 7) (take 7 (iterate inc WeekDay))))))]
    (count (filter #(zero? %)  (map #(nth alldw (first %)) fdm)))))

(defn problem-num-11 []
  (sunday [1 1 1901] 2 [1 1 2001]))    

; Problem Num.12 ---
(defn problem-num-12 []
  (+ 2
     (count
      (take-while #(< % 500)
                  (map #(reduce * (map inc %))
                       (map (comp vals frequencies t/Factor)
                            (map #(/ (* % (inc %)) 2)
                                 (iterate inc 2))))))))

; Problem Num.13 ---
;Simple

; Problem Num.14 ---

(defn collatz [n]
  (if (even? n) (/ n 2) (inc (* n 3))))

(defn collatz-seq [n]
  (take-while #(> % 1) (iterate collatz n)))

(def  all-seq (map count (map collatz-seq  (range 1 1000000 2)))) ; 0.0 miliseconds (It's Lazy)

(defn problem-num-14 []
  (let [cz-max (reduce max all-seq)]
    (inc (* 2 (count
               (take-while #(< % cz-max)
                           all-seq)))))) ; 31.8 seconds

; Problem Num.15 ---
; simple

; Problem Num.16 ---

(def a16 (bigint (Math/pow 2 50)))

(defn problem-num-16 []
  (reduce +
          (map #(Character/getNumericValue %)
               (str (nth (iterate #(* % a16) 1) 20)))))

; Problem Num.17 ---

(def ntxmap   {1 "One" ,2 "Two",3 "Three",4 "Four",5 "Five",6 "Six",7 "Seven",8 "Eight",9 "Nine",
               11 "Eleven",12 "Twelve",13 "Thirteen",14 "Fourteen",15 "Fifteen",16 "Sixteen",
               17 "Seventeen",18 "Eighteen",19 "Ninteen", 10 "Ten" ,20 "Twenty",30 "Thirty",
               40 "Forty",50 "Fifty",60 "Sixty",70 "Seventy",80 "Eighty",90 "Ninety"})

(def ntxhmap {1 "Thousand",2 "Million",3 "Billion",4 "Trillion"})

(defn num-txt [n]
  (loop [sn (reverse (partition 3 3 [\0 \0] (reverse (str n))))  i (dec (count sn)) text []]
    (if (empty? sn)
    ;      text
    (reduce concat text)  
    (let [st (reverse (nth sn 0)) h (ntxmap (Character/getNumericValue (nth st 0)))]
      (recur  (drop 1 sn)   (dec i)
              (into text
                    (into  (if (not (nil? h)) (vector h "Hundred") [])
                           (into (let [r2 (ntxmap (Integer. (apply str (rest st))))]
                                   (if (nil? r2)
                                     (let [k (vector (ntxmap (* 10 (Character/getNumericValue (nth st 1)))) (ntxmap (Character/getNumericValue (nth st 2))))]
                                       (if (nil? h) k (into ["and"] k)))
                                     [r2]))
                                 (if (> i 0) [(ntxhmap i)] [])))))))))

(defn problem-num-17 []
  (count (reduce concat (map num-txt (range 1 1001))))) ; 20412

; Problem Num.18 ---

(def Nums18 [                      [75]
                                 [95 64]
                                [17 47 82]
                              [18 35 87 10]
                             [20 04 82 47 65]
                            [19 01 23 75 03 34]
                           [88 02 77 73 07 63 67]
                          [99 65 04 28 06 16 70 92]
                        [41 41 26 56 83 40 80 70 33]
                       [41 48 72 33 47 32 37 16 94 29]
                      [53 71 44 65 25 43 91 52 97 51 14]
                     [70 11 33 28 77 73 17 78 39 68 17 57]
                   [91 71 52 38 17 14 91 43 58 50 27 29 48]
                 [63 66 04 68 89 53 67 30 73 16 69 87 40 31]
                [04 62 98 27 23 9 70 98 73 93 38 53 60 04 23]])

(def Nums67 (read-string (slurp "./resources/triangle67.txt")))

(defn Pn [vo vn]
  (let [n (count vo)]
    (loop [i 1 v [(+ (vn 0) (vo 0))]]
      (if (= i n)
         (conj v (+ (vn n) (vo (dec n))))
        (recur (inc i) (conj v (+ (vn i) (max (vo i) (vo (dec i))))))))))

(defn problem-num-18 []
  (reduce max (reduce Pn Nums18))) ; 1074

(defn problem-num-67 []
  (reduce max (reduce Pn Nums67))) ; 7273 ; 1.37 msecs
        
; Problem Num.19 ---

(def Nums19 [
             8 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 8
             49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
             81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
             52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
             22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
             24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
             32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
             67 26 20 68 02 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21
             24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
             21 36 23 9 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
             78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 9 53 56 92
             16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
             86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
             19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
             04 52 8 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
             88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
             04 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36
             20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
             20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
             01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48])

(defn problem-num-19 []
  (reduce max (for [i (range 20) j (range 20)]
                (let [ij (+ (* i 20) j)]
                  (max
                   (if (< j 17) ; Horizontal
                     (reduce  * (map #(Nums19 (+ ij %)) [0 1 2 3]))
                     1)
                   (if (< i 17) ; Vertical
                     (reduce  * (map #(Nums19 (+ ij %)) [0 20 40 60]))
                     1)
                   (if (and (< i 17) (< j 17)); Diagonal
                     (reduce  * (map #(Nums19 (+ ij %)) [0 21 42 63]))
                     1)
                   (if (and (> j 2) (< i 17)) ; Diagonal
                     (reduce  * (map #(Nums19 (+ ij %)) [0 19 38 57]))
                     1))))))
                           
; Problem Num.20 ---
(def f100 (loop [n 100N r 99]
  (if (= r 1)
    n
    (if (zero? (mod n 10))
      (recur (/ n 10) r)
      (recur (* n r) (dec r))))))

(defn problem-num-20 []
  (reduce + (map #(Integer/parseInt %) (map str (seq (str f100)))))) ; 648

; Problem Num.21 ---
(defn problem-num-21 []
  (let [a (zipmap (range 1 10000) (map #(- (t/DivisorSum %) %) (range 1 10000)))]
    (reduce #(+ % (first %2)) 0 (filter #(let [k (first %)] (and (not= (a k) k) (= k (second %)))) (zipmap (keys a) (map a (vals a)))))))

; Problem Num.22 ---
(def Names22 (map-indexed vector (sort compare (read-string (str \( (slurp "./resources/names.txt") \))))))
(def Alphabets (zipmap (map char (range (int \A) (inc (int \Z)))) (range 1 27)))

(defn strval [s]
  (reduce + (map Alphabets (seq s))))

(defn problem-num-22 []
  (reduce + (map #(* (inc (% 0)) (strval (% 1))) Names22)))

; Problem Num.23 ---

(def Num23 20162)
  
(defn selfadd [v]
  (for [i (range (count v))]
    (let [n ((vec v) i)]
      (set (filter #(< % Num23) (map #(+ n %) (drop i v)))))))

(defn problem-num-23 []
  (let [r (range 1 Num23)]
    (reduce +
            (difference (set r)
                        (reduce union (selfadd
                                       (map #(first %)
                                            (filter #(< (first %) (second %))
                                                    (partition 2
                                                               (interleave r
                                                                           (map #(- (t/DivisorSum %) %)
                                                                                  r))))))))))  ; 4179871
  
; Problem Num.24 ---
;Simple
; Problem Num.25 ---
;Simple
; Problem Num.26 ---
;Simple
; Problem Num.29 ---
(defn problem-num-29 []
  (count (set (for [a (range 2 101) b (range 2 101)]
                (Math/pow a b)))))
  
; Problem Num.30 ---

(def p5 (zipmap (range 10) (map #(t/power-long % 5)) (range 10))))

(defn problem-num-30 []
  (reduce + (filter #(not (nil? %))
                    (for [i (range 10 200000)]
                      (if (= i (reduce + (map p5 (t/digs i))))
                        i))))) ; 443839

; Problem Num.31 ---
;Simple
; Problem Num.32 ---
(def perms32 (permutations (range 1 10)))

(defn find32 [v]
  (for [i (range 1 8) j (range (inc i) 9)]
    (let [mand   (Integer/parseInt (apply str (subvec v 0 i)))
          mlier  (Integer/parseInt (apply str (subvec v i j)))
          prdct   (Integer/parseInt (apply str (subvec v j 9)))]
      (if (= prdct (* mand mlier)) prdct))))

(def a32  (distinct (filter (complement nil?) (map find32 (permutations (range 1 10))))))

(defn problem-num-32 []
  (reduce + (rest (distinct (flatten a32)))))


; Problem Num.33 ---
(defn problem-num-33 []
  (for [bc (range 12 100) ab (range 10 bc) :when (> (rem bc 10) 0)]
    (let [a (quot ab 10)
          c (rem bc 10)]
      (if (and (= (quot bc 10) (rem ab 10))
               (= (/ ab  bc) (/ a c)))
        (/ a c)))))

; Problem Num.34 ---
(defn factori [n]
  (reduce * (range 1 (inc n))))

(def digfac
  (zipmap (range 10) (map factori (range 10))))

(defn problem-num-34 []
  (filter (complement zero?)
          (for [i (range 1000000)]
            (if (= (reduce + (map digfac (t/digs i))) i)
              i
              0))))


; Problem Num.35 ---
(def p10p6 (t/Primes 1000000))

(defn problem-num-35 []
  (inc (count (filter (complement  zero?) (for [p p10p6]
                                            (if (t/circular? p)
                                              p
                                              0))))))

; Problem Num.36 ---
(defn palindrome? [s]
  (if (= s (apply str (reverse s))) true false))

(defn palin-dec-bin? [n]
  (if (and (palindrome? (str n)) (palindrome? (Integer/toBinaryString n))) n))

(defn problem-num-36 []
  (reduce + (filter (complement nil?) (map palin-dec-bin? (range 1 1000000 2)))))

  
; Problem Num.37 ---

(defn TPfilter [n]
  (let [dn (t/digs n)]
    (if (or (some even? dn) (some #(= % 5) dn)) false true)))

(defn TruncPrime? [n PList]
  (loop [s n sqtn (long (Math/sqrt s))]
    (let [qs (quot s 10) pv (take-while #(< % sqtn) PList)]
      (if (zero? s) true
          (if (t/Prime? s pv)
            (recur qs (long (Math/sqrt qs )))
            false)))))

(defn rTruncPrime? [n PList]
  (TruncPrime? (Integer/parseInt (apply str (reverse (str n))))) PList)

; Problem Num.41 ---
(defn problem-num-41 []
  (apply max (filter #(t/Prime? %) (map #(Integer/parseInt %) (map #(apply str %) (permutations (range 1 8)))))))

; Problem Num.42 ---
(defn Triangle [n]
  (/ (* n (inc n)) 2))

(def triangles (zipmap (map #(Triangle %) (range 1 30)) (range 1 30)))

(defn str2val [s] (reduce + (map #(- (int %) 64) (seq s))))

(def Num42 (map str2val (read-string (str \( (slurp "./resources/words.txt") \)))))

(defn problem-num-42 []
  (count (filter (complement nil?) (map triangles Num42))))
  
; Problem Num.44 ---
(defn PentNum [n]
  (/ (* n (dec (* 3 n)))2))

(defn Pentagonal? [x]
  (if-let [r (t/Sqr? (inc (* 24 x)))]
    (if (zero? (rem (inc r) 6))
      true)))

(defn PentSearch [v i]
  (let [x (last v)]  
    (loop [j (- i 2)]
      (if (>= j 0)
        (if-let[res (let [vj (v j)]
                      (if (and (Pentagonal? (+ x vj)) (Pentagonal? (- x vj)))
                        (- (PentNum i) (PentNum (inc j)))))]
          res
          (recur (dec j)))))))
        
(defn problem-num-44 []
  (loop [i 2 v [1 5]]
    (if-let [result (PentSearch v i)]
      result
      (recur (inc i) (conj v (PentNum (inc i)))))))


; Problem Num.45 ---
(defn Hexagonal? [x]
  (if-let [r (t/Sqr? (inc (* 8 x)))]
    (if (zero? (rem (inc r) 4))
      true)))

(defn problem-num-45 []
  (loop [i 287]
    (if (Pentagonal? (Triangle i))
      i
      (recur (+ i 2)))))

; Problem Num.46 ---
(def Composits (filter #(not (t/Prime? %)) (iterate #(+ 2 %) 1)))
(def DblSqrs (map #(* 2 % %) (iterate inc 1)))

(defn pds? [x]
  (some t/Prime? (map #(- x %) (take-while #(> x %) DblSqrs))))

(defn problem-num-33 []
  (first (for [i Composits :when (not (pds? i))] i)))
  
; Problem Num.47 ---
(defn DF [n i] (= (count (distinct (t/Factor n))) i))
(defn DF4 [n] (every? true? (map #(DF % 4) (take 4 (iterate inc n)))))

(defn problem-num-46 []
  (first (for [i (iterate inc 1) :when (DF4 i)] i)))

; Problem Num.48 ---
(defn SelfPow [m]
  (let [pm (Math/pow 10 10)]
  (loop [i (dec m) p m]
    (if (zero? i)
      (rem p pm)
      (if (> p pm)
        (recur (dec i) (* (rem p pm)  m))
        (recur (dec i) (* p m)))))))

(defn problem-num-48 []
  (reduce + (map #(SelfPow %) (range 1 1001))))

; Problem Num.49 ---
(def p4 (filter t/Prime? (range 1000 10000)))

(defn pdigs [n] (sort < (t/digs n)))

(defn arith? [v] 
  (for [i (range 2 (count v)) j (range 1 i) k (range j) :when (= (* 2 (v j)) (+ (v i) (v k)))]
    (map v (vector i j k))))

(defn problem-num-49 []
  (for [i (range 3 12) 
        :let [l (filter #(= i (count %)) (vals (group-by pdigs  p4))) 
              m (filter (complement empty?) (map arith? l))]
        :when (not (empty?  m))]
    m))

; Problem Num.50 ---

(def Num50Max 1000000)
(def p50 (t/Primes Num50Max))

(defn N50AUX1 [n]       ; Minimum & Maximum values for n
  (loop  [i 1 sum 2 v i vs 2]
             (if (>= sum n) 
               (vector v vs i sum)
               (if (t/Prime? sum)
                 (recur (inc i) (+ sum (nth p50 i)) i sum)
                 (recur (inc i) (+ sum (nth p50 i)) v vs)))))

(defn N50AUX2 [sum n]   ; Sliding Window
  (loop [psum sum i 0]         
    (if (< psum Num50Max)
      (if (t/Prime? psum)
        psum
        (recur (- (+ psum (nth p50 (+ n (inc i)))) (nth p50 i)) (inc i))))))

(defn problem-num-50 []
  (let [mnmx (N50AUX1 Num50Max)]
    (loop [sum (mnmx 1) n (mnmx 0) v [sum]]
      (let [nsum (+ sum (nth p50 n))
            psum (N50AUX2 nsum n)]
        (if (< n (mnmx 2))
          (recur nsum (inc n) (conj v psum))
          v)))))

; Problem Num.52 ----
;First Cyclic Number
;=>142857
  
; Problem Num.53 ----
(defn problem-num-53 []
  (loop [V [1N 1N] i 1 m 0]
    (if (= i 101) 
      m
      (recur (vec (map + (vec (cons 0 V)) (conj V 0))) (inc i) (+ m (count (filter #(> % 1e6) V)))))))
;=>4075
  



