(ns projecteuler.tools)

; --------------------------------------String/Digits--------------------------------------------
; convert a number to vector of its digits
(defn digs [n]
  (loop [r n d []]
    (if (zero? r)
      d
      (recur (int (/ r 10)) (conj d (rem r 10))))))


; --------------------------------------Polynomials--------------------------------------------

(defn monomul [mon1 mon2]
  (list (* (nth mon1 0) (nth mon2 0)) (+ (nth mon1 1) (nth mon2 1))))

; ----------------------------------------

(defn mpmul [mono poly]
  (map #(monomul mono %) poly))

; -----------------------------------------

(defn polymul [poly1 poly2]
  (reduce into () (for [mono poly1]
                    (mpmul mono poly2))))

; -----------------------------------Number Theory-----------------------------------------------

(defn power-long ^long [^long n ^long m]
  (long (Math/pow n m)))

; -------------------------------------------

(defn power-big [n m]
  (bigint (Math/pow (bigint n) (bigint m))))

; --------------------------------------------
(defn Sqr? [n]
  (let [m (Math/sqrt n) l (int m)]
    (if (==  m l) l)))

; --------------------------------------------

(defn gcd [a b]
  (loop [c a r b]
    (let [m (rem c r)]
      (if (zero? m) r (recur r m)))))

; --------------------------------------------

(defn lcm [a b]
  (/  (* a b) (gcd a b)))

; -------------------------------------------

(defn Primes[^long n]
  (let [Max  (long (Math/sqrt n))
        q       (long (/ (inc n) 2))]
    (loop [k  3 p       (vec (cons 1 (cons 2  (range 3 (inc  n) 2))))]
      (if (<= k Max)
        (if-not (zero? (p (/ (inc k) 2)))
          (recur (+ k 2)  (reduce #(assoc % %2 0) p (range (/ (inc (* k k)) 2) (inc q) k)))
          (recur (+ k 2)  p))
        (rest (filter #(> % 0) p))))))

; ---------------------------------------------
(defn Prime?
  ([^long n]
  (let [pv (Primes (long (Math/sqrt n)))]
    (not-any? zero? (map #(rem n %) pv))))
  ([^long n PList]
  (let [pv (take-while #(< % (long (Math/sqrt n))))]
    (not-any? zero? (map #(rem n %) pv)))))

; ---------------------------------------------

(defn NextPrime [PrimeList]
  (loop [p (+ 2 (last PrimeList))]
    (if (some zero? (map #(mod p %) PrimeList))
      (recur (+ 2 p))
      p)))

; ------------------------------------------------

(defn Factor [^long n]
  (loop [k n p (Primes (long (Math/sqrt n))) f []]
    (if  (= 1 k)
      ;(sort < f)
      f
      (let [d (filter #(zero? (rem k %)) p)]
        (if (empty? d)
          (conj f k)
          (recur  (/ k  (reduce * d))  d (reduce conj f  d)))))))

; ------------------------------------------------

(defn MaxFactor [n]
  (loop [nDiv n pl [2 3]]
      (if (= 1 nDiv)
      (last pl)
      (recur
       (if (zero? (mod nDiv (last pl)))
         (/ nDiv (last pl))
         nDiv)
       (if (zero? (mod nDiv (last pl)))
          pl
          (conj pl (NextPrime pl)))))))

; ------------------------------------------------

(defn DivisorSum [n]
  (reduce * (map #(/ (dec (Math/pow (first %) (inc (second %)))) (dec (first %))) (seq (frequencies (Factor n))))))

; ------------------------------------------------
(defn rotates [n]
  (let [s (str n)
        l (count s)
        r (take l (partition l (inc l) (cycle s)))]
        (if (< l 10)
          (map #(Integer/parseInt (apply str %)) r)
          (map #(Long/valueOf (apply str %)) r))))

(defn circular? [p]
  (every? Prime? (rotates p)))
