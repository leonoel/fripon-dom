(ns fripon-dom.math)

(defn ratio [n d]
  #(/ (* % n) d))

(defn pct [p]
  (ratio p 100))

(def half (ratio 1 2))
(def third (ratio 1 3))

(defn barycenter [& points]
  (loop [total-weight 0
         total-position 0
         points (partition 2 points)]
    (if (seq points)
      (let [[[position weight] & points] points]
        (recur (+ total-weight weight) (+ total-position (* weight position)) points))
      (/ total-position total-weight))))

(defn center
  ([p] p)
  ([p1 p2] (/ (+ p1 p2) 2))
  ([p1 p2 & ps] (/ (apply + p1 p2 ps) (+ 2 (count ps)))))

(defn floor [x]
  ((.-floor js/Math) x))

(defn ceil [x] 
  ((.-ceil js/Math) x))

(defn sqrt [x]
  ((.-sqrt js/Math) x))

(defn distance [a b]
  ((.-abs js/Math) (- a b)))

(defn half-diff [x y]
  (/ (- x y) 2))

(defn avg [a]
  (let [l (.-length a)]
    (loop [i 0
           s 0]
      (if (= i l) (/ s l) (recur (inc i) (+ s (aget a i)))))))

