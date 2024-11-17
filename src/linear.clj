(ns linear)

(defn universal [obj op]
  (apply mapv op obj))

(defn v+ [& vectors]
  (universal vectors +))

(defn v- [& vectors]
  (universal vectors -))

(defn v* [& vectors]
  (universal vectors *))

(defn vd [& vectors]
  (universal vectors /))

(defn m+ [& matrices]
  (universal matrices v+))

(defn m- [& matrices]
  (universal matrices v-))

(defn m* [& matrices]
  (universal matrices v*))

(defn md [& matrices]
  (universal matrices vd))

(defn dot [& vectors]
  (if (empty? vectors)
    0
    (apply + (universal vectors *))))

(defn *s [vector scalars op]
  (let [pr (apply * scalars)]
    (mapv #(op % pr) vector)))

(defn v*s [vector & scalars]
    (*s vector scalars *))

(defn m*s [matrix & scalars]
    (*s matrix scalars v*s))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*v [matrix vector]
  (mapv (fn [row] (apply + (map * row vector))) matrix))

(defn m*m [& matrices]
  (reduce (fn [matrix1 matrix2]
            (let [matrix2_tr (transpose matrix2)]
              (mapv (partial m*v matrix2_tr) matrix1)))
          matrices))
