(ns linear)

(defn universal [op vectors]
  (apply mapv op vectors))

(defn v+ [& vectors]
  (universal + vectors))

(defn v- [& vectors]
  (universal - vectors))

(defn v* [& vectors]
  (universal * vectors))

(defn vd [& vectors]
  (universal / vectors))

(defn m+ [& matrices]
  (universal v+ matrices))

(defn m- [& matrices]
  (universal v- matrices))

(defn m* [& matrices]
  (universal v* matrices))

(defn md [& matrices]
  (universal vd matrices))

(defn dot [& vectors]
  (if (empty? vectors) 0 (reduce + (apply mapv * vectors))))

(defn *s [op vector pr]
  (mapv (fn [x] (op x pr)) vector))

(defn v*s [vector & scalars]
  (let [pr (reduce * scalars)]
    (*s * vector pr)))

(defn m*s [matrix & scalars]
  (let [pr (reduce * scalars)]
    (*s v*s matrix pr)))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*v [matrix vector]
  (mapv (fn [string] (reduce + (map * string vector))) matrix))

(defn m*m [& matrices]
  (reduce (fn [matrix1 matrix2]
            (let [matrix2_tr (transpose matrix2)]
              (mapv (fn [string] (m*v matrix2_tr string)) matrix1)))
          matrices))
