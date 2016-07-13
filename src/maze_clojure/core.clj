(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec
    (for [row (range 0 size)]
      (vec
        (for [col (range 0 size)]
          {:row row :col col :visited? false :bottom? true :right? true})))))

(defn possible-neighbors [rooms row col]
  (let [top-room (get-in rooms [(dec row) col])
        bottom-room (get-in rooms [(inc row) col])
        left-room (get-in rooms [row (dec col)])
        right-room (get-in rooms [row (inc col)])]
    (filter
      (fn [room]
        (and (not (nil? room))
             (not (:visited? room))))
      [top-room bottom-room left-room right-room])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (> (count neighbors) 0)
      (rand-nth neighbors)
      nil)))

(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    (< new-row old-row) ;going up
    (assoc-in rooms [new-row new-col :bottom?] false)
    (> new-row old-row) ;going down
    (assoc-in rooms [old-row old-col :bottom?] false)
    (< new-col old-col) ;going left
    (assoc-in rooms [new-row new-col :right?] false)
    (> new-col old-col) ;going right
    (assoc-in rooms [old-row old-col :right?] false)))

(declare create-maze)

(defn create-maze-loop [rooms old-row old-col new-row new-col]
  (let [new-rooms (tear-down-wall rooms old-row old-col new-row new-col)
        new-rooms (create-maze new-rooms new-row new-col)]
    (if (= rooms new-rooms)
      rooms
      (create-maze-loop new-rooms old-row old-col new-row new-col))))
      
    
(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)]
    (if next-room
      (create-maze-loop rooms row col (:row next-room) (:col next-room))
      rooms)))
  
(defn -main []
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)]
    (doseq [row rooms]
      (print " _"))
    (println)
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (print (if (:bottom? room) "_" " "))
        (print (if (:right? room) "|" " ")))
      (println))))
