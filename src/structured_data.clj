(ns structured-data)


(defn do-a-thing [x]
  (let [mm (+ x x)]
    (Math/pow mm mm)))

(defn spiff [v]
  (cond
   (> 3 (count v)) \?
   :else (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and (<= x1 x3 x2) (<= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[x3 y3] [x4 y4]] inner]
      (and (contains-point? outer (point x3 y3)) (contains-point? outer (point x4 y4))))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count (:authors book)) 1))

(defn add-author [book new-author]
    (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [x] (get x 1))]
    (map seconds collection)))

(defn titles [books]
 (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (apply concat (repeat n "*"))))

(defn toggle [a-set elem]
  (cond
   (not (contains? a-set elem)) (conj a-set elem)
   :else (disj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  ((:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
     (cond
      (>= 1 (count author)) (str (:name author))
      :else (str (:name author) " (" (:birth-year author)
       " - " (:death-year author) ")")))

(defn authors->string [authors]
     (let [auth (fn [m]
       (cond
        (>= 1 (count m)) (str (:name m))
          :else (str (:name m) " (" (:birth-year m)
           " - " (:death-year m) ")")))]
             (apply str (interpose ", " (map auth authors)))))

(defn book->string [book]
  (apply str (interpose "" [(:title book) ", written by " (authors->string (:authors book))])))

(defn books->string [books]
  (str (let [size (count books)]
     (cond
      (= size 0) (str "No books")
      (= size 1) (apply str(interpose ". " [(str (count books) " book")
              (apply str (interpose ". " (map book->string books)))]))
      :else (apply str(interpose ". " [(str (count books) " books")
              (apply str (interpose ". " (map book->string books)))]))
                  )) "."))

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))



(books-by-author china books)

(defn author-by-name [name authors2]
   (filter
    (fn [authors2]
      (contains? (set (map :name authors2)) name))
    authors))

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
