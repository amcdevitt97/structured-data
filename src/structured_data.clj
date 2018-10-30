(ns structured-data)

(defn do-a-thing [x]
  (let [twoTimes (+ x x)]
    (Math/pow twoTimes twoTimes)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z]v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bottom_x bottom_y] [top_x top_y]] rectangle]
  (- top_x bottom_x)))

(defn height [rectangle]
  (let [[[bottom_x bottom_y] [top_x top_y]] rectangle]
  (- top_y bottom_y)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bottom_x bottom_y] [top_x top_y]] rectangle [x y] point]
    (and (<= bottom_x x top_x) (<= bottom_y y top_y))))

(defn contains-rectangle? [outer inner]
  (let [[inner_bottom inner_top] inner]
    (and (contains-point? outer inner_bottom)
         (contains-point? outer inner_top))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
(let [second-element (fn [v] (get v 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (let [name (:name author)
        years (if (:birth-year author)
              (str " (" (:birth-year author) " - " (:death-year author) ")")
       )]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

;; TODO: Debug backslash issue with books->string

(defn books->string [books]
  (if (== (count books) 0)
      (str "No books.")
      (if (> (count books) 1)
;        (pr-str (count books) " books. " (map book->string books) ".")
;        (pr-str (count books) " book. " (map book->string books) "."))
        (str (count books) " books. " (apply str (map book->string books)) ".")
        (str (count books) " book. " (apply str (map book->string books)) "."))
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
