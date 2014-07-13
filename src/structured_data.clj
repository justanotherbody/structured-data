(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
    ))

(defn spiff [v]
  (let [one (get v 0)
        three (get v 2)]
    (+ one three)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one, UNUSED, three] v]
    (+ one three)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1, _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_, y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1, y1] [x2, y2]] rectangle
        [px, py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left) (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        updated-authors (conj authors new-author)]
    (assoc book :authors updated-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (first (rest x)))]
    (map second collection)))

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
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [author-vector (:authors book)]
    (assoc book :authors (set author-vector))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors-vectors (map :authors books)]
    (apply clojure.set/union (map set authors-vectors))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years
        (fn [author] (if (contains? author :birth-year)
                       (str " ("(:birth-year author) " - " (:death-year author) ")")))]
  (str (:name author) (years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [nbooks (count books)
          bookword (if (= nbooks 1) "book" "books")
          preamble (str nbooks " " bookword)
          books-info (apply str (interpose ". " (map book->string books)))]
    (str preamble ". " books-info "."))))

(defn books-by-author [author books]
  (let [has-author? (fn [book] (contains? (:authors book) author))]
    (filter has-author? books)))

(defn author-by-name [name authors]
  (let [has-name? (fn [author] (= name (:name author)))
        authors-with-name (filter has-name? authors)]
    (if (empty? authors-with-name)
      nil
      (first authors-with-name))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
