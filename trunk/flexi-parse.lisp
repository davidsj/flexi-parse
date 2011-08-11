(defparameter *words* '((he :pronoun)
                        (is :verb)
                        (a :article)
                        (cat :noun)))

(defparameter *word-types* '(:pronoun :verb :article :noun))

(defvar *source-sentence*)

;; Evolve

(defun evolve (&key (pop-size 100) (pop (rand-pop pop-size)) (gens 10))
  (if (zerop gens)
      (values pop (mapcar 'score-tree pop))
      (evolve :pop-size pop-size
              :pop (append (subseq (sort pop '> :key 'score-tree) 0 20)
                           (rand-pop (- pop-size 20) pop))
              :gens (- gens 1))))

;; Gen

(defun rand-pop (size &optional trees)
  (if (zerop size)
      nil
      (cons (rand-tree trees)
            (rand-pop (- size 1)))))

(defun rand-elt (seq)
  (elt seq (random (length seq))))

(defun rand-tree (&optional trees)
  (case (random 1)
    (0 (rand-word))))

(defun rand-range (seq)
  (let ((start (random (+ (length seq) 1))))
    (list start (+ start
                   (random (+ (- (length seq) start) 1))))))

(defun rand-word ()
  (list (rand-elt *word-types*)
        (rand-range *source-sentence*)
        (first (rand-elt *words*))))

;; Mutate

(defun mutate-tree (tree)
  (let ((mutated
         (if (find (first tree) *word-types*)
             (mutate-word tree))))
    (if (= (random 2) 0)
        mutated
        (mutate-tree mutated))))

(defun mutate-word (tree)
  (destructuring-bind (type range word) tree
    (case (random 3)
      (0 (list (rand-elt *word-types*) range word))
      (1 (list type (mutate-range range) word))
      (2 (list type range (first (rand-elt *words*)))))))

(defun mutate-range (range)
  (destructuring-bind (start end) range
    (case (random 4)
      (0 (list (max (- start 1) 0) end))
      (1 (list (min (+ start 1) end) end))
      (2 (list start (max (- end 1) start)))
      (3 (list start (min (+ end 1) (length *source-sentence*)))))))

;; Score

(defun score-tree (tree)
  (if (find (first tree) *word-types*)
      (score-word tree)))

(defun score-word (tree)
  (destructuring-bind (type (start end) word) tree
    (+
     ;; present in source-sentence's range
     (if (find word (subseq *source-sentence* start end))
         0 -2)
     ;; word exists
     (if (find word *words* :key 'first)
         1 0)
     ;; correct part-of-speech
     (if (find (list word type) *words* :test 'equal)
         1 0))))
