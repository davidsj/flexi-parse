(defparameter *words* '((he :pronoun)
                        (she :pronoun)
                        (is :verb)
                        (was :verb)
                        (loves :verb)
                        (feeds :verb)
                        (a :article)
                        (the :article)
                        (cat :noun)
                        (food :noun)
                        (dog :noun)))

(defparameter *word-types* '(:pronoun :verb :article :noun))
(defparameter *clause-types* '((:s :np :vp)
                               (:np :pronoun)
                               (:np :article :noun)
                               (:vp :verb)
                               (:vp :verb :np)))

(defvar *source-sentence*)

;; Evolve

(defun evolve (&key (pop-size 1000)
               (pop (rand-pop pop-size))
               (gens 100)
               (elitism 750)
               (genesis 100))
  (if (zerop gens)
      (values pop (mapcar 'score-clause pop))
      (let* ((elite (remove-duplicates                        ; slow?
                     (subseq (sort pop '< :key 'score-clause) ; memoize score-clause
                             (- pop-size elitism))
                     :test 'equal))
             (clauses-indexed (clauses-indexed pop))
             (new (rand-pop genesis clauses-indexed)))
        (evolve :pop-size pop-size
                :pop (append new
                             (loop for i from 1 to
                                  (- pop-size (length elite) genesis)
                                collect
                                  (let ((parent (rand-elt pop)))
                                    (if (or (= (random 2) 1)
                                            (find (first parent)
                                                  *word-types*))
                                        (mutate-clause clauses-indexed
                                                       parent)
                                        (rand-elt (cddr parent)))))
                             elite)
                :gens (- gens 1)
                :elitism elitism))))

;; Gen

(defun rand-pop (size &optional (clauses (make-hash-table)))
  (if (zerop size)
      nil
      (cons (rand-clause clauses)
            (rand-pop (- size 1)))))

(defun rand-elt (seq)
  (elt seq (random (length seq))))

(defun clauses-indexed (clauses)
  (let ((ht (make-hash-table)))
    (dolist (clause clauses)
      (push clause (gethash (first clause) ht)))
    (let ((ht2 (make-hash-table)))
      (maphash (lambda (type clauses)
                 (setf (gethash type ht2)
                       (coerce clauses 'vector)))
               ht)
      ht2)))

(defun rand-clause (&optional clauses
                    (type
                     (rand-elt
                      (append *word-types*
                              (remove-duplicates
                               (mapcar 'first *clause-types*))))))
  (let ((candidate-clauses (if (= (random 2) 0)
                               (gethash type clauses)
                               #())))
    (if (not (zerop (length candidate-clauses)))
        (rand-elt candidate-clauses)
        (if (find type *word-types*)
            (rand-word type)
            (let* ((schemas (remove type *clause-types*
                                    :test-not 'eq
                                    :key 'first))
                   (schema (rand-elt schemas)))
              (list* (first schema)
                     (rand-range *source-sentence*)
                     (mapcar (lambda (type)
                               (rand-clause clauses type))
                             (rest schema))))))))

(defun rand-range (seq)
  (let ((start (random (+ (length seq) 1))))
    (list start (+ start
                   (random (+ (- (length seq) start) 1))))))

(defun rand-word (type)
  (list type
        (let ((start (random (length *source-sentence*))))
          (list start (1+ start)))
        (first (rand-elt *words*))))

;; Mutate

(defun mutate-clause (clauses clause)
  (let ((mutated (%mutate-clause% clauses clause)))
    (if (= (random 2) 0)
        mutated
        (mutate-clause clauses mutated))))

(defun %mutate-clause% (clauses clause)
  (destructuring-bind (type range &rest subclauses) clause
    (case (random 2)
      (0 (list* type (mutate-range range) subclauses))
      (1 (list* type range
                (mutate-subclauses clauses subclauses))))))

(defun mutate-subclauses (clauses subclauses)
  (let ((mutating-index (random (length subclauses))))
    (append (subseq subclauses 0 mutating-index)
            (list (mutate-subclause clauses
                                    (elt subclauses mutating-index)))
            (subseq subclauses (+ mutating-index 1)))))

(defun mutate-subclause (clauses subclause)
  (if (symbolp subclause)
      (first (rand-elt *words*))
      (if (= (random 2) 0)
          (rand-clause clauses (first subclause))
          (mutate-clause clauses subclause))))

(defun mutate-range (range)
  (destructuring-bind (start end) range
    (case (random 4)
      (0 (list (max (- start 1) 0) end))
      (1 (list (min (+ start 1) end) end))
      (2 (list start (max (- end 1) start)))
      (3 (list start (min (+ end 1) (length *source-sentence*)))))))

;; Score

(defun score-clause (clause)            ; memoize
  (if (find (first clause) *word-types*)
      (score-word clause)
      (score-complex-clause clause)))

(defun score-word (clause)
  (destructuring-bind (type (start end) word) clause
    (+
     ;; present in source-sentence's range
     (if (find word (subseq *source-sentence* start end))
         1 -9)
     ;; range <= 1 char
     (if (>= start (- end 1))
         0 -1)
     ;; word exists
     (if (find word *words* :key 'first)
         4 0)
     ;; correct part-of-speech
     (if (find (list word type) *words* :test 'equal)
         4 0))))

(defun score-complex-clause (clause)
  (destructuring-bind (type range &rest subclauses) clause
    (if (null subclauses)
        0
        (let ((sub (first subclauses)))
          (+
           ;; subclause score
           (score-clause sub)
           ;; range starts as late as possible
           (if (= (apply 'min (mapcar 'first (mapcar 'second
                                                     subclauses)))
                  (first range))
               0 -1)
           ;; range ends as soon as possible
           (if (= (apply 'max (mapcar 'second (mapcar 'second
                                                      subclauses)))
                  (second range))
               0 -1)
           ;; range contains subclause
           (if (contains-range-p range (second sub))
               0 -6)
           ;; recursive
           (score-complex-clause (list* type
                                        (list (second (second sub))
                                              (second range))
                                        (rest subclauses))))))))

(defun contains-range-p (range1 range2)
  (<= (first range1)
      (first range2)
      (second range2)
      (second range1)))