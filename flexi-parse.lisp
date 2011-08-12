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

(defparameter *clause-types* '((:s :np :vp)
                               (:np :pronoun)
                               (:np :article :noun)
                               (:vp :verb)
                               (:vp :verb :np)))

(defvar *source-sentence*)

(defun rand-elt (seq)
  (elt seq (random (length seq))))

(defun candidate-word-clauses (word)
  (mapcar 'reverse
	  (remove word *words* :key 'first :test-not 'eq)))

(defun initial-clauses ()
  (mapcar 'candidate-word-clauses *source-sentence*))

(defun initial-bag ()
  (let ((clauses (initial-clauses)))
    (list :starts (coerce (append clauses '(())) 'vector)
	  :ends (coerce (cons () clauses) 'vector))))

(defun word-length (clause)
  (if (symbolp clause)
      1
      (reduce '+ (rest clause) :key 'word-length)))

(defun create-string (bag start-index subschema)
  (cond ((null subschema) (values nil t))
	((>= start-index (length (getf bag :starts))) (values nil nil))
	(t
	 (let ((candidates
		(remove (first subschema) (elt (getf bag :starts) start-index)
			:key 'first :test-not 'eq)))
	   (if candidates
	       (let* ((first (rand-elt candidates))
		      (next-start-index (+ start-index (word-length first))))
		 (multiple-value-bind (rest success)
		     (create-string bag next-start-index (rest subschema))
		   (if success
		       (values (cons first rest) t)
		       (values nil nil))))
	       (values nil nil))))))

(defun try-add-clause (bag)
  (let ((start-index (random (length (getf bag :starts))))
	(schema (rand-elt *clause-types*)))
    (multiple-value-bind (string success)
	(create-string bag start-index (rest schema))
      (if (and success
	       (not (find (cons (first schema) string)
			  (elt (getf bag :starts) start-index)
			  :test 'equal)))
	  (let ((clause (cons (first schema) string))
		(bag (list :starts (copy-seq (getf bag :starts))
			   :ends (copy-seq (getf bag :ends)))))
	    (push clause (elt (getf bag :starts) start-index))
	    (push clause (elt (getf bag :ends) (+ start-index
						  (word-length clause))))
	    (values bag t))
	  (values bag nil)))))

(defun try-add-clauses (bag n &optional (num-added 0))
  (if (zerop n)
      (values bag num-added)
      (multiple-value-bind (bag success)
	  (try-add-clause bag)
	(try-add-clauses bag (1- n) (+ num-added (if success 1 0))))))
