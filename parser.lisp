(defun working-append (xs x)
  "I do sometimes get annoyed by the default append behaviour in CL.
I get why its like that, but still."
  (append xs (list x)))

(defun split-by-space (string)
  "Split up a string by space.
I should probably generalise this function, but that can be a job for future Cam."
  (defun split-by-space-iter (string string-carry list-carry)
    "May God forgive me for this function..."
    (let ((head (subseq string 0 1))
	  (tail (subseq string 1)))
      
      (cond ((equal tail "") (working-append list-carry (concatenate 'string string-carry head)))
	    ((equal head " ") (split-by-space-iter tail
						   ""
						   (working-append list-carry string-carry)))
	    (t (split-by-space-iter tail
				    (concatenate 'string string-carry head)
				    list-carry)))))
  (if (= (length string) 0) nil
      (split-by-space-iter string "" nil)))
