;; Simply the instruction set for this machine.
;; The car should be a function, and the cdr it's name.
(defvar *instruction-set* '((0 "ADD")
			    (1 "SUB")
			    (2 "ZRO")
			    (3 "JMP")
			    (4 "CND")))

(defun working-append (xs x)
  "I do sometimes get annoyed by the default append behaviour in CL.
I get why its like that, but still."
  (append xs (list x)))

(defun split-by-space (string)
  "Split up a string by space.
I should probably generalise this function, but that can be a job for future Cam.
Call this pre-processing."
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

(defun execute (script)
  (defun execute-line (line)
    (loop for instruction in *instruction-set*
	  do (if (equal line instruction) )))
  (loop for line in script
	do ))
