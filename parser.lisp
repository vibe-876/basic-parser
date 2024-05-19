(defun working-append (xs x)
  "I do sometimes get annoyed by the default append behaviour in CL.
I get why its like that, but still."
  (append xs (list x)))

(defun splitter (script)
  "Splits a string into some smaller sub-strings when spaces are found.
I should have probably written a generalised function instead of hard coding it to spaces, but I'm sure that it will be fine..."
  (defun splitter-iter (head carry script)
    (if (= (length script) 0)
        (let ((top (subseq script 0 1))
              (rest (subseq script 1)))

          (cond ((equal top "") carry)
                ((equal top " ") (splitter-iter ""
                                                (working-append carry head)
                                                rest))
                (t (splitter-iter (concatenate 'string head top)
                                  carry
                                  rest))))
        carry))
  (splitter-iter "" nil script))
