#!/usr/bin/env -S sbcl --script

(defun readin_file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun firstNumber (chars)
    (if (digit-char-p (first chars))
        (digit-char-p (first chars))
        (firstNumber (rest chars))))

(defun solveOne (input)
    (+ (* 10 (firstNumber (coerce input 'list)))
       (firstNumber (reverse (coerce input 'list)))))

(defun solveAll ()
    (map 'list 'solveOne (readin_file "input")))

(format t "~D~%"
    (apply '+ (solveAll)))
