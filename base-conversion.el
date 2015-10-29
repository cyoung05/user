;;; base-conversion.el --- base conversion for integers

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <s.marshall@dcs.hull.ac.uk>
;; Keywords: numbers bases conversion
;; Version: 1.00

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This is hand written software.  Use it at your own risk.

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Simon Marshall (s.marshall@dcs.hull.ac.uk)

;;; User functions

(defun number-to-number (string base1 base2)
  "Return conversion of STRING in base BASE1 to string in base BASE2.
STRING may be a number or string.
See `number-to-decimal' and `decimal-to-number'."
  (decimal-to-number (number-to-decimal string base1) base2))

(defun number-to-decimal (string base)
  "Return decimal conversion of STRING in base BASE.
STRING may be a number or string.  Does range checking of STRING.
Deals with integers only."
  (let ((string (if (stringp string) string (number-to-string string))))
    (if (string-match "^-" string)
	(- (number-to-decimal (substring string 1) base))
      (let ((value 0) (index 0) (len (length string)))
	(while (< index len)
	  (setq value (+ (* value base) 
			 (digit-to-value (substring string index (1+ index))
					 base))
		index (1+ index)))
	value))))

(defun decimal-to-number (decimal base)
  "Return conversion of DECIMAL to string in base BASE.
DECIMAL may be a number or string.  There is no range checking of DECIMAL.
Deals with integers only."
  (let ((decimal (if (numberp decimal) decimal (string-to-number decimal))))
    (if (< decimal 0)
	(concat "-" (decimal-to-number (abs decimal) base))
      (let ((string "") (divisor 1) value)
	(while (/= decimal 0)
	  (setq value (% (/ decimal divisor) base)
		string (concat (value-to-digit value) string)
		decimal (- decimal (* value divisor))
		divisor (* divisor base)))
	string))))
 
;;; Other useful functions

(defun number-to-numbers (string base)
  "Return list of cons pairs of number STRING in base BASE.
The list comprises base-value cons pairs.  Bases are character, binary, octal,
decimal, and hexadecimal.
STRING may be a number or string."
  (let ((bases '(char 2 8 10 16)))
    (mapcar (function (lambda (b)
		(cons b (if (numberp b)
			    (number-to-number string base b)
			  (char-to-string (number-to-decimal string base))))))
	    bases)))

(defun hex-to-string (hex)
  "Convert arg HEX ascii to a one-character string.
HEX may be a hexadecimal number or string.
See `number-to-decimal' and `char-to-string'."
  (char-to-string (number-to-decimal hex 16)))

(defun string-to-hex (string)
  "Convert arg STRING to hexadecimal ascii.
HEX may be a hexadecimal number or string.
See `decimal-to-number' and `string-to-char'."
  (decimal-to-number (string-to-char string) 16))
 
;;; Work horse functions

(defun digit-to-value (digit base)
  "Return decimal value of string DIGIT in base BASE.
Alphabetic digits may be in upper or lower case.  Does range checking of DIGIT."
  (let* ((ascii (string-to-char digit))
	 (value (cond ((and (>= ascii ?0) (<= ascii ?9)) (- ascii ?0))
		      ((and (>= ascii ?a) (<= ascii ?z)) (+ 10 (- ascii ?a)))
		      ((and (>= ascii ?A) (<= ascii ?Z)) (+ 10 (- ascii ?A)))
		      (t (error (format "Unknown digit `%s'" digit))))))
    (if (< value base)
	value
      (error (format "Digit `%s' to big for base %d" digit base)))))

(defun value-to-digit (value)
  "Return string digit representing decimal VALUE.
Alphabetic digits are lower case.  There is no range checking of VALUE."
  (if (and (>= value 0) (<= value 9))
      (number-to-string value)
    (char-to-string (+ (- value 10) ?a))))

(provide 'base-conversion)

;;; base-conversion.el ends here