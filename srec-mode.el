;;; srec-mode.el --- Major Mode for Motorola S-format record files

;; Copyright (C) 2003  Dast <dast@freeshell.org>

;; Author: Dast <dast@freeshell.org>
;; Keywords: files, data, hardware, tools
;; URL: http://dast.freeshell.org/section/emacs/elisp
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a mode for viewing Motorola S-Record formatted
;; files.  The format is designed to allow printable ASCII
;; representations of program or data files to be loaded on various
;; hardware platforms.  Each line is a record with a specific format.
;; Details on the S-Record format can be found at the following
;; locations.  It has been modified to allow records to start with
;; either the letter S or T, upper or lower case.

;; http://www.cs.net/lucid/moto.htm
;; http://www.compusmart.ab.ca/rc/TechTips/srecords.html
;; http://www.amelek.gda.pl/avr/uisp/srecord.htm

;; This mode relies on the base-conversion package by Simon Marshall
;; <s.marshall@dcs.hull.ac.uk>.  You can find a copy of it at 
;; http://dast.freeshell.org/section/emacs/elisp

;; To use this file, make sure you put it in your load-path.  Then you
;; can put the following line in your .emacs file.

;; (require 'srec-mode)

;; This file does not modify auto-mode-alist to load the mode
;; automatically.  You may wish to set it to load for appropriate file
;; extensions.

;; Tested with GNU Emacs 21.2.1 on Win32.

;; You can find this file at
;; http://dast.freeshell.org/section/emacs/elisp

;;; Code:

(require 'base-conversion)

(defgroup srec ()
  "Mororola S Record file group"
  :group 'languages)

(defface srec-mode-type-face
  '((t (:foreground "cyan" :weight bold)))
  "Bold cyan"
  :group 'srec)

(defface srec-mode-length-face
  '((t (:foreground "Aquamarine")))
  "Aquamarine"
  :group 'srec)

(defface srec-mode-addr-face
  '((t (:foreground "lightsalmon")))
  "Light Salmon"
  :group 'srec)

(defface srec-mode-misc-face
  '((t (:foreground "Orchid1" :weight bold)))
  "Orchid1"
  :group 'srec)

(defface srec-mode-checksum-face
  '((t (:foreground "yellow")))
  "yellow"
  :group 'srec)

(defface srec-mode-error-face
  '((t (:foreground "red")))
  "red")

(defcustom srec-mode-hook nil
  "Normal hook run by `srec-mode'."
  :type 'hook
  :group 'srec)

(defcustom srec-mode-check-s0-len nil
  "If nil, do not check the length of S0 records, otherwise check them."
  :type 'boolean
  :group 'srec)

(defvar srec-font-lock-keywords
  '(
    ; bad records
    ("^[^SsT].*$" . 'srec-mode-error-face) ;anything that doesn't begin with an S or a T
    ("^[SsT].*[^[:xdigit:]\n].*$" . 'srec-mode-error-face) ;anything that begins with an S or a T but has non hex digits

    ; S0 records
    ("^\\([SsT]0\\)\\([[:xdigit:]]\\{2\\}\\)\\(.*\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-misc-face) (4 'srec-mode-checksum-face)))

 ; data records
    ("^\\([SsT]1\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{4\\}\\)[[:xdigit:]]\\{0,504\\}\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
    ("^\\([SsT]2\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{6\\}\\)[[:xdigit:]]\\{0,502\\}\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
    ("^\\([SsT]3\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{8\\}\\)[[:xdigit:]]\\{0,500\\}\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))

; terminator record with optional starting execution address, must be the last in the file?
;    ("^\\([SsT]7\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
;    ("^\\([SsT]8\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{6\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
;    ("^\\([SsT]9\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{4\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))

; hardcoded lengths
    ("^\\([SsT]7\\)\\(05\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
    ("^\\([SsT]8\\)\\(04\\)\\([[:xdigit:]]\\{6\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))
    ("^\\([SsT]9\\)\\(03\\)\\([[:xdigit:]]\\{4\\}\\)\\([[:xdigit:]]\\{2\\}\\)$" . ((1 'srec-mode-type-face) (2 'srec-mode-length-face) (3 'srec-mode-addr-face) (4 'srec-mode-checksum-face)))

    )
  "Keyword highlighting specification for `srec-mode'.")

;;;###autoload
(define-derived-mode srec-mode fundamental-mode "SREC"
  "A major mode for editing Motorola S-format Record files.  Runs `srec-mode-hook'."
  (set (make-local-variable 'font-lock-defaults)
       '(srec-font-lock-keywords))

  (toggle-truncate-lines)

  (local-set-key [menu-bar srec] (cons "SREC" (make-sparse-keymap "SREC")))
  (local-set-key [menu-bar srec check] '("Check Buffer Format" . srec-check-buffer))
  (local-set-key [menu-bar srec process] '("Process S2 Records" . srec-process-buffer))
  (local-set-key [menu-bar srec consec] '("Consecutive S2 Ranges" . srec-consecutive-ranges))
  ;(define-key srec-mode-map [menu-bar srec check] '("Check Buffer Format" . srec-check-buffer))

  (run-hooks 'srec-mode-hook))


(defun srec-check-region (beg end)
  "Check all of the S records in the given region."
  (interactive "r")
  (goto-char beg)
  (while (and
	  (<= (point) end)
	  (re-search-forward "^\\([SsT]\\([0-9]\\)\\(\\([[:xdigit:]]\\{2\\}\\)[[:xdigit:]]*\\)\\([[:xdigit:]]\\{2\\}\\)\\)$" nil t))
    (let ( (line (match-string-no-properties 1))
	   (type (match-string-no-properties 2))
	   (cblk (match-string-no-properties 3))
	   (leng (match-string-no-properties 4))
	   (csum (match-string-no-properties 5)) )

	; highlight bad lengths in yellow, bad checksums in blue
      (if (and
	   (or (not (string= type "0")) srec-mode-check-s0-len)
	   (not (srec-good-len-p cblk leng)))
	  (highlight-phrase line "hi-yellow") ; highlight entire line
	;else
	(if (not (srec-good-sum-p cblk csum))
		(highlight-phrase line "hi-blue") ; highlight entire line
	      )
	))
    (forward-line)
    )
  )

(defun srec-check-buffer ()
  "Check all of the S records in the given buffer."
  (interactive)
  (save-excursion
    (srec-check-region (point-min) (point-max))
    (princ "All done!\n")
))

(defun srec-good-sum-p (cblk csum)
  "Check the checksum in an S record."
  (let ((index 0)
	(num 0)
	(asum 0))

    ; calculate the checksum
    (while (setq index (string-match "\\([[:xdigit:]]\\{2\\}\\)" cblk index))
      (setq index (+ index 2))  ; 2 is the length of the match data
      (setq asum (+ asum (number-to-decimal (match-string 1 cblk) 16))))
    
; compare the calculated checksum against the checksum field
; logxor to do ones compliment, logand to get least significant bits
    (= (logand 255 (logxor asum 255)) (number-to-decimal csum 16))
))


(defun srec-good-len-p (cblk leng)
  "Check the length of an S record."
  ; (length of cblk) == (leng field in decimal * 2)
  (= (length cblk) (* 2 (string-to-number leng 16)))
)


(defun srec-process-region (beg end)
  "Process all of the S2 records in the given region."
  (interactive "r")
  (goto-char beg)
  (with-output-to-temp-buffer "*Processed S2 Records*"
    (while (and
	    (<= (point) end)
	    (re-search-forward "^[SsT]2[[:xdigit:]]\\{2\\}\\([[:xdigit:]]\\{6\\}\\)\\([[:xdigit:]]*\\)[[:xdigit:]]\\{2\\}$" nil t))
      (let ( (addr (match-string-no-properties 1))
	     (blk  (match-string-no-properties 2)) 
	     (index 0)
	     (byte)
	     (line ""))
	(princ (format "0x%s : " addr))
	
	(while (setq index (string-match "\\([[:xdigit:]]\\{2\\}\\)" blk index))
	  (setq index (+ index 2))  ; 2 is the length of the match data
	  (setq byte (format "%c" (string-to-number (match-string 1 blk) 16)))
	  (if (string-match "\\([^[:print:]]\\)" byte 0)
	      (setq byte "."))
	  (setq line (concat line byte)))
	(princ (format "%s\n" line))
	
)))
  (save-current-buffer
    (set-buffer "*Processed S2 Records*")
    (toggle-truncate-lines))
  )

(defun srec-process-buffer ()
  "Process all of the S2 records in the given buffer."
  (interactive)
  (save-excursion
    (srec-process-region (point-min) (point-max))
))

(defun srec-consecutive-ranges ()
  "Find all consecutive ranges of S2 records in the given buffer"
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (with-output-to-temp-buffer "*Consecutive S2 Record Ranges*"
      (re-search-forward "^[SsT]2\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{6\\}\\)[[:xdigit:]]*[[:xdigit:]]\\{2\\}$" nil t)
	; subtract 4 from the length to get rid of 3 character pairs for the address and 1 character pair for the checksum
      (let ( (prev-leng (- (string-to-number (match-string-no-properties 1) 16) 4))
	     (prev-addr (match-string-no-properties 2)) )

	(setq prev-addr-num (string-to-number prev-addr 16))
	(setq prev-end-addr (+ prev-addr-num prev-leng))

	; we begin at the first address we find
	(princ (format "%s - " prev-addr))

	(while (re-search-forward "^[SsT]2\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{6\\}\\)[[:xdigit:]]*[[:xdigit:]]\\{2\\}$" nil t)
	; subtract 4 from the length to get rid of 3 character pairs for the address and 1 character pair for the checksum
	  (let ( (leng (- (string-to-number (match-string-no-properties 1) 16) 4))
		 (addr (match-string-no-properties 2)) )
	    
	    (setq addr-num (string-to-number addr 16))
	    (setq end-addr (+ addr-num leng))

	; if we don't begin at the end of the previous record
;	    (princ (format "Addr: %d\tPrev Addr: %d\n" addr-num prev-end-addr))
	    (if (/= addr-num prev-end-addr)
		; then we have a new range
;		(princ (format "0%s\n%s - " (upcase (decimal-to-number prev-end-addr 16)) addr))
		(princ (format "0%X\n%s - " prev-end-addr addr))
		)

	    (setq prev-end-addr end-addr)

	    )) ; end while
;	(princ (format "0%s\n" (upcase (decimal-to-number prev-end-addr 16))))
	(princ (format "0%X\n" prev-end-addr))
	))))

(provide 'srec-mode)
;;; srec-mode.el ends here
