;;; lnk-mode.el --- Major Mode for LNK files

;; Copyright (C) 2002  Dast <cfy1@cs.msstate.edu>

;; Author: Dast <cfy1@cs.msstate.edu>
;; Keywords: lisp, extensions
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

;; Derived from sample.el by StefanMonnier

;; This file provides a mode to deal with the output file format from
;; the American Automation linker called SUPERLNK.  These output files
;; typically have the extention .lnk and are flat text files.  This
;; linker was shipped as a part of the now unsupported EZ-Pro 2.1
;; development system.

;; This mode relies on the base-conversion package by Simon Marshall
;; <s.marshall@dcs.hull.ac.uk>.  You can find a copy of it at 
;; http://dast.freeshell.org/?section=emacs

;; To use this file, make sure you put it in your load-path.  Then you
;; can put the following line in your .emacs file.

;; (require 'lnk-mode)

;; This file does not modify auto-mode-alist to load the mode
;; automatically.  You may wish to set it to load for appropriate file
;; extensions.

;; Tested with GNU Emacs 21.2.1 on Win32.

;; You can find this file at
;; http://dast.freeshell.org/section/emacs/elisp

;;; Code:

(require 'base-conversion)

(defgroup lnk ()
  "LNK file group"
  :group 'languages)

(defface lnk-mode-keyword-face
  '((t (:foreground "cyan" :weight bold)))
  "Bold cyan"
  :group 'lnk)

(defface lnk-mode-section-face
  '((t (:foreground "lightsalmon")))
  "Light Salmon"
  :group 'lnk)

(defface lnk-mode-error-face
  '((t (:foreground "red" :weight bold)))
  "Bold Red"
  :group 'lnk)

(defface lnk-mode-group-report-face
  '((t (:foreground "Orchid1" :weight bold)))
  "Light Red"
  :group 'lnk)

(defface lnk-mode-group-face
  '((t (:foreground "Aquamarine")))
  "Aquamarine"
  :group 'lnk)

(defface lnk-mode-candidate-face
  '((t (:background "yellow")))
  "yellow background highlight"
  :group 'lnk)

(defvar lnk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'lnk-do-foo)
    map)
  "Keymap for `lnk-mode'.")

(defvar lnk-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `lnk-mode'.")

(defcustom lnk-error-regex "(e)"
  "Regex to match linker errors."
  :type 'string
  :group 'lnk)

(defcustom lnk-mode-hook nil
  "Normal hook run by `lnk-mode'."
  :type 'hook
  :group 'lnk)

(defvar lnk-font-lock-keywords
  '(
    ("\\(/BANK\\|/COM[0-9]\\|/CBAR\\)" . 'lnk-mode-keyword-face)
    ("\\(?:/BANK\\|/COM[0-9]\\),\\([0-9A-F]+\\)" . (1 font-lock-constant-face))
    ("\\(?:\\(?:/BANK\\|/COM[0-9]\\),[0-9A-F]+\\):\\([0-9A-Z_,()]+\\)" . (1 'lnk-mode-section-face))
    ("^(e).*$" . 'lnk-mode-error-face)
    ("^Common Area [0-9] Group, Physical Address = [0-9A-F]+ [(]1000[)]" . 'lnk-mode-group-face)
    ("^Common Area [0-9] Group, Physical Address = [0-9A-F]+ [(][^1]000[)]" . 'lnk-mode-error-face)
    ("^Bank Area Group, Physical Address = [0-9A-F]+ [(]1000[)]" . 'lnk-mode-group-face)
    ("^Bank Area Group, Physical Address = [0-9A-F]+ [(][^1]000[)]" . 'lnk-mode-error-face)
    )
  "Keyword highlighting specification for `lnk-mode'.")

(defvar lnk-report-alist ()
  "An alist that associates a bank address with the size of the code in it.")

;;
;;  (defvar lnk-imenu-generic-expression
;;    ...)

;;  (defvar lnk-outline-regexp
;;    ...)

(defun lnk-sum (&optional addr sz)
  "Sum up hex sizes for sections in bank at addr of size sz."
  (interactive)
  (let ((sum 0))
    (save-excursion
      (while (looking-at "^\t")
	(re-search-forward "[A-Z_0-9]+: [0-9A-Z]+ [(]\\([0-9A-F]*\\)[)].*$")
	(setq sum (+ sum (number-to-decimal (match-string-no-properties 1) 16)))
	(skip-chars-forward "\n")))
    sum
    )
  )

(defun lnk-decorate (&optional addr sz)
  "Find the size of the code in a bank and put in an overlay with this info."
  (interactive)
  (let ((sum (lnk-sum)) (ovr))
    (setq ovr (make-overlay (point) (+ (point) 1)))
    (overlay-put ovr 'before-string 
		 (format (concat "`--> BANK SIZE: %4X\n"
				 (if (< sum 4096)
				     "`--> REMAINING: %4X\n" "`-->      OVER: %4X\n"))
			 sum
			 (abs (- 4096 sum))))
    )
)

(defun lnk-investigate (addr &optional sz)
  "Add into the report alist details for the bank at addr of size sz."
  (progn
    (setq lnk-report-alist (assq-delete-all addr lnk-report-alist))
    (setq lnk-report-alist (cons (cons addr (- 4096 (lnk-sum))) lnk-report-alist)))
)

(defun lnk-for-each-bank (bank-regex action)
  "Run action for every bank in the file matched by bank-regex."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward bank-regex nil t)
;;      (sit-for 2)
      (forward-line)
      (funcall action (match-string-no-properties 1) (match-string-no-properties 2))
;;      (sit-for 2)
      ))
)

(defun lnk-decorate-buffer ()
  "Put overlays in the buffer that indicate bank size and remaining space."
  (interactive)
  (lnk-for-each-bank "Group, Physical Address = \\([0-9A-F]+\\).*[(]\\([0-9A-F]+\\)[)]$" 'lnk-decorate)
)

(defun lnk-compare-banks (first second)
  "Compare a two cons cells from the report alist to see if the first cell is \"less than\" the second."
  (< (cdr first) (cdr second))
)

(defun lnk-investigate-buffer ()
  "Fill the report alist with the sizes of all of the banks."
  (interactive)
  (setq lnk-report-alist ())
  (lnk-for-each-bank "Bank Area Group, Physical Address = \\([0-9A-F]+\\).*[(]\\([0-9A-F]+\\)[)]$"
		     'lnk-investigate)

  (setq lnk-report-alist (sort lnk-report-alist 'lnk-compare-banks))
)

(defun lnk-show-candidates (sz)
  "Show the banks of at least of size sz."
  (interactive "MSize: ")
  (save-excursion
    (with-output-to-temp-buffer "*Candidates*"
      (goto-char (point-min))
      (let ((size (number-to-decimal sz 16)))
	(dolist (cell lnk-report-alist)
	  (if (>= (cdr cell) size)
	      (progn
		(princ (format "Bank: %s  of size %s (%s)\n" (car cell)
			       (decimal-to-number (cdr cell) 16)
			       (cdr cell)))
		(highlight-phrase (car cell) "hi-yellow")
		))))))
  )

(defun lnk-sort-symbols ()
  "Sort the symbols defined and display them in a temp buffer"
  (interactive)
  (with-output-to-temp-buffer "*Sorted Symbols*"
    (let ( (cur-buf (current-buffer)) (beg) (end) (tmp))
      (goto-char (point-min))
      (re-search-forward "^ Symbol            Section/Type        Logical   Physical     Length$")
      (setq beg (line-beginning-position 2))
      (re-search-forward "^MODULES PROCESSED:$")
      (setq end (line-beginning-position -1))
      (setq tmp (buffer-substring-no-properties beg end))
      (with-temp-buffer
	(insert tmp)
	(goto-char (point-min))
	(while (re-search-forward "\\(.*IO register[^\n]*\n\\)\\|\\(.*local constant[^\n]*\n\\)\\|\\(.*auto defined[^\n]*\n\\)" nil t)
	  (replace-match "" nil nil))
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9A-F]\\{4\\}\\)      \\([0-9A-F]\\{5\\}\\)" nil t)
	  (replace-match "0x\\1     0x\\2 " t))
	(sort-numeric-fields 4 (point-min) (point-max))
	(princ (buffer-substring-no-properties (point-min) (point-max)))
	))
))

(defun lnk-find-next-error ()
  "Find the next linker error in the current buffer."
  (interactive)
  (re-search-forward lnk-error-regex nil t)
)

 ;;;###autoload
(define-derived-mode lnk-mode fundamental-mode "LNK"
  "A major mode for editing LNK files.  Runs `lnk-mode-hook'."
  (set (make-local-variable 'comment-start) "; ")
  (set (make-local-variable 'comment-start-skip) ";+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(lnk-font-lock-keywords))
  (set (make-variable-buffer-local 'lnk-report-alist) ())
  (lnk-decorate-buffer)
  (lnk-investigate-buffer)
; (set (make-local-variable 'indent-line-function) 'lnk-indent-line)
;   (set (make-local-variable 'imenu-generic-expression)
;  lnk-imenu-generic-expression)
;   (set (make-local-variable 'outline-regexp) lnk-outline-regexp)

  (local-set-key [menu-bar lnk] (cons "LNK" (make-sparse-keymap "LNK")))
  (local-set-key [menu-bar lnk revert] '("Revert Buffer" . revert-buffer))
  (local-set-key [menu-bar lnk finderr] '("Find Next Error" . lnk-find-next-error))
  (local-set-key [menu-bar lnk show] '("Show Candidates" . lnk-show-candidates))

  (run-hooks 'lnk-mode-hook)
)

 ;;; Indentation

;; (defun lnk-indent-line ()
;;   "Indent current line of Lnk code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))

;; 	(indent (condition-case nil (max (lnk-calculate-indentation) 0)
;; 		  (error 0))))
;;     (if savep
;; 	(save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun lnk-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)


(provide 'lnk-mode)
;;; lnk-mode.el ends here
