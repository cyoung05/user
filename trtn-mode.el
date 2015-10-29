;;; trtn-mode.el --- A minor mode for Triton Systems z180 Terminal projects.

;; Copyright (C) 2003  Dast <cfy1@cs.msstate.edu>

;; Author: Dast <cfy1@cs.msstate.edu>
;; Keywords: tools, c, languages, files, local

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

;; Copy some code from which-func-mode? to always display current section?

;;; Code:

(require 'custom)
(require 'cc-mode)

(defgroup trtn-mode ()
  "file group"
  :group 'files)

(defcustom trtn-mode-default-customsoftwareproject ""
  "Default project for work comments in trtn-mode."
  :type 'string
  :group 'trtn-mode)


;(require 'which-func)?
; Here we could define which-func-format to be
; '( "[" trtn-mode-current-section ":" which-func-current "]" )
; to show [SECTION:FUNCTION]

;; (defun trtn-mode-add-br-comment (br)
;;   "Add a C comment for a given bug report BR."
;;   (interactive "MBug report: ")

;;   (insert (concat "/* BR " br " *   * CY "
;; 		  (format-time-string "%m/%Y")
;; 		  " */"
;; 	   ))
;; )

(defun trtn-mode-add-br-comment (br)
  "Add a comment for a given bug report BR."
  (interactive "MBug report: ")
  (let ((beg (point))
	(mid) (end))
    (insert (concat "BR " br " * "))
    (setq mid (point))
    (insert-before-markers (concat "  * CFY " (format-time-string "%m/%d/%Y")))
    (setq end (point))
    (goto-char mid)
    (comment-region beg end))
)

(defun trtn-mode-add-fr-comment (fr)
  "Add a comment for a given feature request number."
  (interactive "MFeature Request #: ")
  (let ((beg (point))
	(mid) (end))
    (insert (concat "FR " fr " * "))
    (setq mid (point))
    (insert-before-markers (concat "  * CFY " (format-time-string "%m/%d/%Y")))
    (setq end (point))
    (goto-char mid)
    (comment-region beg end))
)

(defun trtn-mode-add-x2-comment ()
  "Add a comment for the X2 Project."
  (interactive)
  (let ((beg (point))
	(mid) (end))
    (insert "X2 Project * ")
    (setq mid (point))
    (insert-before-markers (concat "  * CFY " (format-time-string "%m/%d/%Y")))
    (setq end (point))
    (goto-char mid)
    (comment-region beg end))
)

(defun trtn-mode-insert-comment ()
  "Insert a comment at the point." 
  (interactive)
  (let ((beg (point))
	(mid) (end))
    (insert (concat "CFY " (format-time-string "%m/%d/%Y") " * " ))
    (setq mid (point))
    (setq end (point))
    (goto-char mid)
    (comment-region beg end))
)

(defun trtn-mode-add-customsofwareproject-comment ()
  "Add a comment for the current Custom Software Group Project."
  (interactive)
  (let ((beg (point))
	(mid) (end))
    (insert (concat trtn-mode-default-customsoftwareproject " * "))
    (setq mid (point))
    (insert-before-markers (concat "  * CFY " (format-time-string "%m/%d/%Y")))
    (setq end (point))
    (goto-char mid)
    (comment-region beg end))
)

;; (defun trtn-mode-insert-comment ()
;;   "Insert a comment at the point." 
;;   (interactive)
;;   (let ((old-point))
;;     (insert (concat "/* CFY * " (format-time-string "%Y%m%d") " * "))
;;     (setq old-point (point))
;;     (insert " */")
;;     (goto-char old-point)
;;     )
;; )

;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 	    (make-variable-buffer-local 'tab-width)
;;	    (setq tab-width 4)))


(add-hook 'c-mode-hook
	  (lambda ()
	    (set-variable 'tab-width 4 t)
	    (auto-fill-mode -1)
	    ))


(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key [(control ?c) ?c ?b] 'trtn-mode-add-br-comment)
	    (local-set-key [(control ?c) ?c ?f] 'trtn-mode-add-fr-comment)
	    (local-set-key [(control ?c) ?c ?i] 'trtn-mode-insert-comment)
	    (local-set-key [(control ?c) ?c ?x] 'trtn-mode-add-x2-comment)
	    (local-set-key [(control ?c) ?c ?p] 'trtn-mode-add-customsofwareproject-comment)
	    ))

;; (add-hook 'c++-mode-hook
;; 	  (lambda ()
;; 	    (make-variable-buffer-local 'tab-width)
;; 	    (setq tab-width 4)
;;	    ))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (set-variable 'tab-width 4 t)
	    (auto-fill-mode -1)
	    ))


(defconst trtn-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     (brace-list-open)
     (brace-entry-open)
     (substatement-open after)
     (block-close . c-snug-do-while))
    (c-cleanup-list brace-else-brace)
    (c-offsets-alist
     (namespace-open . 0)
     (namespace-close . 0)
     (innamespace . 0)
     (inextern-lang . 0)
     (statement-block-intro . +)
     (knr-argdecl-intro . 0)
     (substatement-open . 0)
     (label . 0)
     (case-label . +)
     (statement-cont . +)))
  "Triton C Programming Style")

(defconst trtn-pc-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist
     (brace-list-open)
     (brace-entry-open)
     (substatement-open after)
     (block-close . c-snug-do-while))
    (c-cleanup-list brace-else-brace)
    (c-offsets-alist
     (namespace-open . 0)
     (namespace-close . 0)
     (innamespace . 0)
     (inextern-lang . 0)
     (statement-block-intro . +)
     (knr-argdecl-intro . 0)
     (substatement-open . 0)
     (label . 0)
     (case-label . 0)
     (statement-cont . +)))
  "Triton C PC Products Programming Style")

(c-add-style "TRTN" trtn-c-style nil)
(c-add-style "TRTNPC" trtn-pc-c-style nil)



(provide 'trtn-mode)
;;; trtn-mode.el ends here
