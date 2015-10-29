;;; emacsrc.el --- Emacs Common Configuration

;; Copyright (C) 2001  Dast <cfy1@cs.msstate.edu>
;; Copyright (C) 2015  Chris Young <chris.young05@gmail.com>

;; Author: Dast <cfy1@cs.msstate.edu>
;; Keywords: lisp

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

;; This file holds configuration common to all hosts

;;; Code:

(defun elisp-set-hjkl-keys ()
  "Take a mode key map and add hjkl VI-like keys to it"
  (interactive)
  (local-set-key "j" 'next-line)
  (local-set-key "k" 'previous-line)
  (local-set-key "h" 'backward-char)
  (local-set-key "l" 'forward-char))

(tool-bar-mode -1)
(show-paren-mode t)
(setq next-line-add-newlines nil)
(column-number-mode t)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(setq next-line-add-newlines nil)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(setq display-time-format "%l:%M")
(setq display-time-day-and-date t)
(display-time-mode t)

(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

(require 'tramp)

;; Make things a little more readable
(require 'hl-line)
(set-face-background hl-line-face "gray13")

;; 2013-09-03:
;; http://stackoverflow.com/questions/3216081/integrate-emacs-copy-paste-with-system-copy-paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;(set-language-environment "latin-1")
;(set-terminal-coding-system 'latin-1)

(setenv "ORGANIZATION" "Dast <cfy1@cs.msstate.edu>")
(add-hook 'find-file-hooks 'auto-insert)

; 2015-09-24: Added a load of standard generic-x
(require 'generic-x)


;-------------------------
; ansi-color stuff
;-------------------------

(require 'ansi-color)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;-------------------------
; escreen stuff
;-------------------------

(require 'escreen)
(escreen-install) ;; need to move this to a :after action in el-get

(add-hook 'escreen-goto-screen-hook
          'escreen-enable-number-mode-if-more-than-one-screen)

;-------------------------
; boxquote stuff
;-------------------------

(require 'boxquote)
(autoload 'boxquote-region "boxquote" nil t)
(autoload 'boxquote-title "boxquote" nil t)
(autoload 'boxquote-yank "boxquote" nil t)
(autoload 'boxquote-defun "boxquote" nil t)
(autoload 'boxquote-describe-key "boxquote" nil t)

;-------------------------
; global keys
;-------------------------

(global-set-key "ra" 'append-to-register)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'backward-delete-char-untabify)
(global-set-key [C-tab] 'other-window)
(global-set-key [f4] 'calendar)
(global-set-key [f5] 'view-mode)
(global-set-key "g" 'goto-line)

(server-start)

(defadvice find-file-read-only (after turn-on-view-mode activate)
  "Advise find-file-read-only to turn on view mode"
  (view-mode 1)
)

;-------------------------
; view mode stuff
;-------------------------
(add-hook 'view-mode-hook
	  (lambda ()
	    (elisp-set-hjkl-keys)
	    (define-key view-mode-map [backspace] 'backward-char)))


;-------------------------
; calendar mode stuff
;-------------------------

(add-hook 'calendar-load-hook
	  (lambda ()
	    (local-set-key "j" 'calendar-forward-week)
	    (local-set-key "k" 'calendar-backward-week)
	    (local-set-key "h" 'calendar-backward-day)
	    (local-set-key "l" 'calendar-forward-day)
	    (local-set-key "H" 'calendar-cursor-holidays)))
	    

;-------------------------
; custom mode stuff
;-------------------------

(add-hook 'custom-mode-hook 'elisp-set-hjkl-keys)
(add-hook 'tar-mode-hook 'elisp-set-hjkl-keys)

;-------------------------
; dired stuff
;-------------------------

;; 2015-10-29: Found this was necessary to get refresh in dired
;;             working (otherwise it throws an error related to flymake)
(require 'flymake)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "K" 'dired-do-kill-lines)
	    (local-set-key "k" 'dired-previous-line)
	    (local-set-key "j" 'dired-next-line)
	    (local-set-key "" 'dired-custom-find-file)
	    (local-set-key "P" 'dired-do-ps-print)))

(defun dired-custom-find-file ()
  "A replacement for find-file under dired that respects the dired
current directory."
  (interactive)
  (find-file (read-file-name "Find File: " (dired-current-directory)) t))

(defcustom dired-ps-print-buffer-with-faces nil
  "*If non-nil, `dired-ps-print' will print fonts, colors, and underlines."
  :type 'boolean
  :group 'dired)

(defun dired-do-ps-print (&optional arg)
  "Print the marked (or next ARG) files with ps-print.el.
If `dired-ps-print-buffer-with-faces' is non-nil, use
`ps-print-buffer-with-faces; otherwise, use `ps-print-buffer'."
  (interactive "P")
  (let ((files (dired-get-marked-files t arg)))
    (while files
      (let ((buffer (get-file-buffer (car files))))
	(save-excursion
	    (set-buffer (or buffer (find-file-noselect (car files))))
	      (save-restriction
		    (widen)
		        (if dired-ps-print-buffer-with-faces
			    (ps-print-buffer-with-faces)
			        (ps-print-buffer)))
	        ;; ps-print.el somehow alters the current buffer
	        ;; (or buffer (kill-buffer (current-buffer)))
	        (or buffer (kill-buffer (get-file-buffer (car files))))))
      (setq files (cdr files)))))

; 2015-09-21: Added new library for sorting in dired
(require 'dired-sort)
(require 'dired-sort-menu)
(require 'dired-sort-menu+)

(grep-compute-defaults)
(defun dired-grep (pattern)
  "Run grep in the current directory the point is in when using dired."
  (interactive "MGrep for (regexp): ")
  (grep (concat grep-command pattern " -r " (dired-current-directory)))
  (other-window 1)
  (rename-buffer (concat "GREP: " pattern))
)

(defun dired-grep-source (pattern)
  "Run grep in the current directory the point is in when using dired only on .C, .CPP, .H, .ASM, .AS[ACMP]X, *.CS, *.PRC, *.SQL"
  (interactive "MGrep source files for (regexp): ")
  (grep (concat grep-command pattern " --include=\"*.[Pp][Rr][Cc]\" --include=\"*.[Ss][Qq][Ll]\" --include=\"*.[Aa][Ss][AaCcMmPp][Xx]\" --include=\"*.[Cc][Ss]\" --include=\"*.[cChH]\" --include=\"*.[CcHh][Pp][Pp]\" --include=\"*.ASM\" --include=\"*.asm\" --include=\"*.[Pp][Hh][Pp]\" --include=\"*.[Ii][Nn][Cc]\" --include=\"*.[Ii][Nn][Ff][Oo]\" --include=\"*.[Ii][Nn][Ss][Tt][Aa][Ll][Ll]\" --include=\"*.[Mm][Oo][Dd][Uu][Ll][Ee]\" --include=\"*.[Tt][Ee][Ss][Tt]\" -r " (dired-current-directory)))
  (other-window 1)
  (rename-buffer (concat "GREP: " pattern))
)

(defun dired-grep-marked-files (pattern)
  "Run grep on marked files"
  (interactive "MGrep marked files for (regexp): ")
  (grep (concat grep-command pattern " "
		(shadow-join (dired-get-marked-files) " " "\"")))
  (other-window 1)
  (rename-buffer (concat "GREP: " pattern))
)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "A" 'dired-grep-marked-files)
	    (local-set-key "E" 'dired-grep-source)))


;-------------------------
; info mode stuff
;-------------------------

(add-hook 'Info-mode-hook 'elisp-set-hjkl-keys)
(add-hook 'Info-mode-hook
	  (lambda ()
	    (local-set-key "/" 'search-forward-regexp)))

;-------------------------
; apropos mode stuff
;-------------------------
(add-hook 'apropos-mode-hook 'elisp-set-hjkl-keys)

;-------------------------
; C/C++/C# stuff
;-------------------------

(autoload 'align "align" nil t)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;(c-set-style "linux")
	    (hs-minor-mode t)
	    (local-set-key "cw" 'fixup-whitespace)
	    (local-set-key "cr" 'remote-compile)
	    (local-set-key "cc" 'comment-region)
	    (local-set-key "" 'compile)
	    (local-set-key "]" 'align)
	    (auto-fill-mode t)))

(add-hook 'compilation-mode-hook
	  (lambda ()
	    (local-set-key "ce" 'compile-goto-error)
	    (local-set-key "cr" 'remote-compile)
	    (local-set-key "" 'compile)
	    (local-set-key "cc" 'comment-region)))

(add-hook 'makefile-mode-hook
	  (lambda ()
	    (local-set-key "" 'compile)
	    (local-set-key "cr" 'remote-compile)))

(add-hook 'compilation-mode-hook 'elisp-set-hjkl-keys)

(defconst msti-c-style
  '((c-basic-offset . 8)
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
     (statement-cont . +)))
  "MSTI C/C++ Programming Style")


(c-add-style "MSTI" msti-c-style nil)

(require 'trtn-mode)

(require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;-------------------------
; ibuffer stuff
;-------------------------

(require 'ibuffer)

(autoload 'ibuffer "ibuffer" "ibuffer mode" t)
(global-set-key "" 'ibuffer)

(add-hook 'ibuffer-mode-hooks
	  (lambda()
	    (define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
	    (define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
	    (define-key ibuffer-mode-map (kbd "* !") 'ibuffer-unmark-all)
	    ))


;-------------------------
; sql stuff
;-------------------------

;; sql-mode
(setq auto-mode-alist
(append '(("\\.prc$" . sql-mode)) auto-mode-alist))

(setq auto-mode-alist
(append '(("\\.sql$" . sql-mode)) auto-mode-alist))

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))


;-------------------------
; CSS stuff
;-------------------------

; 2015-03-27: Added css color helper
(require 'mon-buffer-utils)
(require 'mon-css-color) 
(add-hook 'css-mode-hook  'css-color-turn-on-in-buffer)

;-------------------------
; Misc mode stuff
;-------------------------

;(require 'lnk-mode)
(autoload 'lnk-mode "lnk-mode" nil t)
;(require 'srec-mode)
(autoload 'srec-mode "srec-mode" nil t)

(autoload 'dayreport-mode "dayreport-mode" nil t)

(setq auto-mode-alist
      (append '(("\\.lnk\\'" . lnk-mode)
		("\\.i\\'" . lnk-mobade)
		("\\.bas\\'" . visual-basic-mode)
		("[UKZSN][DC][0-9]\\{6\\}\\.[0-9]\\{2\\}\\'" . srec-mode)
		("\\.AAX\\'" . srec-mode)
		("[UKZSN]A?T?-[0-9]\\{4\\}\\.[0-9]\\{2\\}\\'" . srec-mode)
;		("\\.cmd\\'" . bat-generic-mode) 
;		("\\.bat\\'" . bat-generic-mode) ; Eli says this is done automagically if (require 'generic-x)
;		("\\.inf\\'" . inf-generic-mode) ; Eli says this is done automagically if (require 'generic-x)
;		("\\.reg\\'" . reg-generic-mode) ; Eli says this is done automagically if (require 'generic-x)
		("dayreport\\'" . dayreport-mode)
		("mtables\\.[htx]\\'" . srec-mode)
		("\\.as[pc]x" . html-mode)
;		("\\.php\\'" . php-mode)
;		("\\.info\\'" . conf-mode)
		("\\.install\\'" . php-mode)
		("\\.module\\'" . php-mode)
		("\\.test\\'" . php-mode)
		) auto-mode-alist))


(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))
 
;-------------------------
; Java stuff
;-------------------------

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
	(call-process "javap" nil (current-buffer) nil "-verbose"
		      "-classpath" (file-name-directory file)
		      (file-name-sans-extension (file-name-nondirectory file)))
	(setq buffer-file-name file)
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(java-mode)
	(current-buffer))))
   ((javap-handler-real op args))))
 
(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
	 (cons 'javap-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))


;-------------------------
; Git/Magit stuff
;-------------------------
(setenv "GIT_SSL_NO_VERIFY" "true")
(setenv "SSH_ASKPASS" "ssh-askpass")
(autoload 'magit-status "magit" nil t)

;; Created for USM work PC with ~/log pointing to the directory where daylogs
;; are stored.
;; Updated for USM 2015
(fset 'do-startup
   [?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-x ?d ?l ?o ?g tab ?2 ?0 ?1 ?5 return return ?\C-\\ ?1])


;; Created for Ultravac 2011 in combination with terminus share
;(fset 'do-startup
;   [?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-\\ ?c ?\C-x ?d ?\C-a ?\C-k ?/ ?m ?n ?t ?/ ?T ?e ?r ?m ?i ?n ?u ?s ?/ ?c ?h ?r ?i ?s ?. ?y ?o ?u ?n ?g ?/ ?l ?o ?g ?/ ?2 ?0 ?1 ?1 ?/ return return])


(provide 'emacs-user-rc)
