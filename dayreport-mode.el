;;; dayreport-mode.el --- Major Mode for Reporting daily activity

;; Copyright (C) 2003  Dast <cfy1@cs.msstate.edu>

;; Author: Dast <cfy1@cs.msstate.edu>
;; Keywords: files, local

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

;; 

;;; Code:

(defgroup dayreport ()
  "Dayreport file group"
  :group 'files)

(defcustom dayreport-mode-hook nil
  "Normal hook run by `dayreport-mode'."
  :type 'hook
  :group 'dayreport)

(defcustom dayreport-mode-default-project "admin"
  "Default project for work blocks in `dayreport-mode'."
  :type 'string
  :group 'dayreport)

; syntax table?

(defvar dayreport-font-lock-keywords
  '(
    ("^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" .
     ((1 'font-lock-builtin-face) (2 'font-lock-constant-face) (3 'font-lock-type-face)))
    ("^\\(\\[[^]]*\\]\\)\\(([^)]*)\\)" . ((1 'font-lock-function-name-face) (2 'font-lock-keyword-face)))
    )
  "Keyword highlighting specification for `dayreport-mode'.")

;;;###autoload
(define-derived-mode dayreport-mode fundamental-mode "RPRT"
  "A major mode for editing day report files.  Runs `dayreport-mode-hook'."
  (set (make-local-variable 'font-lock-defaults)
       '(dayreport-font-lock-keywords))

  (local-set-key [menu-bar day] (cons "DAY" (make-sparse-keymap "DAY")))
  (local-set-key [menu-bar day narrow-today] '("Goto Today" . dayreport-narrow-to-today))
  (local-set-key [menu-bar day narrow-date] '("Goto Date" . dayreport-narrow-to-date))
  (local-set-key [menu-bar day insert-work-block] '("Insert work block" . dayreport-insert-work-block))

  (local-set-key [(control ?c) ?g] 'dayreport-narrow-to-date)
  (local-set-key [(control ?x) ?w] 'dayreport-insert-work-block)

  ; add keybindings for calendar mode maybe?
  ; copy c-mode syntax table?
  ; call dayreport-narrow-to-today
  (auto-fill-mode t)
  (run-hooks 'dayreport-mode-hook))

;Functions:
; dayreport-current-day (returns current day, to figure next/prev day/week/year)
; dayreport-next-(day|week|year)
; dayreport-prev-(day|week|year)
; dayreport-digest
; dayreport-new-task-entry

(defun dayreport-transform-day (str)
  "Transform STR into a date formated for `dayreport-mode' functions."
  (if (string= str "today")
      (format-time-string "%Y%m%d" (current-time))
    str)
)

(defun dayreport-day-has-entry-p (date)
  "Check for a day entry for DATE, in the form of YYYYMMDD."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward date nil t)
      ))
)

(defun dayreport-narrow-to-date (date)
  "Narrow to the day entry for DATE, where DATE is in any form understod by `dayreport-transform-day'."
  (interactive "MEnter date: ")

  (setq datet (dayreport-transform-day date))

  (if (dayreport-day-has-entry-p datet)
      (progn
	(widen)
	(goto-char (point-min))
	(search-forward datet nil nil)
	(narrow-to-page))
    ;else 
    (if (y-or-n-p (format "Day %s has no entry!  Make one? " date ))
	(dayreport-add-entry datet)))
)

(defun dayreport-insert-work-block (project task)
  "Inserts a work block."
  (interactive "MProject: \nMTask: ")
  (progn
    (if (string= project "")
	(setq project dayreport-mode-default-project))
    (dayreport-narrow-to-today)
    (goto-char (point-max))
    (insert (format "\n\n[%s](%s)\n{\n\n\n}\n" project task))
    )
)


(defun dayreport-add-entry (date)
  "Add an entrty for DATE to the daylog, where DATE is in any form understood by `dayreport-transform-day'."
  (interactive "MEnter date: ")
  (setq datet (dayreport-transform-day date))

  (if (dayreport-day-has-entry-p datet)
      (error (format "Day %s already has an entry!" date))
    (progn ; else
      (widen)
      (goto-char (point-max))
      (insert "\n\n" datet "\n\n")
      (narrow-to-page)))
)

(defun dayreport-narrow-to-today ()
  "Narrow to day entry for today."
  (interactive)
  (dayreport-narrow-to-date "today")
)

(provide 'dayreport-mode)
;;; dayreport-mode.el ends here
