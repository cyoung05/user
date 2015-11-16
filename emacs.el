(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(custom-enabled-themes (quote (tango-dark)))
 '(dired-listing-switches "-alh")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :foreground "#ffffff" :height 105 :width normal)))))

(make-directory "~/.emacs.d/el-get/el-get" t)
(make-directory "~/.emacs.d/user" t)
(make-directory "~/.emacs.d/el-get-user/recipes" t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/user")


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes, el-get-sources should only accept PLIST element
(setq el-get-sources
 '(
   (:name magit
	  :package-version '(magit . "2.1.0")
	  :after (progn
		   (global-set-key (kbd "C-x C-z") 'magit-status)))
   ;;()
   )
)

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get		; el-get is self-hosting
   escreen            	; screen for emacs, C-\ C-h
   php-mode-improved	; if you're into php...
;;   auto-complete	; complete as you type with overlays
;;   zencoding-mode	; http://www.emacswiki.org/emacs/ZenCoding
   color-theme		; nice looking emacs
   color-theme-tango	; check out color-theme-solarized
   csharp-mode
   mon-buffer-utils
   mon-css-color
   dired-sort
   dired-sort-map
   dired-sort-menu
   dired-sort-menu+
   hl-line+
   col-highlight
   crosshairs

;; magit source does not support emacs 24.3
;;   magit
))


(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;;(el-get 'sync)

;(setq my:el-get-packages (append my:el-get-packages (mapcar #'el-get-source-name el-get-sources)))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(require 'emacs-user-rc)

