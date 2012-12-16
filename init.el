;; plugins 
(add-to-list 'load-path' "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/color-theme") 
(add-to-list 'load-path "~/.emacs.d/color-theme/themes") 
(add-to-list 'load-path "~/.emacs.d/helm")

;; Textmate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; Color theme
(require 'color-theme) 
(color-theme-initialize) 
(setq color-theme-is-global t) 
;;(color-theme-arjen)

(if window-system
    (color-theme-deep-blue)   ;; Emacs in own window
    (color-theme-dark-laptop) ;; Emacs in tty
)

;; multi-web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
		  (ruby-mode "<%" "%>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "html.erb" "js.erb" "css.erb" "erb"))
(multi-web-global-mode 1)

;; dont add magic encoding for ruby
(setq ruby-insert-encoding-magic-comment nil)

;; tramp mode config
(setq tramp-default-method "ssh")

;; Erlang mode
(setq load-path (cons  "/Users/adamwork/erlang/r15b01/lib/tools-2.6.7/emacs" load-path))
(setq erlang-root-dir "/Users/adamwork/erlang")
(setq exec-path (cons "/Users/adamwork/erlang/bin" exec-path))
(require 'erlang-start)


;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (python . t)
   (java . t)
   (perl . t)
   (sh . t)
   (emacs-lisp . t)
   ))
;;
;;(global-set-key (kbd "C-x f") 'find-name-dired)

;;
;;; Move the backup *~ files into a single backups directory instead of
;;; littering the filesystem with them. Also version the backups and onlyu
;;; keep a finite number of backup versions.
;; (require 'backup-dir)
;; (make-variable-buffer-local 'backup-inhibited)
;; (setq bkup-backup-directory-info
;;       '((t "~/.emacs.d/backups" ok-create full-path prepend-name)))
;; (setq delete-old-versions t
;;       kept-old-versions 1
;;       kept-new-versions 3
;;       version-control t)
;; (defun make-backup-file-name (FILE)
;;   (let ((dirname (concat "~/.emacs.d/backups"
;;                          (format-time-string "%y/%m/%d/"))))
;;     (if (not (file-exists-p dirname))
;;         (make-directory dirname t))
;;     (concat dirname (file-name-nondirectory FILE))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/.projects.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
