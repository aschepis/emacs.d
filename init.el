;; plugins(add-to-list 'load-path' "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/color-theme/themes")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'sql-transform)
(require 'scala-mode)
(require 'protobuf-mode)
(require 'ruby-block)
(require 'css-mode)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
	  (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; Textmate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate")
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
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "html.erb" "js.erb" "css.erb" "erb" "eco" "jst.eco"))
(multi-web-global-mode 1)

(defun my-html-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode nil))
(add-hook 'html-mode-hook 'my-html-mode-hook)

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
 '(js-indent-level 2)
 '(org-agenda-files (quote ("~/.projects.org")))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Bind keys
(global-set-key (kbd "C-x C-g") 'rgrep)
(global-set-key (kbd "C-c C-f") 'gtags-find-tag)

;; scss-mode
;; https://github.com/antonj/scss-mode
(add-to-list 'load-path "~/.emacs.d/vendor/scss-mode")
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(defun my-scss-mode-hook ()
  "Hooks for SASS mode."
  (setq-default scss-compile-at-save nil)
  (setq css-index-offset 2)
  ;; (setq-default scss-output-directory "/dev/shm")
  ;; (flymake-mode-on)
)
(add-hook 'scss-mode-hook 'my-scss-mode-hook)

;; whitespace
(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

;; move faster up/down page with C-n and C-p
(global-set-key (kbd "C-n")
    (lambda () (interactive) (next-line 10)))
(global-set-key (kbd "C-p")
    (lambda () (interactive) (previous-line 10)))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; make M-backspace delete instead of kill
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; markdown mode
(require 'markdown-mode)

(setq js-indent-level 2)


;; git gutter mode
;; (require 'git-gutter)
;; (global-git-gutter-mode t)

;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 2)

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))


;; column mode
(setq column-number-mode t)

;; open new frames for new documents (via open, finder, etc)
(setq ns-pop-up-frames t)


;; C-k to kill whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-region* - Diff two regions
;;
;;  To compare two regions, select the first region
;; and run `diff-region`.  The region is now copied
;; to a seperate diff-ing buffer.  Next, navigate
;; to the next region in question (even in another file).
;; Mark the region and run `diff-region-now`, the diff
;; of the two regions will be displayed by ediff.
;;
;;  You can re-select the first region at any time
;; by re-calling `diff-region`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p)  ; there is a region
        (let (buf)
          (setq buf (get-buffer-create "*Diff-regionA*"))
          (save-current-buffer
            (set-buffer buf)
            (erase-buffer))
          (append-to-buffer buf (region-beginning) (region-end)))
        )
  (message "Now select other region to compare and run `diff-region-now`")
  )

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
        (let (bufa bufb)
          (setq bufa (get-buffer-create "*Diff-regionA*"))
          (setq bufb (get-buffer-create "*Diff-regionB*"))
          (save-current-buffer
            (set-buffer bufb)
            (erase-buffer))
          (append-to-buffer bufb (region-beginning) (region-end))
          (ediff-buffers bufa bufb))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/src/jshint-mode")
(require 'flymake-jshint)
(add-hook 'js-mode-hook
     (lambda () (flymake-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
                `(,(lambda ()
                     (and (string= (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>"
					     magic-mode-regexp-match-limit t)))
                  . objc-mode))
