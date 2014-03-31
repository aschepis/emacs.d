(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/color-theme/themes")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")

(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; ===============================
;; Misc Modes
;; ===============================
(require 'pbcopy)
(require 'sql-transform)
(require 'protobuf-mode)
(require 'css-mode)
(require 'markdown-mode)

;; ===============================
;; projectile - Project Management
;; ===============================
(require 'flx-ido)
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(global-set-key (kbd "M-t") 'projectile-find-file)

;; ===============================
;; ruby-mode - Ruby Source Editing
;; ===============================
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
(setq ruby-deep-indent-paren nil)

;; dont add magic encoding for ruby files
(setq ruby-insert-encoding-magic-comment nil)

;; =======================================
;; multi-web-mode - Web Dev Source Editing
;; =======================================
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
                  (ruby-mode "<%" "%>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "html.erb" "js.erb" "css.erb" "erb" "eco" "jst.eco", "hbs"))
(multi-web-global-mode 1)

(defun my-html-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode nil))
(add-hook 'html-mode-hook 'my-html-mode-hook)

;; =======================================
;; protobuf-mode - Protobuf Source Editing
;; =======================================
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; ========================
;; TRAMP Mode Configuration
;; ========================
(setq tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; =====================
;; Active Babel Languages
;; =====================
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (python . t)
   (java . t)
   (perl . t)
   (sh . t)
   (emacs-lisp . t)
   ))

;; ====================
;; Customized Variables
;; ====================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
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

;; ========================
;; Backup/Autosave Settings
;; ========================
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; ===============================
;; scss-mode - SASS Source Editing
;; https://github.com/antonj/scss-mode
;; ===============================
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
(add-to-list 'auto-mode-alist '("\\.\\(sass\\|scss\\)\\'" . scss-mode))

;; ===========================================
;; javascript-mode - Javascript Source Editing
;; ===========================================
(setq js-indent-level 2)

;; =========================================
;; coffee-mode - Coffeescript Source Editing
;; =========================================
(require 'coffee-mode)
(setq coffee-tab-width 2)

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

;;; markdown-mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; C/C++
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;;; auto complete mode
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-modes '(c++-mode sql-mode c-mode ruby-mode))

;; ;; yasnippets
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (setq yas/root-directory '("~/.emacs.d/snippets"
;;                            ))
;; (mapc 'yas/load-directory yas/root-directory)
;; (yas-global-mode 1)

;;; ============= SOLARIZED ================
(setq solarized-termcolors 256)
(load-theme 'solarized-dark)

;; FCI mode
(setq fci-rule-width 1)
(setq fci-rule-color "yellow")
(setq fci-rule-column 100)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; By an unknown contributor

;; (global-set-key "%" 'match-paren)

;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))


;; go-mode stuff

(defcustom gofmt-command "goimports"
  "The 'gofmt' command.  Some users may replace this with 'goimports'
from https://github.com/bradfitz/goimports."
  :type 'string
  :group 'go)

(add-hook 'before-save-hook #'gofmt-before-save)

;; shamelessly ripped commands from textmate.el
(defun aschepis-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.

A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun aschepis-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (aschepis-shift-right (* -1 (or arg 1))))


(global-set-key (kbd "M-[") 'aschepis-shift-left)
(global-set-key (kbd "M-]") 'aschepis-shift-right)
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "M-l") 'goto-line)

;; column mode
(setq column-number-mode t)

;; open new frames for new documents (via open, finder, etc)
(setq ns-pop-up-frames t)

;; C-k to kill whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Bind keys
(global-set-key (kbd "C-x C-g") 'rgrep)
(global-set-key (kbd "C-c C-f") 'gtags-find-tag)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default css-indent-offset 2)

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


(setq line-move-visual t)
