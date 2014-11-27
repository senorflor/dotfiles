;;; Slather with elisp
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-lisp
                      starter-kit-js
                      starter-kit-ruby

                   ;; General
                      auto-complete
                      color-theme-solarized
                      dash-at-point
                      dirtree
                      powerline
                      rainbow-delimiters
                      tree-mode
                      undo-tree
                      windata
                      yaml-mode
                      yasnippet

                   ;; Clojure
                      ac-cider
                      clojure-mode
                      clojurescript-mode
                      cider

                   ;; Go
                      go-mode

                   ;; Haskell
                      haskell-mode

                   ;; Java
                      malabar-mode

                   ;; Javascript
                      js2-mode
                      ac-js2
                      jsx-mode
                      react-snippets

                   ;; Markdown
                      markdown-mode

                   ;; Project nav
                      ack-and-a-half
                      projectile

                   ;; R
                      ess)
  "Packages required at launchtime")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; global setup
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; ido mode
(require 'ido)
(ido-mode t)

;;; powerline
(require 'powerline)

;;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;;; dirtree
(require 'dirtree)


;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Clojurescript/EDN highlighting
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
;;; cider config
(require 'ac-cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(add-to-list 'load-path "~/.emacs.d/malabar-1.5.0-SNAPSHOT/lisp/")
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/malabar-1.5.0-SNAPSHOT/lib/")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;;; Javascript
;; React
(require 'react-snippets)
;; it's just JSX, don't overreact
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

;;; org mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; auto-complete
(if (equal nil (boundp 'ac-dictionary-directories))
    (setq ac-dictionary-directories '()))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict" t)
(require 'auto-complete-config)
(ac-config-default)

;;; solarized
(load-theme 'solarized-dark t)
(defun endarken () (interactive) (load-theme 'solarized-dark t))
(defun enlighten () (interactive) (load-theme 'solarized-light t))
(global-set-key (kbd "C-c s") 'endarken)
(global-set-key (kbd "C-c C-M-s") 'enlighten)

;;; haskell
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook
          (lambda () (interactive)
            (local-set-key (kbd "TAB") (kbd "SPC SPC"))
            (kill-local-variable 'indent-line-function)
            (set (make-local-variable 'indent-line-function)
                                  'indent-relative)))
(add-to-list 'completion-ignored-extensions ".hi")

;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(defun el-get-sync-recipes (overlay)
  (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
         (recipe-files (file-expand-wildcards recipe-glob))
         (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
    (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
    (el-get 'sync (mapcar 'el-get-source-name recipes))))
(setq el-get-user-package-directory user-emacs-directory)
;; EL-GET SYNC OVERLAYS
(el-get-sync-recipes "el-get-haskell")
(el-get-sync-recipes "el-get-user")
;; CUSTOM FILE
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)



;;; dash-at-point
(require 'dash-at-point)
(global-set-key "\C-c\C-\M-d" 'dash-at-point)

;;; detab, and tab = 2 spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that, for
;; example.

;;; Line numbering
;;; (from http://www.emacswiki.org/LineNumbers)
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
     (propertize (format my-linum-format-string line-number) 'face 'linum))
(global-linum-mode 1)

;;; backup files in a backup directory:
(setq backup-directory-alist `(("." . "~/.saved.emacs")))
(setq backup-by-copying t)

;;; Projectile everywhere
(require 'projectile)
(projectile-global-mode)
(put 'downcase-region 'disabled nil)
