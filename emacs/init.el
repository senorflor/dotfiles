(require 'cl)

;;; Slather with elisp
(load "package")
(package-initialize)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar my-packages '(
                   ;; General
                      auto-complete
                      org
                      rainbow-delimiters
                      undo-tree
                      yaml-mode

                   ;; Clojure
                      ac-cider
                      clojure-mode
                      clojurescript-mode
                      cider

                   ;; Go
                      go-mode

                   ;; Haskell
                      haskell-mode

                   ;; Javascript
                      jsx-mode

                   ;; Markdown
                      markdown-mode

                   ;; Project nav
                      projectile)
  "Packages required at launchtime")

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Initialize color-theme and load solarized
(require 'color-theme)
(color-theme-initialize)
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized" t)

;; global setup
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(setq tab-width 2
      standard-indent 2
      indent-tabs-mode nil)
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(delete-selection-mode t)

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Clojurescript/EDN highlighting
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
;;; cider config
(require 'ac-cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq nrepl-hide-special-buffers t)
(setq nrepl-log-messages t)
(setq cider-show-error-buffer 'only-in-repl)

;;; Javascript
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'js-custom)
;; React
(require 'react-snippets)
;; it's just JSX, don't overreact
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

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
(load-theme 'solarized t)
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

;;; Projectile everywhere
(require 'projectile)
(projectile-global-mode)
(put 'downcase-region 'disabled nil)
