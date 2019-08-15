;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;             EMACS PACKAGE MANGER CONFIGURATION       ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)))

(use-package company
  :ensure t
  :bind (("TAB" . company-indent-or-complete-common))
  :config (global-company-mode))

(use-package undo-tree ; This is required to ensure we have a undo tree
  :ensure t)           ; (default of VIM) vs linear undo (default of EMACS)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-b") 'evil-visual-block)
  
  (define-key evil-normal-state-map (kbd "C-v") nil)
  (define-key evil-visual-state-map (kbd "C-v") nil)
  (define-key evil-motion-state-map "\C-v" nil))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (define-key elpy-mode-map (kbd "C-c C-e") nil)
  (custom-set-variables '(elpy-shell-command-prefix-key "C-c C-e"))
  (custom-set-variables '(elpy-syntax-check-command "flake8 --ignore E30")))

(use-package zenburn-theme
  :ensure t)

(use-package cider
  :ensure t)

;;;; EVIL Config

(evil-set-initial-state 'inferior-python-mode 'emacs)

;;;; Clojure CONFIG
(setq clojure-indent-style :always-indent)

(add-to-list 'exec-path "C:\\Program Files\\Lein")

(defun create-empty-clj (pname)
  (interactive "sClojure Project Name: ")
  (let ((cmd (concat "lein new empty " pname))
        (core-file (concat pname "/src/" pname "/core.clj")))
    (shell-command cmd)
    (find-file (expand-file-name core-file))
    (cider-jack-in '())))

;;;; General CONFIG

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "f" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(scroll-bar-mode 0)

(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; indent spaces

(defun kill-process (port) ; TODO: Get to work with Linux too
  (interactive "sProcess Port: ")
  (let* ((cmd (concat "netstat -a -n -o | find \"" port "\""))
         (pid (nth 4 (split-string (shell-command-to-string cmd)))))
    (when pid (shell-command (concat "taskkill /F /PID " pid)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-shell-command-prefix-key "C-c C-e")
 '(elpy-syntax-check-command "flake8 --ignore E30")
 '(package-selected-packages
   (quote
    (parinfer lispy paredit ace-window nord-theme zenburn-theme use-package helm evil elpy cider))))
(custom-set-faces)
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

