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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-acario-light t)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config))
(use-package projectile
  :ensure t
  :bind (("M-p" . projectile-m)
         ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode 1))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package company
  :ensure t
  :bind* (("M-/" . company-complete))
  :config
  (setq company-idle-delay 0)
  (global-company-mode))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package undo-tree
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-b") 'evil-visual-block)
  (define-key evil-normal-state-map (kbd "C-v") nil)
  (define-key evil-visual-state-map (kbd "C-v") nil)
  (define-key evil-motion-state-map "\C-v" nil)
  (evil-set-initial-state 'inferior-python-mode 'emacs))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

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

(use-package cider
  :ensure t)

(use-package tex
  :ensure auctex
  :config
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-log-done nil)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold)))))

(use-package julia-mode
  :ensure t
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (setenv "PATH" (concat (getenv "PATH") ":/opt/julia-1.4.2/bin"))
  (setq exec-path (append exec-path '("/opt/julia-1.4.2/bin"))))

;;;; helm CONFIG
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-browse-project)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil)))

(use-package helm-swoop
  :ensure t
  :bind ("C-s" . helm-swoop)
  :config
  (setq helm-swoop-split-window-function 'display-buffer))
 
(use-package shackle
  :ensure t
  :config
  (setq shackle-default-ratio 0.35)
  (setq shackle-rules
        '((("*helm*" "*Helm*") :regexp t :align 'below  :same nil :popup t)))
  (shackle-mode))

;;;; Python CONFIG
(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config (setq elpy-rpc-virtualenv-path 'current))

(defun activate-python-environment ()
  (interactive)
  (let* ((files (seq-remove
                 (lambda (fname) (equal "." (substring (file-name-nondirectory fname) 0 1)))
                 (directory-files "~/miniconda3/envs" t)))
         (selection
          (helm :sources (helm-build-sync-source "Select Python Environment" :candidates files)
                :buffer "*helm select python environment to activate*")))
    (pyvenv-activate selection)))

;;;; BLOG CONFIG
(defun new-blog-post (name)
  (interactive "MPost name: ")
  (let* ((posts-dir "/home/schmidt73/Desktop/blog/_posts/")
         (post-file (concat posts-dir (format-time-string "%Y-%m-%d") "-" name ".markdown")))
    (find-file post-file)
    (insert "new-blog-post")
    (evil-append-line 1)))

;;;; General CONFIG
(desktop-save-mode 1)
(server-start)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default)))
 '(elpy-shell-command-prefix-key "C-c C-e")
 '(elpy-syntax-check-command "flake8 --ignore E30")
 '(org-agenda-files (quote ("~/Dropbox/org/plan.org")))
 '(package-selected-packages
   (quote
    (all-the-icons doom-themes smart-mode-line solarized-theme treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs eglot-jl eglot julia-repl julia-mode helm-projectile projectile helm-swoop conda evil-surround helm-ls-git elpy counsel f ivy markdown-mode ein yasnippet-snippets auctex magit parinfer lispy paredit ace-window nord-theme zenburn-theme use-package helm evil cider))))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
