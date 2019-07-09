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
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
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

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

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
  (define-key evil-motion-state-map "\C-v" nil)
 
  (add-hook 'dired-mode-hook 'evil-emacs-state)
  
  (defun disable-evil-auto-indent ()
    (make-variable-buffer-local 'evil-auto-indent)
    (setq evil-auto-indent nil))
  
  (add-hook 'org-mode-hook 'disable-evil-auto-indent)
  (add-hook 'org-mode-hook 'evil-emacs-state))

(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda))
  :config (setq org-log-done t))

(use-package zenburn-theme
  :ensure t)

;;;; Clojure CONFIG

(use-package cider
  :ensure t
  :hook ((cider-popup-buffer-mode-hook . evil-emacs-state)
         (cider-repl-mode-hook . evil-emacs-state)
         (cider-test-report-mode-hook . evil-emacs-state))
  :config (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode))

(setq clojure-indent-style :always-indent)

(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode)

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

(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; indent spaces

(defun kill-process (port) ; TODO: Get to work with Linux too
  (interactive "sProcess Port: ")
  (let* ((cmd (concat "netstat -a -n -o | find \"" port "\""))
         (pid (nth 4 (split-string (shell-command-to-string cmd)))))
    (when pid (shell-command (concat "taskkill /F /PID " pid)))))
