; Adding Package Archives
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

; Packages
(setq package-list
  '(evil
    clj-refactor
    undo-tree
    company
    org
    magit
    cider
    web-mode
    helm
    oceanic-theme
    nord-theme
    ediprolog
    dracula-theme))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Helper Functions

(defun find-buffer-containing-name (name)
  (let ((pred
         (lambda (buf)
            (string-match-p (regexp-quote name) (buffer-name buf)))))
    (car (seq-filter pred (buffer-list)))))

; HELM settings

(require 'helm-config)

; Website Specific Configuration
(defun nrepl-refresh ()
  (interactive)
  (let ((old-buf (current-buffer))
        (cider-buf (find-buffer-containing-name "*cider-repl")))
    (set-buffer cider-buf)
    (goto-char (point-max))
    (insert "(clojure.tools.namespace.repl/refresh)")
    (cider-repl-return)
    (set-buffer old-buf)
    (message "Refreshed REPL.")))

(defun nrepl-reset ()
  (interactive)
  (let ((old-buf (current-buffer))
        (cider-buf (find-buffer-containing-name "*cider-repl")))
    (set-buffer cider-buf)
    (goto-char (point-max))
    (insert "(user/reset)")
    (cider-repl-return)
    (set-buffer old-buf)
    (message "Reset REPL.")))

; CIDER Configuration

(require 'cider)

(add-to-list 'exec-path "C:\\Program Files\\Lein")
(define-key cider-mode-map (kbd "C-c C-r") 'nrepl-reset)

; COMPANY Configuration
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

; EVIL Configuration
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-b") 'evil-visual-block)
(define-key evil-visual-state-map (kbd "C-b") 'evil-visual-block)

(define-key evil-normal-state-map (kbd "C-v") nil)
(define-key evil-visual-state-map (kbd "C-v") nil)
(define-key evil-motion-state-map "\C-v" nil)

; If you enter a mode whose name is in this 'evil-emacs-states-modes,
; Evil will toggle to Emacs state automatically.
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)

; Ensure we start in emacs state in these modes
(add-hook 'cider-popup-buffer-mode-hook 'evil-emacs-state)
(add-hook 'cider-repl-mode-hook 'evil-emacs-state)
(add-hook 'dired-mode-hook 'evil-emacs-state)
(add-hook 'cider-test-report-mode-hook 'evil-emacs-state)

(defun disable-evil-auto-indent ()
  (make-variable-buffer-local 'evil-auto-indent)
  (setq evil-auto-indent nil))

(add-hook 'org-mode-hook 'disable-evil-auto-indent)
(add-hook 'org-mode-hook 'evil-emacs-state)

; Prolog CONFIG
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(add-hook 'prolog-inferior-mode-hook 'evil-emacs-state)

; Clojure CONFIG
(setq clojure-indent-style :always-indent)

(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode)

; ORG Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; WEB Mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t))

(add-hook 'web-mode-hook  'my-web-mode-hook)

; Theme Settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; General CONFIG

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

(global-linum-mode 1) ; line numbers
(scroll-bar-mode -1) ; remove scroll bar

(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))

(setq linum-format "%4d | ")

(setq tab-width 4) ; or any other preferred value
(setq-default indent-tabs-mode nil) ; indent spaces

; WINDOWS

(defun kill-process (port)
  (interactive "sProcess Port: ")
  (let* ((cmd (concat "netstat -a -n -o | find \"" port "\""))
         (pid (nth 4 (split-string (shell-command-to-string cmd)))))
    (when pid (shell-command (concat "taskkill /F /PID " pid)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("12bacee81d067acf07dec4c867be541a04744a6ac6a39636de25a2c77e9b573c" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(package-selected-packages
   (quote
    (refactor-nrepl clj-refactor nrepl ediprolog prolog-mode web-mode helm company oceanic-theme nord-theme cider dracula-theme magit evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Office Code Pro D" :foundry "outline" :slant normal :weight normal :height 99 :width normal)))))
