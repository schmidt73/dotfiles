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
    undo-tree))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; (add-to-list 'load-path "~/altprojects/llvm/src_root/utils/emacs") 

; EVIL Configuration
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-b") 'evil-visual-block)
(define-key evil-visual-state-map (kbd "C-b") 'evil-visual-block)

(define-key evil-normal-state-map (kbd "C-v") nil)
(define-key evil-visual-state-map (kbd "C-v") nil)
(define-key evil-motion-state-map "\C-v" nil)

; LLVM Stuff
; (require 'llvm-mode)
; (require 'tablegen-mode)
; 
; (add-to-list 'auto-mode-alist '("\\.ir\\'" . llvm-mode))
; (add-to-list 'auto-mode-alist '("\\.td\\'" . tablegen-mode))
 
; General CONFIG
(global-linum-mode 1) ; line numbers

(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))

(setq linum-format "%4d | ")

(setq tab-width 4) ; or any other preferred value
(setq-default indent-tabs-mode nil) ; indent spaces

(defun display-on-side (buffer &optional not-this-window frame)
  (let* ((window (or (minibuffer-selected-window)
                     (selected-window)))
         (display-buffer-function nil)
         (pop-up-windows nil))
    (with-selected-window (or window (error "display-on-side"))
      (when (one-window-p t)
        (split-window-horizontally))
      (display-buffer buffer not-this-window frame))))

(setq display-buffer-function 'display-on-side)
