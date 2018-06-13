; Packages

(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/undo-tree")

; EVIL Configuration

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(require 'evil)
(evil-mode 1)

; General CONFIG

(global-linum-mode t) ; line numbers

(setq tab-width 4) ; or any other preferred value
(setq-default indent-tabs-mode nil) ; indent spaces
