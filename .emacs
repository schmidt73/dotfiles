(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   '("ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" default))
 '(elpy-shell-command-prefix-key "C-c C-e")
 '(elpy-syntax-check-command "flake8 --ignore E30")
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#383a42")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   '("~/Dropbox/org/mskcc/base-editing/starting_off.org" "~/Dropbox/org/mskcc/guidescan/lib_design.org" "~/Dropbox/org/mskcc/days.org" "~/Dropbox/org/ref/notes.org"))
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t)))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.95 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-noter-notes-search-path '("~/Dropbox/org/"))
 '(package-selected-packages
   '(cdlatex poly-R ess-r-mode ess iedit raku-mode ein yasnippet-snippets all-the-icons company-irony irony elpy julia-repl julia-mode js2-mode cider shackle helm-swoop poly-org polymode org-noter org-ref helm-bibtex markdown-mode yasnippet auctex parinfer evil-surround undo-tree smart-mode-line treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs company helm-projectile projectile doom-themes ace-window use-package))
 '(pdf-tools-enabled-hook nil)
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(raku-exec-path "rakudo")
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight semi-bold :height 143 :width normal)))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.


(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

(use-package hydra
  :ensure t)

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-homage-white)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config))

(use-package projectile
  :ensure t
  :bind (("M-p" . projectile-command-map)
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
  (setq company-idle-delay 0.1)
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
  (sml/setup 'respectful))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :config
  (evil-ex-define-cmd "q" 'kill-this-buffer)
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
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }"))
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

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-source-correlate-start-server t)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2021/bin/x86_64-linux/"))
  (setq exec-path (append exec-path '("/usr/local/texlive/2021/bin/x86_64-linux/")))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map (kbd "C-c C-c") 'hydra-tex-menu/body)))
  (defhydra hydra-tex-menu (:color blue)
    "
^Compile^             ^References^        ^Editing^               ^Marking^
^^^^^^^^^^^^^^^^-------------------------------------------------------------------
_c_: compile         _r_: reference      _m_: insert macro        _._: mark environment
_a_: compile-all     _i_: citation       _e_: create environment  _*_: mark section
^ ^                  ^ ^                 _f_: close environment   ^ ^
"
    ("i" helm-bibtex-with-local-bibliography)
    ("r" reftex-reference)
    ("m" TeX-insert-macro)
    ("c" TeX-command-master)
    ("e" LaTeX-environment)
    ("f" LaTeX-close-environment)
    ("a" TeX-command-run-all)
    ("." LaTeX-mark-environment)
    ("*" LaTeX-mark-section)))

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

;;;; org CONFIG
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-capture-templates
        '(("c" "Citation" plain (file "~/Dropbox/org/ref/master.bib")
           "\n%x\n")))
  (setq org-agenda-files '("~/Dropbox/org/mskcc/days.org" "~/Dropbox/org/ref/notes.org"))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-log-done nil)
  (setq org-log-into-drawer t)
  (setq org-hide-leading-stars t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "INPROGRESS(p)" "|" "DONE(d!)"))))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.95))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("CANCELLED" :foreground "blue" :weight bold)
                ("INPROGRESS" :foreground "yellow" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)))))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (async-shell-command (concat "okular " fpath))))
  (setq bibtex-completion-bibliography
      '("~/Dropbox/org/ref/master.bib"))
  (setq bibtex-completion-library-path
      '("~/Dropbox/org/ref/pdfs/")))
  
(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/org/ref/notes.org"
        org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib")
        org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/")
  (setq org-ref-open-pdf-function 'org-ref-open-pdf-at-point)
  (defun org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (call-process-shell-command (concat "okular " pdf-file "&") nil 0)
        (message "no pdf found for %s" key))))
  (bind-key "C-c g" 'org-ref-google-scholar-at-point org-mode-map))

(use-package org-noter
  :ensure t
  :config
  (setq org-noter-default-notes-file-names '("~/Dropbox/org/ref/notes.org"))
  (setq org-noter-notes-search-path '("~/Dropbox/org/ref/notes.org"))
  (setq org-noter-auto-save-last-location t))

(use-package poly-org
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;;; helm CONFIG
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("C-c h" . helm-apropos))
  :config
  (helm-mode))

(use-package helm-swoop
  :ensure t
  :bind (("C-s" . helm-swoop)
         ("M-p C-s" . helm-multi-swoop-projectile))
  :config
  (setq helm-swoop-split-window-function 'display-buffer))

(use-package shackle
  :ensure t
  :config
  (setq shackle-default-ratio 0.35)
  (setq shackle-rules
        '((("*helm*" "*Helm*" "*helm bibtex*")
           :regexp t :align 'below  :same nil :popup t)))
  (shackle-mode))

;;;; Clojure CONFIG
(use-package cider
  :ensure t
  :config (add-hook 'cider-popup-buffer-mode-hook (lambda () (evil-insert 1))))

(defun clojure-reload ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(reset-all)")
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

;;;; Javascript CONFIG
(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2))

;;;; Julia CONFIG
(use-package julia-mode
  :ensure t
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (setenv "PATH" (concat (getenv "PATH") ":/opt/julia-1.4.2/bin"))
  (setq exec-path (append exec-path '("/opt/julia-1.4.2/bin"))))

(use-package julia-repl
  :ensure t)

(defun julia-strip-repl-response (raw-response line)
  (substring raw-response
             (length line)
             (- (length raw-response) (length "julia> "))))

(defun julia-repl-send-line-and-read (line)
  (with-current-buffer (julia-repl-inferior-buffer)
    (let ((starting-pos (point)))
      (term-send-raw-string line)
      (term-send-raw-string "\t")
      (sleep-for 0.5)
      (let ((raw-response (buffer-substring-no-properties
                           starting-pos (length (buffer-string)))))
        (julia-strip-repl-response raw-response line)))))

;; Python CONFIG
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'current))

(defun activate-python-environment ()
  (interactive)
  (let* ((files (seq-remove
                 (lambda (fname) (equal "." (substring (file-name-nondirectory fname) 0 1)))
                 (directory-files "~/miniconda3/envs" t)))
         (selection
          (helm :sources (helm-build-sync-source "Select Python Environment" :candidates files)
                :buffer "*helm select python environment to activate*")))
    (pyvenv-activate selection)))

;; Perl CONFIG
(use-package raku-mode
  :ensure t)

;; C/C++ CONFIG
(use-package irony
  :ensure t
  :config 
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode))

(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(setq c-basic-offset 4)

;; R CONFIG
(use-package polymode
  :ensure t)

(use-package poly-R
  :ensure t
  :mode (("\\.Rmd\\'" . poly-markdown+r-mode)
         ("\\.Rmarkdown\\'" . poly-markdown+r-mode)))

(use-package ess
  :ensure t
  :bind (("C-c d" . ess-help)))

;; shell CONFIG

;;;; Sets up Polymode for Shell

(defun poly-shell-eval-region (beg end msg)
  (let ((proc (get-process "shell"))
        pbuf command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
        
    (setq pbuff (process-buffer proc))
    (setq command (concat (buffer-substring beg end) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point)))
       ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)))

(defun poly-shell-mode-setup ()
  (setq-local polymode-eval-region-function #'poly-shell-eval-region))

(add-hook 'sh-mode-hook #'poly-shell-mode-setup)

(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
        
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point)))
       ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step 
      (goto-char max)
      (next-line))))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

;; Font config
(use-package ligature
  :load-path "~/.emacs.d/ligature.el"
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

;;;; BLOG CONFIG
(defun new-blog-post (name)
  (interactive "MPost name: ")
  (let* ((posts-dir "/home/schmidt73/Desktop/blog/_posts/")
         (post-file (concat posts-dir (format-time-string "%Y-%m-%d") "-" name ".markdown")))
    (find-file post-file)
    (insert "new-blog-post")
    (evil-append-line 1)))

;;;; General CONFIG
(server-start)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "f" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; indent spaces

(setq vc-follow-symlinks t) ; follow symlinks to actual file

;; Fixes bug when opening Java files...
(add-to-list 'auto-mode-alist '("\\.java\\'"))
