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

(add-to-list 'load-path "~/altprojects/llvm/src_root/utils/emacs") 

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

; GIT/SVN/ETC Project Functions

(setq project-file-extensions
     '("*.c" "*.cc" "*.class" "*.clj" "*.cpp" "*.cs" "*.cxx" "*.el" 
       "*.go" "*.h" "*.java" "*.lua" "*.m" "*.m4" "*.pl" "*.po" "*.py"
       "*.rb" "*.sh" "*.sh" "*.swift" "*.vb" "*.vcxproj" "*.xcodeproj" "*.xml"
       "*.td" "*.inc" "*.make" "*.txt" "*.md" "*.tex" "*.ir" "*.s" "*.TXT"))

(grep-compute-defaults) ; Need this or rgrep don't work

(defun find-project-root (file &optional default)
  "Finds the nearest git root of file, returning default if none is found."
  (let*
      ((search-pred
        (lambda (dir) (member ".git" (directory-files dir))))
       (proot (locate-dominating-file file search-pred)))
    (if proot proot default)))

(defun project-find-name-file ()
  "Searches for a file in the current project using a wildcard. 
The current project is either the nearest root project directory 
or current directory if none is found."
  (interactive)
  (let* ((root (find-project-root default-directory default-directory))
         (query (read-string (format "Query (in %s): " root))))
    (find-name-dired root query)))

(defun project-rgrep ()
  " Searches for a file in current project using a regexp."
  (interactive)
  (let* ((root (find-project-root default-directory default-directory))
         (regexp (read-string (format "Regex (in %s): " root))))
    (rgrep regexp (mapconcat 'identity project-file-extensions " ") root)))

(defun project-cycle-file ()
  " Cycles through files with same name but different extensions "
  (interactive)
  (let* ((fname (file-name-sans-extension (buffer-file-name)))
         (fnames (mapcar (lambda (ext) (concat fname ext)) project-file-extensions)))
    (message "%s" fnames)))
    

(define-key ctl-x-map (kbd "p f") 'project-find-name-file)
(define-key ctl-x-map (kbd "p g") 'project-rgrep)

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
