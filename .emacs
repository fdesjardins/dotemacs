;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar packages
  '(auto-complete
    tabbar
    tabbar-ruler
    zenburn-theme
    clojure-mode
    cider
    js2-mode
    sws-mode
    jade-mode
    yasnippet))

(if (member nil (mapcar 'package-installed-p packages))
    (progn
      (package-refresh-contents)
      (dolist (p packages)
	(when (not (package-installed-p p))
	    (package-install p)))))

(require 'cl)
(require 'tramp)
(require 'jade-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid-Sans" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme

(require 'tabbar)

(setq default-theme 'zenburn)
;(setq default-theme 'solarized-dark)

(when (eq default-theme 'zenburn)
  (require 'zenburn-theme)
  (add-hook 
   'window-setup-hook
   (lambda ()
     (set-face-attribute 'tabbar-selected nil :background "#DCDCCC" :foreground "#3F3F3F")
     (set-face-attribute 'tabbar-unselected nil :background "#3F3F3F" :foreground "#DCDCCC")
     (set-face-attribute 'hl-line nil :background "gray27")
     (set-face-attribute 'region nil :background "gray27"))))
  
(when (eq default-theme 'solarized-dark)
  (require 'solarized)
  (load-theme 'solarized-dark)
  (add-hook 
   'window-setup-hook
   (lambda ()
     (set-face-attribute 'tabbar-selected nil :background "#DCDCCC" :foreground "#073642")
     (set-face-attribute 'tabbar-unselected nil :background "#073642" :foreground "#DCDCCC")
     (set-face-attribute 'hl-line nil :background "#073642")
     (set-face-attribute 'region nil :background "#073642"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

;; Latex-mode make function
(defun latexmk-keys ()
  (lambda nil (interactive) 
    (shell-command "make 2>1 > latex.log")))

;; Two tabbar tab groups: emacs, user
(defun my-tabbar-buffer-groups ()
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
	            ((eq major-mode 'dired-mode) "emacs")
		          (t "user"))))

;; Move to first character
(defun my-beginning-of-line (arg)
  (interactive "P")
  (if (and (looking-at "^") (= arg 1))
      (skip-chars-forward " \t")
    (move-beginning-of-line arg)))

;; Multiple keybindings
(defun global-set-keys (alist) nil
  (mapcar 
   (lambda (a) nil
     (setcar a (read-kbd-macro (car a)))
     (apply 'global-set-key a)) 
   alist))

(defun my-buffer-menu-files-only (arg)
  (interactive "P")
  (split-window-below (+ -3 (* -1 (length (buffer-list)))))
  (other-window 1)
  (buffer-menu 'files-only))

(defun my-buffer-menu (arg)
  (interactive "P")
  (split-window-below (+ -3 (* -1 (length (buffer-list)))))
  (other-window 1)
  (buffer-menu))

(defun my-buffer-menu-1-window ()
  (interactive)
  (let ((target-buffer (Buffer-menu-buffer t))
	(buffer-menu-buffer (buffer-name)))
    (delete-window)
    (switch-to-buffer target-buffer)
    (kill-buffer buffer-menu-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(add-hook 'latex-mode-hook 'latexmk-keys)
(add-hook 'jade-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'stylus-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'coffee-mode-hook (lambda () (setq tab-width 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(add-to-list 
 'auto-mode-alist 
 '("\\.js$" . js2-mode))

(require 'auto-complete)
(column-number-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(global-auto-complete-mode t)

;; Two tabbar tab groups: emacs, user
(when window-system
  (require 'tabbar-ruler)
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  (setq tabbar-ruler-global-tabbar t)
  (setq tabber-ruler-global-ruler t))

;; Interface
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Buffer-menu
(define-key Buffer-menu-mode-map (kbd "RET") 'my-buffer-menu-1-window)
(define-key Buffer-menu-mode-map [mouse-2] 'my-buffer-menu-1-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings

(global-set-keys 
 '(
   ("C-b" switch-to-buffer)
   ("C-l" my-buffer-menu-files-only)
   ("C-S-l" my-buffer-menu)
   ("C-o" find-file)
   ("C-f" occur)
   ("C-x m" make-directory)
   ("C-x C-m" latexmk-keys)
   ("C-x -" comment-region)
   ("C-x _" uncomment-region)
   ("C-<backspace>" backward-kill-word)
   ("C-<return>" newline-and-indent)
   ("M-<left>" tabbar-backward-tab)
   ("M-<right>" tabbar-forward-tab)
   ("M-l" linum-mode)
   ("M-S-<up>" windmove-up)
   ("M-S-<left>" windmove-left)
   ("M-S-<right>" windmove-right)
   ("M-S-<down>" windmove-down)
   ("C-a" my-beginning-of-line)
   ("<home>" my-beginning-of-line)
   ("<f5>" eval-region)))

;; Conditional bindings
(when window-system
  (global-set-keys 
   '(
     ("C-z" undo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq 
 inhibit-startup-screen t
 initial-scratch-message nil
 backup-directory-alist '(("." . "~/.emacs-backups")))

(kill-buffer "*scratch*")

;; More undo history
(setq 
 undo-strong-limit 150000
 undo-limit 100000)

(setq tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)
(setq uniquify-buffer-name-style (quote forward))

(setq tramp-default-mode "ssh")
