;;; -*- lexical-binding: t; -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "window-manipulation")
(load "buffer-manipulation")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 0.2)
 '(blink-cursor-mode t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(css-indent-offset 2)
 '(cua-normal-cursor-color "orchid")
 '(cua-remap-control-v nil)
 '(cursor-in-non-selected-windows nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-interval 1)
 '(display-time-mode t)
 '(frame-resize-pixelwise t)
 '(fringe-mode 0 nil (fringe))
 '(global-company-mode t)
 '(indicate-empty-lines t)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (magit cider clojure-mode projectile paredit multiple-cursors web-server impatient-mode html5-schema web-mode expand-region company sly)))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(tool-bar-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white"))))
 '(mc/cursor-bar-face ((t (:background "cyan" :height 1))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "cyan"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan")))))


;; expand selections more conveniently
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; modes
(electric-pair-mode) ;; automatically generate matching brackets, etc
(show-paren-mode) ;; highlight matching parenthesis
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook    #'paredit-mode)
(add-hook 'cider-mode-hook      #'paredit-mode)
(add-hook 'text-mode-hook       #'flyspell-mode)

;; sly
;; The SBCL binary and command-line arguments for sly
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

;; fix company-mode annoying behavior
(with-eval-after-load 'company
  ;; complete on tab instead of return
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key
    company-active-map (kbd "<tab>") 'company-complete-selection)
  ;; easy navigation inside company suggestions definitions
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; git
(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "<f5>") 'magit-status)

;; window navigation
;; f11 and f12 navigate left and right through windows
(global-set-key (kbd "<f11>") (lambda ()
				(interactive)
				(other-window -1)))
(global-set-key (kbd "<f12>") (lambda ()
				(interactive)
				(other-window 1)))
(global-set-key (kbd "C-<f11>") (lambda ()
				(interactive)
				(current-buffer-other-window -1)))
(global-set-key (kbd "C-<f12>") (lambda ()
				(interactive)
				(current-buffer-other-window 1)))

;; finding and saving files and buffers
;; f8 changes buffer f9 finds f10 saves Ctrl-10 kills the buffer
(global-set-key (kbd "<f8>") 'switch-to-buffer)
(global-set-key (kbd "<f9>") 'find-file)
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-<f10>") 'kill-buffer-with-save)

;; cutting and pasting, kill ring, cursor
(setq kill-do-not-save-duplicates t) ; no duplicate entries in kill ring
(cua-mode t) ; enable CUA key bindings (Ctrl-c = copy, etc)
(require 'multiple-cursors) ; what it says on the tin
(global-unset-key (kbd "C-<down-mouse-1>")) ; ctrl-mouse1 to add a cursor
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c c r") 'mc/edit-lines) ; every line of region

;; development
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; web development
;; impatient mode
(httpd-start) ; start http server
(setq imp-default-user-filters
      '((html-mode . nil)
	(web-mode . nil)
	(mhtml-mode . nil)
	(nxml-mode . nil)))
(add-hook 'web-mode-hook 'impatient-mode)
(add-hook 'css-mode-hook 'impatient-mode)
;; major mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
