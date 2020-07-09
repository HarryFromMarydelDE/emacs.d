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
(load "web-editing")
(load "convenience-functions")
(load "org-mode-local")

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
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(js-indent-level 2)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (exec-path-from-shell rjsx-mode indium js2-mode nodejs-repl lorem-ipsum wucuo gitignore-mode markdown-mode vterm ssh-agency emmet-mode magit cider clojure-mode projectile paredit multiple-cursors web-server impatient-mode web-mode expand-region company sly)))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(tool-bar-mode nil)
 '(winner-mode t))

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
;; in DocView mode, reload docs when changed on disk
(add-hook 'doc-view-mode-hook   #'auto-revert-mode)
;; find multiple files
(load "dired-x")
;; override f command in dired to find all marked files
(define-key dired-mode-map (kbd "f") (lambda ()
                                       (interactive)
                                       (dired-do-find-marked-files t)
                                       (dired-unmark-all-marks)))

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
(global-set-key (kbd "<f10>") (lambda ()
                                (interactive)
                                (delete-trailing-whitespace)
                                (save-buffer)))
(global-set-key (kbd "C-<f10>") 'kill-buffer-with-save)
(global-set-key (kbd "M-<f10>") 'write-file)

;; cutting and pasting, kill ring, cursor
(setq kill-do-not-save-duplicates t) ; no duplicate entries in kill ring
(cua-mode t) ; enable CUA key bindings (Ctrl-c = copy, etc)
(require 'multiple-cursors) ; what it says on the tin
(global-unset-key (kbd "C-<down-mouse-1>")) ; ctrl-mouse1 to add a cursor
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c c r") 'mc/edit-lines) ; every line of region
(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "<return>") nil))

;; development
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'gitignore-mode-hook 'flyspell-prog-mode)
;; git
(load "magit") ; magit-after-save-refresh-status available at start
(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "<f5>") 'magit-status)
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
;; TODO: bind space to change from marked to unmarked and visa-versa

;; web development
(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "M-r") 'web-mode-element-unwrap)
  (define-key web-mode-map (kbd "<f1>") 'web-mode-element-wrap))
;; impatient mode
(httpd-start) ; start http server
(setq imp-default-user-filters
      '((html-mode . nil)
	(web-mode . nil)
	(mhtml-mode . nil)
	(nxml-mode . nil)))
(add-hook 'web-mode-hook 'impatient-mode)
(add-hook 'css-mode-hook 'impatient-mode)
(add-hook 'js2-mode-hook 'impatient-mode)
;; emmet
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
;; major modes
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; preload files
(find-file-noselect "~/.emacs.d/init.el")
(find-file-noselect "~/")
(find-file-noselect "~/.emacs.d")
(find-file-noselect "~/School") ; school folder
;; personal fileshare folder for Netlify
(find-file-noselect "~/School/personal-fileshare")
 ; Web Fundamentals Build Week project folder
(find-file-noselect "~/School/Web Applications I/Applied JavaScript")

;; OS stuff
(exec-path-from-shell-initialize)

;; TODO: run every time window configuration changes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white"))))
 '(mc/cursor-bar-face ((t (:background "cyan" :height 1))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "cyan"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan")))))

; LocalWords:  fileshare LocalWords emacs noinform
