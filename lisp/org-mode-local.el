;;; -*- lexical-binding: t; -*-
;; functions to help with org-mode

(load "org")

;; (defun org-local-save ()
;;   "Keybinding to automatically perform exports on file save"
;;   (interactive)
;;   (save-buffer)
;;   (make-thread 'org-latex-export-to-pdf))
;; (define-key org-mode-map (kbd "<f10>") 'org-local-save)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

;; TODO check ditaa location and set org-ditaa-jar-path instead of
;; hard coding in customization system
