;;; -*- lexical-binding: t; -*-
;; functions to help with org-mode

(defun org-local-save ()
  "Keybinding to automatically perform exports on file save"
  (interactive)
  (save-buffer)
  (org-latex-export-to-pdf))
(define-key org-mode-map (kbd "<f10>") #'org-local-save)
