;; Thanks to Andrew Stewart for providing this code on the Emacs wiki.
(defun web-mode-element-unwrap ()
  "Raise an element in the DOM, erasing its parent.
Just like `paredit-splice-sexp+' style. Be careful to have the
entire element selected or have the cursor on the opening
bracket."
  (interactive)
  (save-excursion
    (web-mode-element-parent)
    (web-mode-element-vanish 1)
    (back-to-indentation)))
