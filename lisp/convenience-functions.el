;;; -*- lexical-binding: t; -*-
(defun list-last-item (list)
  "Return the last item of a list.
Return the last item of LIST.
If LIST is nil, return nil."
  (car (last list)))
