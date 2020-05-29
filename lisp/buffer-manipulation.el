(defun kill-buffer-with-save (&optional buffer-or-name)
  "Kill buffer, asking to save if modified.
Kill the buffer specified by BUFFER-OR-NAME.  The argument may be
a buffer or the name of an existing buffer.  Argument nil or
omitted means kill the current buffer. If the buffer has been
modified, offer to cancel or save the buffer. Return t if the
buffer is actually killed, nil otherwise."
  (interactive)
  (let* ((viewing-buffer (current-buffer)) ; the current buffer
	 (killing-buffer ; the buffer being killed
	  (if buffer-or-name ; get buffer, or if nil current buffer
	      (get-buffer buffer-or-name)
	    viewing-buffer))
	 abort) ; whether to abort
    (unless killing-buffer ; couldn't find buffer
      (error "Unable to find buffer '%s'" buffer-or-name))

    (when (buffer-modified-p killing-buffer)
      (set-buffer killing-buffer) ; temporarily make killing buffer current
      (pcase (let ((read-answer-short t))
	       (read-answer
		(format
		 "Buffer '%s' has been modified. Save? "
		 (buffer-name killing-buffer))
		'(("yes" ?y "save the buffer.")
		  ("no" ?n "kill the buffer without saving.")
		  ("abort" ?! "abort."))))
	("yes" (save-buffer))
	;; prevent kill-buffer from reasking for confirmation
	("no" (set-buffer-modified-p nil))
	("abort" (setq abort t)))
      (set-buffer viewing-buffer)) ; restore original current buffer

    (unless abort
      (kill-buffer killing-buffer))))
