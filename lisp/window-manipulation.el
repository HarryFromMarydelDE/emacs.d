;;; -*- lexical-binding: t; -*-
;; functions to create window configurations
(provide 'window-manipulation)

(defun ide-windows (&optional fullscreen)
  "Create a window configuration ideal for use as an IDE.
Divides the current window into three equal width windows, with
the left-most reserved for *Help* and *info*. If FULLSCREEN is
non-nil, set fullscreen window parameter to value. Return list of
windows created, from left to right."
  (interactive)
  (when fullscreen
    (set-frame-parameter nil 'fullscreen fullscreen))
  (set-cursor-color "white")
  (setq frame-title-format "%b")
  (let* ((window-list (evenly-split-window 3))
	 (left-window (nth 0 window-list))
	 (right-window (nth 2 window-list)))
    (customize-set-variable
     'display-buffer-alist
     (cons `("\\*\\(Help\\|Info\\)\\*"
	     ,(specific-windowf right-window)
	     (attached-window . ,right-window))
	   display-buffer-alist))
    window-list))

;; TODO - Make opening DocView mode in right window actually work
(defun document-windows (&optional fullscreen)
  "Create a window configuration ideal for use editing documents.
Divides the current window into two equal width windows, with
the left-most reserved DocView mode buffers. If FULLSCREEN is
non-nil, set fullscreen window parameter to value. Return list of
windows created, from left to right."
  (interactive)
  (when fullscreen
    (set-frame-parameter nil 'fullscreen fullscreen))
  (set-cursor-color "white")
  (setq frame-title-format "%b")
  (let* ((window-list (evenly-split-window 2))
	 (left-window (nth 0 window-list))
	 (right-window (nth 1 window-list)))
    (customize-set-variable
     'display-buffer-alist
     (cons `(,(derived-mode-p 'doc-view-mode)
             ,(specific-windowf right-window)
             (attached-window . ,right-window))
           display-buffer-alist))
    window-list))

(defun specific-windowf (window)
  "Return buffer display action function to display buffer in specific WINDOW on current frame."
  (let ((frame (selected-frame)))
    (lambda (buffer _)
      (when (eq frame (selected-frame))
	(set-window-buffer window buffer)
	window))))

(defun delete-stale-buffer-action-functions ()
  "Remove display buffer action functions that no longer point to live windows."
  (dolist (item display-buffer-alist)
    (dolist (subitem item)
      ;; only check cons cell attached window property
      (when (consp subitem)
	(when (eq (car subitem) 'attached-window)
	  ;; Remove item when subitem no longer refers to a live window
	  ;; in addition to preventing display-buffer-alist from accumulating
	  ;; stale cruft, this should also free the window for garbage
	  ;; collection
	  (unless (window-live-p (cdr subitem))
	    (customize-set-variable
	     'display-buffer-alist
	     (remove item display-buffer-alist))))))))
(add-hook
 'window-configuration-change-hook
 'delete-stale-buffer-action-functions)

(let (original-window) ; holds selected window when function is first called
  (defun evenly-split-window (splits
			      &optional orientation no-make-atom window-list)
    "Split a window into SPLITS equally sized windows.
Splits the current windows into SPLITS evenly sized windows. Any
left over characters are distributed to the left or top most
windows possible. If ORIENTATION is 'vertical windows are
split vertically, otherwise they are split horizontally. Returns
a list of the split windows ordered from left to right or top to
bottom and leaves the current window unchanged when finished.
Makes splits into an atomic window unless NO-MAKE-ATOM is
non-nil."
    (interactive "NNumber of windows: ")
    (when (< splits 1)
      (error "Must split into at least one window"))

    (let ((side 'right) ; side along which the split will take place
	  get-size ; function to retrieve window's size
	  (current-window (selected-window))
	  new-size) ; size of new window
      (unless window-list ; we start with a list of just the current window
	(setq window-list (list current-window))
	(setq original-window current-window))
      (if (= splits 1) ; no more splits to make
	  (progn
	    (unless no-make-atom
	      (when (cdr window-list) ; single windows are already atomic
		(window-make-atom (window-parent (car window-list)))))
	    (select-window original-window) ; change to original window
	    window-list) ; return final window list
	;; choose size function and side if necessary
	(if (eq orientation 'vertical)
            (progn
              (fset 'get-size 'window-total-height)
              (setq side 'below))
          (fset 'get-size 'window-total-width))
	;; calculate size of new window making sure any modulus goes to
	;; current window.
	(setq new-size (floor (get-size current-window 'floor) splits))
	(setq window-list ; split window and add new window to window list
	      (append window-list
		      (list (split-window current-window new-size side))))
	(select-window (list-last-item window-list))
	;; continue until done
	(evenly-split-window (1- splits) orientation no-make-atom
			     window-list)))))

(defun current-buffer-other-window (count)
  "Move current buffer to another window in the cyclic ordering of windows.
Displays the current buffer to the window which would have been
selected by other-window (COUNT). COUNT specifies the number of
windows to skip, starting with the selected window, before making
the selection.  If COUNT is positive, skip COUNT windows
forwards.  If COUNT is negative, skip -COUNT windows backwards.
COUNT zero means do not skip any window, so select the selected
window. Original buffer displays the next window in it's buffer
list. In an interactive call, COUNT is the numeric prefix
argument.  Returns nil."
  (interactive "p")
  ;; store information on original buffer status
  (let ((original-buffer (current-buffer))
	(original-point (point))
	new-window)
    (bury-buffer) ; remove buffer from current window
    (other-window count) ; change selected window
    (switch-to-buffer original-buffer) ; bring buffer to new window
    (goto-char original-point))) ; restore point

(defun close-all-buffers-by-mode ())
