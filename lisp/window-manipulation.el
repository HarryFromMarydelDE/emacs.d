;;; -*- lexical-binding: t; -*-
;; functions to create window configurations
(provide 'window-manipulation)

(defun ide-windows (&optional fullscreen)
  "Create a window configuration ideal for use as an IDE.
Divides the current window into three equal width windows, with
the left-most reserved for *Help* and *info*. If FULLSCREEN is
non-nil, set fullscreen window parameter to value, Return list of
windows created, from left to right."
  (interactive)
  (when fullscreen
    (set-frame-parameter nil 'fullscreen fullscreen))
  (let* ((window-list (evenly-split-window 3))
	 (left-window (nth 0 window-list))
	 ; not sure why this is in this spot, but it is
	 (right-window (nth 1 window-list)))
    (customize-set-variable
     'display-buffer-alist
     (cons `("\\*\\(Help\\|Info\\)\\*"
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

(defun evenly-split-window (splits
			    &optional orientation no-make-atom window-list)
  "Split a window into SPLITS equally sized windows.
Splits the current windows into SPLITS evenly sized windows. Any
left over characters are distributed to the left or top most
windows possible. If ORIENTATION is \"vertical\" windows are
split vertically, otherwise they are split horizontally. Returns
list of windows and leaves current window unchanged when
finished. Makes splits into an atomic window unless NO-MAKE-ATOM
is non-nil."
  (interactive "NNumber of windows: ")
  (when (< splits 1)
    (error "Must split into at least one window"))
  
  (let ((side 'right) ; side along which the split will take place
	(current-window (selected-window))
	get-size ; function to retrieve window's size
	new-size) ; size of new window
    (unless window-list
      (setq window-list (list current-window)))
    (if (= splits 1) ; return list of windows
	(progn
	  (unless no-make-atom
	    (when (cdr window-list)  ; can't make single windows atomic
	      (window-make-atom (window-parent (car window-list)))))
	  window-list)
      (if (eq orientation 'vertical) ; choose size function and side
				     ; if necessary
	  (progn (fset 'get-size 'window-total-height)
		 (setq side 'below))
	(fset 'get-size 'window-total-width))
      ;; calculate size of new window making sure any modulus goes to
      ;; current window. new-size is negative because we are specifying
      ;; new window's size instead of current window.
      (setq new-size (- (floor (get-size current-window 'floor) splits)))
      (setq window-list ; split window and add new window to window list
	    (append window-list
		    (list (split-window current-window new-size side))))
      ;; continue until done
      (evenly-split-window (1- splits) orientation no-make-atom
			   window-list))))

(defun move-buffer-other-window ())

(defun close-all-buffers-by-mode ())
