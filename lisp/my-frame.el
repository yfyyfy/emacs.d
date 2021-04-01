(defun my-frame-set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(50))))

(defun my-frame-set-frame ()
  (interactive)
  (let* ((repeat t)
	 (n0 0)
	 (n1 (1- (length (display-monitor-attributes-list))))
	 (font-hL 150)
	 (font-hS 100))
    (while repeat
      (let ((key (read-event)))
	(cond ((eq key ?i)
	       (set-face-attribute 'default nil :height font-hL)
	       (my-frame-modify-frame-geometry n0)
	       (message "Reset to initial settings."))
	      ((eq key ?j)
	       (set-face-attribute 'default nil :height font-hL)
	       (my-frame-modify-frame-geometry n1)
	       (message "Reset to initial settings."))
	      ((eq key ?k)
	       (set-face-attribute 'default nil :height font-hS)
	       (my-frame-modify-frame-geometry n0)
	       (message "Reset to initial settings."))
	      ((eq key ?l)
	       (set-face-attribute 'default nil :height font-hS)
	       (my-frame-modify-frame-geometry n1)
	       (message "Reset to initial settings."))
	      ((eq key ?-)
	       (my-frame-increment-face-attribute 'default :height -10 nil t)
	       (my-frame-modify-frame-geometry))
	      ((eq key ?=)
	       (my-frame-increment-face-attribute 'default :height +10 nil t)
	       (my-frame-modify-frame-geometry))
	      ((eq key ?+)
	       (my-frame-increment-face-attribute 'default :height +10 nil t)
	       (my-frame-modify-frame-geometry))
	      ((eq key ?a)
	       (set-frame-parameter nil 'left 0))
	      ((eq key ?e)
	       (set-frame-parameter nil 'left -1))
	      ((eq key ?<)
	       (set-frame-parameter nil 'top 0))
	      ((eq key ?>)
	       (set-frame-parameter nil 'top -1))
	      ((eq key 'left)
	       (my-frame-increment-frame-parameter nil 'width -1 t))
	      ((eq key 'S-left)
	       (my-frame-increment-frame-parameter nil 'width -10 t))
	      ((eq key 'right)
	       (my-frame-increment-frame-parameter nil 'width +1 t))
	      ((eq key 'S-right)
	       (my-frame-increment-frame-parameter nil 'width +10 t))
	      ((eq key 'up)
	       (my-frame-increment-frame-parameter nil 'height -1 t))
	      ((eq key 'down)
	       (my-frame-increment-frame-parameter nil 'height +1 t))
	      ((eq key ?q)
	       (setq repeat nil))))
      (if repeat
	  (progn
	    (clear-this-command-keys t)
	    (setq last-input-event nil))))
    ;; (when last-input-event
    ;;  (clear-this-command-keys t)
    ;;  (setq unread-command-events (list last-input-event)))
    ))

(defun my-frame-modify-frame-geometry (&optional display-id)
  (let* ((display-id (or display-id (my-frame-current-display-id)))
	 (geometry (cdr (assoc 'geometry (nth display-id (display-monitor-attributes-list)))))
	 (x (nth 0 geometry))
	 (y (nth 1 geometry))
	 (w (nth 2 geometry))
	 (h (nth 3 geometry))
	 (margin-w 40)
	 (margin-h (if (zerop display-id) 70 30)))
    (modify-frame-parameters nil `((left   . ,x)
				   (top    . ,y)
				   (width  . ,(/ (- w margin-w) (frame-char-width)))
				   (height . ,(/ (- h margin-h) (frame-char-height)))))))

(defun my-frame-current-display-id ()
  (let ((repeat t)
	(n -1)
	(list (display-monitor-attributes-list)))
    (while (and repeat list)
      (setq n (1+ n))
      (setq repeat (not (equal (frame-monitor-attributes) (car list))))
      (setq list (cdr list)))
    (if repeat 0 n))) ; repeat should not be zero. Only in case..

(defun my-frame-increment-frame-parameter (frame parameter value &optional message)
  (set-frame-parameter frame parameter
		       (+ (frame-parameter frame parameter) value))
  (if message
      (message "Frame %s: %d" parameter (frame-parameter frame parameter))))

(defun my-frame-increment-face-attribute (face attribute value &optional frame message)
  (set-face-attribute face frame attribute
		      (+ (face-attribute face attribute frame t) value))
  (if message
      (message "Face %s: %d" attribute (face-attribute face attribute frame t))))

(defvar my-frame-hide nil)
(defun my-show-or-hide-frame (&optional show)
  (interactive)
  (if (or show my-frame-hide)
      (progn
	(set-frame-parameter nil 'left my-frame-hide)
	(setq my-frame-hide nil))
    (setq my-frame-hide (frame-parameter nil 'left))
    (set-frame-parameter nil 'left (x-display-pixel-width))))

(provide 'my-frame)