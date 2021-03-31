;; Obsolete

;; use printf??
(defvar my-file-name-from-sendfilename-filename-win nil)
(defvar my-file-name-from-sendfilename-filename nil)
(defun my-file-name-from-sendfilename (&optional use-cache)
  (interactive)
  (if my-frame-hide
      (my-show-or-hide-frame))
  (let (filename-win filename)
    (setq filename-win
	  (with-temp-buffer
	    (insert-file-contents "~/.sendfilename")
	    (buffer-substring (point-min) (point-max))))
    (if (and use-cache
	     (equal my-file-name-from-sendfilename-filename-win
		    filename-win))
	;; Return cache
	my-file-name-from-sendfilename-filename
      ;; Cache variable
      (setq my-file-name-from-sendfilename-filename-win filename-win)
      ;; Format filename to retrieve
      (or (string-match "\"\\(.*\\)\\\"[\s\r\n]*$" filename-win)
	  (string-match "\\(.*\\) [\s\r\n]*$" filename-win))
      (setq filename-win (match-string 1 filename-win))
      (setq filename
	    (with-temp-buffer
	      (call-process "cygpath" nil (current-buffer) nil filename-win)
	      (buffer-substring (point-min) (point-max))))
      (string-match "\n$" filename)
      (setq filename (replace-match "" nil nil filename))
      ;; Cache variable and return filename
      (setq my-file-name-from-sendfilename-filename filename))))

(defun my-visit-file-sent-from-sendto ()
  (interactive)
  (switch-to-buffer (find-file-noselect (my-file-name-from-sendfilename))))
