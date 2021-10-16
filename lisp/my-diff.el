;;;###autoload
(defun my-diff-buffers (buffer1 buffer2 &optional switches)
  ;; (interactive "bBuffer: \nbBuffer: ")
  (interactive "bBuffer: \nbBuffer: \nsSwitches: ")
  (let ((tempfile1 (make-temp-file "buffer-content-"))
	(tempfile2 (make-temp-file "buffer-content-")))
    (unwind-protect
	(progn
	  (with-current-buffer buffer1
	    (write-region nil nil tempfile1 nil 'nomessage))
	  (with-current-buffer buffer2
	    (write-region nil nil tempfile2 nil 'nomessage))
	  (diff tempfile1 tempfile2 switches t)
	  (sit-for 0))
      (progn
	(when (file-exists-p tempfile1)
	  (delete-file tempfile1))
	(when (file-exists-p tempfile2)
	  (delete-file tempfile2)))))
  nil)

(provide 'my-diff)
