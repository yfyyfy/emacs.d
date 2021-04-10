(defun my-python-venv-add-venv-to-exec-path ()
  (let* ((dir (locate-dominating-file default-directory ".venv"))
	 (dot-venv (and dir (expand-file-name ".venv" dir)))
	 (venv-directory
	  (when dot-venv
	    (with-temp-buffer
	      (insert-file-contents dot-venv)
	      (car (split-string (buffer-string)))))))
    (if venv-directory
	(dolist (subdir'( "bin" "Scripts"))
	  (let ((bindir (expand-file-name
			 subdir
			 (expand-file-name venv-directory (file-name-directory dot-venv)))))
	    (when (file-exists-p bindir)
	      (setq-local exec-path (cons bindir exec-path))
	      (message "Add to exec-path locally: %s" bindir)))))))

(provide 'my-python-venv)
