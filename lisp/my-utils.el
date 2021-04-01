(defun my-get-file-coding-systems (directory)
  (let ((default-directory directory)
	(files (directory-files directory)))
    (delq nil (mapcar (lambda (x) (if (not (file-directory-p x))
				      (with-temp-buffer
					(insert-file-contents x)
					(cons (expand-file-name x) buffer-file-coding-system))))
		      files))))
;; (my-get-file-coding-systems "/cygdrive/c/Program Files/Apache Group/Apache2/htdocs/erssmobile/apps/mobile/modules/view/templates/")
