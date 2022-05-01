(require 'f)
(require 'json)

(defun my-docker-tramp-get-docker-local-filename (filename)
  "Return the local filename that corresponds to FILENAME inside a docker container.

If no corresponding file is found, return 'not-found.
If FILENAME is not a remote docker filename, return 'not-docker."
  (if (equal (file-remote-p filename 'method) "docker")
      (let* ((container (file-remote-p filename 'host "docker"))
	     (local-filename (file-local-name filename))
	     (mounts (with-temp-buffer
		       (let ((ret-value (call-process "docker" nil t nil "inspect" "-f" "{{ json .Mounts }}" container)))
			 (when (eq ret-value 0)
			   (goto-char (point-min))
			   (json-read)))))
	     (bind-mounts (seq-filter #'(lambda (mount) (equal (alist-get 'Type mount) "bind")) mounts))
	     (matched-mounts (seq-filter #'(lambda (mount) (string-prefix-p (alist-get 'Destination mount) local-filename)) bind-mounts)))
	(cond ((eq (length matched-mounts) 0)
	       'not-found)
	      ((eq (length matched-mounts) 1)
	       (let ((source (alist-get 'Source (car matched-mounts)))
		     (destination (alist-get 'Destination (car matched-mounts))))
		 (f-join source (f-relative local-filename destination))))
	      (t
	       (error
		(message "Failed to identify source filename: %s" filename)))))
    'not-docker))

(defun my-docker-tramp-find-docker-local-file (filename &optional no-kill)
  "Find the file that corresponds to a remote FILENAME inside a docker container.

The original buffer is killed unless NO-KILL is non-nil."
  (interactive (list (or buffer-file-name default-directory) current-prefix-arg))
  (let ((local-filename (my-docker-tramp-get-docker-local-filename filename)))
    (cond ((eq local-filename 'not-found)
	   (message "Corresponding local file not found"))
	  ((eq local-filename 'not-docker)
	   (error
	    (message "Docker executable not found.")))
	  (t
	   (switch-to-buffer (find-file-noselect local-filename))
	   (if (not no-kill)
	       (kill-buffer (get-file-buffer filename)))))))

(defun my-docker-tramp-get-docker-remote-filenames (local-filename)
  "Return a list of remote docker filenames that correspondto LOCAL-FILENAME.
If docker executable is not found, return 'exec-not-found."
  (if (not (executable-find "docker"))
      'exec-not-found
    (let ((expanded-filename (expand-file-name local-filename))
	  containers candidates)
      (with-temp-buffer
	(let ((ret-value (call-process "docker" nil t nil  "container" "ls" "--format" "{{ json .Names }}")))
	  (when (eq ret-value 0)
	    (goto-char (point-min))
	    (while (< (point) (point-max))
	      (add-to-list 'containers (json-read-from-string (buffer-substring (line-beginning-position) (line-end-position))))
	      (forward-line 1)))))
      (dolist (container containers)
	(let* ((container-mounts (with-temp-buffer
				   (let ((ret-value (call-process "docker" nil t nil "inspect" "-f" "{{ json .Mounts }}" container)))
				     (when (eq ret-value 0)
				       (goto-char (point-min))
				       (json-read)))))
	       (container-bind-mounts (seq-filter #'(lambda (mount) (equal (alist-get 'Type mount) "bind")) container-mounts))
	       (container-matched-mounts (seq-filter #'(lambda (mount) (string-prefix-p (alist-get 'Source mount) expanded-filename)) container-bind-mounts))
	       (destination-filenames (mapcar #'(lambda (mount)
						  (let ((source (alist-get 'Source mount))
							(destination (alist-get 'Destination mount)))
						    (format "/docker:%s:%s"
							    container
							    (f-join destination (f-relative expanded-filename source)))))
					      container-matched-mounts)))
	  (setq candidates (append candidates destination-filenames))))
      candidates)))

(defun my-docker-tramp-find-docker-remote-file (local-filename &optional no-kill)
  "Find the file that corresponds to a LOCAL-FILENAME.
If the local file is mounted to multiple containers, prompt to choose which file to open.

The original buffer is killed unless NO-KILL is non-nil."
  (interactive (list (or buffer-file-name default-directory) current-prefix-arg))
  (cond ((file-remote-p local-filename)
	 (message "Specified file is not a local file" local-filename))
	((not local-filename)
	 (error
	  (message "Invalid filename: %s" local-filename)))
	(t
	 (let ((candidates (my-docker-tramp-get-docker-remote-filenames local-filename)))
	   (cond ((eq candidates 'exec-not-found)
		  (error
		   (message "Docker executable not found.")))
		 ((not candidates)
		  (message "Corresponding file inside docker container not found. Check if the container is running"))
		 (t
		  (switch-to-buffer
		   (find-file-noselect
		    (if (eq (length candidates) 1)
			(car candidates)
		      (completing-read prompt candidates nil t (car candidates) (cons 'candidates 1) (car candidates)))))
		  (if (not no-kill)
		      (kill-buffer (get-file-buffer local-filename)))))))))

(defun my-docker-tramp-find-corresponding-file (filename &optional no-kill)
  "Find the corresponding docker local/remote file.
If FILENAME is a remote docker filename (i.e., /docker:{container}:{path}),
find the corresponding local file.

If FILENAME is a local filename, find a corresponding file inside a docker container.
If the local file is mounted to multiple containers, prompt to choose which file to open.

The original buffer is killed unless NO-KILL is non-nil."
  (interactive (list (or buffer-file-name default-directory) current-prefix-arg))
  (if (file-remote-p filename)
      (my-docker-tramp-find-docker-local-file filename no-kill)
    (my-docker-tramp-find-docker-remote-file filename no-kill)))


(defun my-docker-tramp-recentf-filename-handler (filename)
  "Return the local filename that corresponds to FILENAME inside a docker container.
If FILENAME is not inside a docker container, return FILENAME.

recentf-filename-handler for files inside docker containers."
  (let ((local-filename (my-docker-tramp-get-docker-local-filename filename)))
    (if (member local-filename '(not-found not-docker))
	filename
      local-filename)))

(provide 'my-docker-tramp)
