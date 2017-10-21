(require 'package)

(defun my-package--get-original-value (var)
  "Return the original value of the symbol VAR."
  (let* ((sv (get var 'standard-value))
	 (origval (and (consp sv)
		       (condition-case nil
			   (eval (car sv))))))
    origval))

(defvar my-package-selected-packages
  (if (and (boundp 'package-selected-packages)
	   (not (eq package-selected-packages
		    (my-package--get-original-value 'package-selected-packages))))
      package-selected-packages
    (eval
     (car
      (delete nil
	      (with-temp-buffer
		(insert-file-contents user-init-file)
		(goto-char (point-min))
		(re-search-forward "(custom-set-variables\\_>" nil 'noerror)
		(let* ((start (match-beginning 0))
		       (end (scan-sexps start 1))
		       (sexp (read (buffer-substring start end))))
		  (mapcar #'(lambda (elt)
			      (and (listp elt)
				   (eq 'quote (car elt))
				   (listp (cadr elt))
				   (eq 'package-selected-packages (car (cadr elt)))
				   (cadr (cadr elt))))
			  sexp)))))))
  "my-package's version of `package-selected-packages'.")

(defvar my-package-repositories
  (if (and (boundp 'package-archives)
	   (not (eq package-archives
		    (my-package--get-original-value 'package-archives))))
      (mapcar 'cdr package-archives)
    '(
      "http://elpa.gnu.org/packages/"
      "http://stable.melpa.org/packages/"
      "http://melpa.org/packages/"
      "http://marmalade-repo.org/packages/"
      ))
  "List of package repositories URL for my-package.
Each package will be installed from the first repository which contains it.")

(defun my-package-install (pkgs repositories &optional pop-up dry-run)
  "Install all packages in PKGS from REPOSITORIES.
PKGS is a list of package symbols.
REPOSITORIES is a list of elpa repository URLs.
If POP-UP is non-nil, show buffer summarizing the result.
If DRY-RUN is non-nil, do not install packages and just return the result.
Each package is installed from the first repository in which it is found.

Return an alist of the form ((REPOSITORY-LOCATION . PACKAGE-LIST) ...)
where REPOSITORY-LOCATION is the location of the repository or 'orphan-packages
for packages not found in any of the REPOSITORIES,
and PACKAGE-LIST is a list of package symbols found in REPOSITORY-LOCATION or
'invalid if REPOSITORY-LOCATION cannot be read."
  (interactive
   (list my-package-selected-packages my-package-repositories t nil))
  (let ((ret
	 (apply
	  (let ((repository-package-alist (make-symbol "_repository-package-alist_")))
	    `(lambda (pkgs repositories)
	       (let (,repository-package-alist)
		 (dolist (repository repositories)
		   (setq pkgs (my-package--execute-pop-if-exists pkgs repository
								 #'(lambda (pkgs)
								     (if (not dry-run)
									 (if (not (eq pkgs 'invalid))
									     (dolist (pkg pkgs)
									       (package-install pkg))))
								     (add-to-list ',repository-package-alist
										  (list repository pkgs))))))
		 (add-to-list ',repository-package-alist (list 'orphan-packages pkgs))
		 ,repository-package-alist)))
	  (list pkgs repositories))))
    ;; (nreverse ret)
    (setq ret (reverse ret))
    (if pop-up
	(my-package-pop-up-result ret))
    ret))

(defun my-package-check (pkgs repositories &optional pop-up)
  "Check from which repositories in REPOSITORIES packages in PKGS will be retrieved.
If POP-UP is non-nil, show buffer summarizing the result."
  (interactive
   (list my-package-selected-packages my-package-repositories t))
  (my-package-install pkgs repositories pop-up t))

(defvar my-package-output-buffer "*my-package*")
(defun my-package-pop-up-result (result)
  "Pretty-print results of `my-package-check' in `my-package-output-buffer'"
  (get-buffer-create my-package-output-buffer)
  (with-current-buffer my-package-output-buffer
    (erase-buffer)
    (let ((max-repository-length
	   (apply 'max (mapcar #'(lambda (elt)
				   (let ((c (car elt)))
				     (length
				      (if (stringp c) c (prin1-to-string c)))))
			       result))))
      (dolist (elt result)
	(let ((repos (car elt))
	      (pkgs (cdr elt)))
	  (insert (format (format "%%-%ss: %%s\n" (1+ max-repository-length))
			  repos
			  (if (eq (car pkgs) 'invalid)
			      "(failed to read repository)"
			    (mapconcat 'prin1-to-string (car pkgs) " "))))))))
  (if (with-current-buffer my-package-output-buffer (> (point-max) (point-min)))
      (display-message-or-buffer (get-buffer my-package-output-buffer))))

(defun my-package--execute-pop-if-exists (pkgs repos func)
  "Execute func for each package in PKGS if it is found in REPOS.
PKGS is a list of package symbols.
REPOS is a elpa repository URL.
FUNC takes one argument: list of package symbols, or 'invalid if reading REPOS is failed."
  (let (pkgs-in pkgs-out pkgs-all ret)
    (with-repository repos
		     (apply func (list
				  (progn
				    (setq pkgs-all (mapcar (lambda (elt) (car elt)) package-archive-contents))
				    (dolist (pkg pkgs)
				      (add-to-list (if (memq pkg pkgs-all) 'pkgs-in 'pkgs-out)
						   pkg))
				    pkgs-in))))
    pkgs-out))

(defmacro with-repository (repos &rest body)
  "Read REPOS, evaluate BODY forms sequentially and return value of last one
If reading REPOS is failed, return 'invalid."
  (declare (indent 1) (debug t))
  (let ((temp-dir (make-symbol "_temp-dir_"))
	(invalid-repository-p (make-symbol "_invalid-repository-p_")))
    `(let ((,temp-dir (make-temp-file "my-package-" t)))
       ;; Create temporary repository.
       (with-temp-file (expand-file-name "archive-contents" ,temp-dir)
	 (insert (prin1-to-string '(1 (dummy . [(1) nil "dummy" tar])))))
       ;; Initialize package if necessary.
       (unless package--initialized
	 (package-initialize t))
       ;; First, read temporary repository, whose only package is "dummy".
       ;; Then, read REPOS.
       ;; If `package-archive-contents' contains a sole package "dummy",
       ;; reading REPOS is failed.
       ;; If reading REPOS is succeeded, execute BODY.
       (prog1
	   (let ((,invalid-repository-p
		  (progn
		    (let ((package-archives `(("" . ,,temp-dir))))
		      (package-refresh-contents))
		    (let ((package-archives `(("" . ,,repos)))
			  (debug-on-error nil))
		      (package-refresh-contents))
		    (and (= (length package-archive-contents) 1)
			 (eq (car (car package-archive-contents)) 'dummy))))
		 (package-archives `(("" . ,,repos))))
	     (if ,invalid-repository-p
		 'invalid
	       (progn ,@body)))
	 ;; Delete temporary repository.
	 (delete-directory ,temp-dir t)))))

(provide 'my-package)
