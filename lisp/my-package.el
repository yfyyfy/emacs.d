(require 'package)

(defvar my-package-selected-packages
  (if (boundp 'package-selected-packages)
      package-selected-packages
    '(recentf-ext color-moccur cygwin-mount w3 htmlize yaml-mode php-mode csv-mode magit helm-swoop migemo web-mode msvc helm-gtags company-irony cmake-mode))
  "")

(defvar my-package-repositories
  (if (boundp 'package-archives)
      (mapcar 'cdr package-archives)
    '(
      "http://elpa.gnu.org/packages/"
      "http://stable.melpa.org/packages/"
      "http://melpa.org/packages/"
      "http://marmalade-repo.org/packages/"
      ))
  "")

(defun my-package-install (pkgs repositories)
  "Install all packages in PKGS from REPOSITORIES.
PKGS is a list of package symbols.
REPOSITORIES is a list of elpa repository URLs.
Each package is installed from the first repository in which it is found."
  (dolist (repository repositories)
    (setq pkgs (my-package--execute-pop-if-exists pkgs repository
						 '(lambda (pkgs)
						    (dolist (pkg pkgs)
						      (package-install pkg)))))))

(defun my-package-check (pkgs repositories)
  "Check from which repositories in REPOSITORIES packages in PKGS will be retrieved."
  (apply
   (let ((repository-package-alist (make-symbol "_repository-package-alist_")))
     `(lambda (pkgs repositories)
	(let (,repository-package-alist)
	  (dolist (repository repositories)
	    (setq pkgs (my-package--execute-pop-if-exists pkgs repository #'(lambda (pkgs) (add-to-list ',repository-package-alist (list repository pkgs))))))
	  (add-to-list ',repository-package-alist (list nil pkgs))
	  ,repository-package-alist)))
     (list pkgs repositories)))

(defun my-package--execute-pop-if-exists (pkgs repos func)
  "Execute func for each package in PKGS if it is found in REPOS.
PKGS is a list of package symbols.
REPOS is a elpa repository URL.
FUNC takes one argument (package symbol)."
  (let (pkgs-in pkgs-out pkgs-all)
    (with-repository repos
      (setq pkgs-all (mapcar (lambda (elt) (car elt)) package-archive-contents))
      (dolist (pkg pkgs)
	(add-to-list (if (memq pkg pkgs-all) 'pkgs-in 'pkgs-out)
		     pkg)))
    (apply func (list pkgs-in))
    pkgs-out))

;; Usage
;; (my-package-check my-package-selected-packages my-package-repositories)
;; (my-package-install my-package-selected-packages my-package-repositories)

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
