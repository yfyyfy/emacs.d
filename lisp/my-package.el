(require 'package)

(defvar my-package-selected-packages
  (if (boundp 'package-selected-packages)
      package-selected-packages
    '(recentf-ext color-moccur cygwin-mount w3 htmlize yaml-mode php-mode csv-mode magit helm-swoop migemo web-mode msvc helm-gtags company-irony cmake-mode))
  "")

(defvar my-locations
  (if (boundp 'package-archives)
      (mapcar 'cdr package-archives)
    '(
      "http://elpa.gnu.org/packages/"
      "http://stable.melpa.org/packages/"
      "http://melpa.org/packages/"
      "http://marmalade-repo.org/packages/"
      ))
  "")

(defun my-package-install (pkgs locations)
  "Install all packages in PKGS from LOCATIONS.
PKGS is a list of package symbols.
LOCATIONS is a list of elpa repository URLs.
Each package is installed from the first repository in which it is found."
  (dolist (location locations)
    (setq pkgs (my-package--execute-pop-if-exists pkgs location
						 '(lambda (pkgs)
						    (dolist (pkg pkgs)
						      (package-install pkg)))))))

(defun my-package-check (pkgs locations)
  "Check from which repositories in LOCATIONS packages in PKGS will be retrieved."
  (let (myval)
    (dolist (location locations)
      (setq pkgs (my-package--execute-pop-if-exists pkgs location '(lambda (pkgs) (add-to-list 'myval (list location pkgs))))))
    (add-to-list 'myval (list nil pkgs))
    myval))

(defun my-package--execute-pop-if-exists (pkgs location func)
  "Execute func for each package in PKGS if it is found in LOCATION.
PKGS is a list of package symbols.
LOCATION is a elpa repository URL.
FUNC takes one argument (package symbol)."
  (let (pkgs-in pkgs-out pkgs-all)
    (with-repository location
      (setq pkgs-all (mapcar (lambda (elt) (car elt)) package-archive-contents))
      (dolist (pkg pkgs)
	(add-to-list (if (memq pkg pkgs-all) 'pkgs-in 'pkgs-out)
		     pkg)))
    (apply func (list pkgs-in))
    pkgs-out))

;; Usage
;; (my-package-check my-package-selected-packages my-locations)
;; (my-package-install my-package-selected-packages my-locations)

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
