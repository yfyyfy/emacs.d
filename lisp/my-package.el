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
  (let ((package-archives `(("" . ,location)))
	pkgs-in pkgs-out pkgs-all)
    (unless package--initialized
      (package-initialize t))
    (package-refresh-contents)
    (setq pkgs-all (mapcar (lambda (elt) (car elt)) package-archive-contents))
    (dolist (pkg pkgs)
      (add-to-list (if (memq pkg pkgs-all) 'pkgs-in 'pkgs-out)
		   pkg))
    (apply func (list pkgs-in))
    pkgs-out))

;; Usage
;; (my-package-check my-package-selected-packages my-locations)
;; (my-package-install my-package-selected-packages my-locations)

(provide 'my-package)
