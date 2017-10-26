(defvar my-el-get-package-list '(el-get)
  "List of packages intended to be managed by el-get.")

(defun my-el-get-install (packages)
  "Install PACKAGES via el-get and load them.
Install el-get first if it has not been installed.
This function does not work when system-type is windows-nt."
  (interactive (list my-el-get-package-list))
    (if (eq system-type 'windows-nt)
	(message "This function does not work when system-type is windows-nt.")
      ;; Install el-get.
      (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
      (unless (require 'el-get nil 'noerror)
	(with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      ;; Install the other packages.
      (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
      (mapc #'(lambda (package) (eval `(el-get-bundle ,package))) packages)))

(defun my-el-get-activate-packages (&optional packages)
  "Load PACKAGES if all PACKAGES have been installed via el-get.
When called interactively or with PACKAGES nil, use
`my-el-get-package-list' as PACKAGES"
  (interactive (list my-el-get-package-list))
  (unless packages
    (setq packages my-el-get-package-list))
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (if (require 'el-get nil 'noerror)
      (progn
	(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
	(let ((packages-not-installed (my-el-get-get-packages-not-installed packages)))
	  (if packages-not-installed
	      (message "Following packages are not installed: %s"
		       (mapconcat 'prin1-to-string packages-not-installed ", "))
	    (mapc #'(lambda (package) (eval `(el-get-bundle ,package))) packages))))
    (message "el-get was not found.")))

(defun my-el-get-get-packages-not-installed (packages)
  "Return a list of packages still not installed via el-get.
PACKAGES is a list of packages to query.
This function assumes that el-get is loaded."
  (let ((installed-packages (el-get-list-package-names-with-status "installed")))
    (delete nil
	    (mapcar #'(lambda (package)
			(unless (member (car (last (split-string (prin1-to-string package) "/"))) installed-packages)
			  package))
		    packages))))

(provide 'my-el-get)
