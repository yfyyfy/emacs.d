(defvar my-el-get-el-get-path
  (locate-user-emacs-file "el-get/el-get")
  "The path of el-get library.
This variable is intended to be used for locating el-get library.  Do
not assume that `el-get-dir' is defined when defining this variable.")

(defvar my-el-get-my-recipe-path
  (locate-user-emacs-file "el-get-user/recipes")
  "A personal directory of el-get recipes to add to `el-get-recipe-path'")

(defvar my-el-get-package-list '(el-get)
  "List of packages intended to be managed by el-get.")

(defun my-el-get-load (package)
  "Load PACKAGE managed by el-get.
This function loads all lisp files in el-get's PACKAGE directory and
intended to be used for single-file packages."
  (let* ((regexp "\\.elc?$")
	 (dir (el-get-package-directory package))
	 (files
	  (delete-dups
	   (mapcar #'(lambda(str) (replace-regexp-in-string regexp "" str))
		   (directory-files dir nil regexp)))))
    (mapcar 'load files)))

(defun my-el-get-activate-packages-install-if-necessary (&optional packages force-install)
  "Load PACKAGES and install those which are not installed via el-get.
When called interactively or with PACKAGES nil, use
`my-el-get-package-list' as PACKAGES.
If FORCE-INSTALL is not nil, install packages without asking."
  (interactive)
  (let ((ret (my-el-get-activate-packages packages)))
    (when (or force-install
	      (and (eq ret nil)
		   (yes-or-no-p "el-get is not installed. Install? "))
	      (and (not (eq ret t))
		   (yes-or-no-p (format "Following packages are not installed: %s. Install? "
					(mapconcat 'prin1-to-string ret ", ")))))
      (my-el-get-install packages))))

(defun my-el-get-install (&optional packages)
  "Install PACKAGES via el-get and load them.
Install el-get first if it has not been installed.
This function does not work when system-type is windows-nt."
  (interactive)
  (unless packages
    (setq packages my-el-get-package-list))
  (if (eq system-type 'windows-nt)
      (message "This function does not work when system-type is windows-nt.")
    ;; Install el-get.
    (add-to-list 'load-path my-el-get-el-get-path)
    (unless (require 'el-get nil 'noerror)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
	(goto-char (point-max))
	(eval-print-last-sexp)))
    ;; Install the other packages.
    (add-to-list 'el-get-recipe-path my-el-get-my-recipe-path)
    (mapc #'(lambda (package) (eval `(el-get-bundle ,package))) packages)))

(defun my-el-get-activate-packages (&optional packages)
  "Load PACKAGES if all PACKAGES have been installed via el-get.
When called interactively or with PACKAGES nil, use
`my-el-get-package-list' as PACKAGES.
Return nil if `el-get' is not installed.
Return t if all packages in PACKAGES are installed and activate.
Otherwise, return list of packages not installed."
  (interactive (list my-el-get-package-list))
  (unless packages
    (setq packages my-el-get-package-list))
  (add-to-list 'load-path my-el-get-el-get-path)
  (if (require 'el-get nil 'noerror)
      (progn
	(add-to-list 'el-get-recipe-path my-el-get-my-recipe-path)
	(let ((packages-not-installed (my-el-get-get-packages-not-installed packages)))
	  (if packages-not-installed
	      (progn
		(message "Following packages are not installed: %s"
			 (mapconcat 'prin1-to-string packages-not-installed ", "))
		packages-not-installed)
	    (mapc #'(lambda (package) (eval `(el-get-bundle ,package))) packages)
	    t)))
    (message "el-get was not found.")
    nil))

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
