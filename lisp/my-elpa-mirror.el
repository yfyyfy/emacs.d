(setq elpamr-default-output-directory "~/myelpa")

(defun my-elpamr-create-mirror-for-installed ()
  "Set output directory and clean it up before calling `elpamr-create-mirror-for-installed'.
The behavior of `elpamr-create-mirror-for-installed' is modified so that output
directory is self-sufficient, i.e., all packages with the version specified in
archive-contents file is included in the directory."
  (interactive)
  (require 'elpa-mirror)
  (let* ((default-parent-dir (and elpamr-default-output-directory (file-name-directory elpamr-default-output-directory)))
	 (default-dirname (and elpamr-default-output-directory (file-name-nondirectory elpamr-default-output-directory)))
	 (directory (read-directory-name "Output directory: " default-parent-dir nil nil default-dirname))
	 (execute-p (or (not (file-directory-p directory))
			(yes-or-no-p "Directory already exists. Overwrite?: "))))
    (when execute-p
      ;; Create output directory or empty it if it exists.
      (if (file-directory-p directory)
	  (delete-directory directory t))
      (make-directory-internal directory)

      (setq elpamr-default-output-directory directory)

      ;; See the comment for elpamr-create-mirror-for-installed-around below.
      (let ((package-archive-contents nil))
	(elpamr-create-mirror-for-installed)))))

;; By default archive-contents file is created using `package-archive-contents'.
;; However, `package-archive-contents' is based on information from archive
;; repository. If package versions of local and remote are different,
;; package dependency (dependent packages and their versions) can be different.
;; In this case, resulting elpamr output directory will not be self-sufficient.
;; By setting `package-archive-contents' nil, archive-contents file is created
;; using information from local elpa directory.
;; See implementation of `elpamr--create-one-item-for-archive-contents'.
;; Tested with elpa-mirror-2.1.0.
;;
;; (defun elpamr-create-mirror-for-installed-around (f &rest args)
;;   (let ((package-archive-contents nil))
;;     (apply f args)))
;; (advice-add 'elpamr-create-mirror-for-installed :around #'elpamr-create-mirror-for-installed-around)

(defun my-elpamr-restore-from-mirror ()
  "Install packages from local repository.
The packages listed in `my-package-selected-packages' are installed from
`elpamr-default-output-directory'.
If the output directory `package-user-dir' is not empty, abort."
  (interactive)
  (if (not (and (boundp 'my-package-selected-packages)
		my-package-selected-packages))
      ;; No package to install.
      (message "my-package-selected-packages is empty or not defined.")
    ;; Create `package-user-dir' if necessary.
    (require 'my-package)
    (if (not (file-directory-p package-user-dir))
	(make-directory-internal package-user-dir))
    ;; Install packages to `package-user-dir' if it is empty.
    (if (delete 0
		(mapcar #'(lambda (str) (or (string-match "^\\.*$" str) str))
			(directory-files package-user-dir)))
	(message "%s is not empty" package-user-dir)
      (my-package-install my-package-selected-packages (list elpamr-default-output-directory)))))

(defun elpamr--executable-find-around (f &rest args)
  "Let Emacs to find executables without the help of `elpamr--executable-find'."
  ;; (let ((system-type 'not-windows-nt))
  ;;   (apply f args))
  (car args))
(advice-add 'elpamr--executable-find :around #'elpamr--executable-find-around)

(provide 'my-elpa-mirror)
