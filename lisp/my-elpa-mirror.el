(setq elpamr-default-output-directory (locate-user-emacs-file "elpa-local-repos"))

(defun my-elpamr-create-mirror-for-installed ()
  "Set output directory and clean it up before calling `elpamr-create-mirror-for-installed'.
The behavior of `elpamr-create-mirror-for-installed' is modified so that output
directory is self-sufficient, i.e., all packages with the version specified in
archive-contents file is included in the directory.

FIXME:
Created mirror can be restored by `my-elpamr-restore-from-mirror',
but re-creating mirror with `my-elpamr-create-mirror-for-installed'
may end up with some differences (diff -rq myelpa myelpa2).
There are two reasons for this behavior.
1. Time-stamps in PACKAGE.tar archive are different.
2. Time-stamps in PACKAGE-autoloads.el (near the end of the file) are different.

These problems can be solved by scripts below,
but it will be better to automate this process.
===============================================================================
for tarfile in *.tar
do
	tmpfile=`mktemp -d tmp.XXXXX`
	tar xf $tarfile -C $tmpfile --warning=no-timestamp
	rm $tmpfile/${tarfile%.tar}/${tarfile%-*}-autoloads.el
	tar cf $tarfile -C $tmpfile `ls $tmpfile` --mtime='1970-01-01 00:00:00Z' --warning=no-timestamp --owner=0 --group=0 --numeric-owner --sort=name
	rm -rf $tmpfile
	# tar --delete -f $tarfile ${tarfile%.tar}/${tarfile%-*}-autoloads.el
done
===============================================================================
"
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

(defun my-elpamr-restore-from-mirror (&optional dir ask)
  "Install packages from local repository.
The packages listed in `my-package-selected-packages' are installed from
`elpamr-default-output-directory'.
If the output directory `package-user-dir' is not empty, abort."
  (interactive (list nil current-prefix-arg))
  (require 'my-package)
  ;; Set local mirror directory for restoration.
  (when ask
    (setq dir (read-directory-name "Restore from: " elpamr-default-output-directory nil t)))
  (when (or (not dir)
	    (= (length dir) 0))
    (setq dir elpamr-default-output-directory))
  ;; Create `package-user-dir' if necessary.
  (if (not (file-directory-p package-user-dir))
      (make-directory-internal package-user-dir))
  ;; Install packages to `package-user-dir' if it is empty.
  (if (delete 0
	      (mapcar #'(lambda (str) (or (string-match "^\\.*$" str) str))
		      (directory-files package-user-dir)))
      (message "%s is not empty" package-user-dir)
    (my-package-install my-package-selected-packages (list dir))))

(defun elpamr--executable-find-around (f &rest args)
  "Let Emacs to find executables without the help of `elpamr--executable-find'."
  ;; (let ((system-type 'not-windows-nt))
  ;;   (apply f args))
  (car args))
(advice-add 'elpamr--executable-find :around #'elpamr--executable-find-around)

(defun elpamr--extract-info-from-dir-override (dirname)
  "Use package.el's logic to extract package information.
The original function does not recognize version strings with alphabet.
According to `package-version-join', \"pre\",\"beta\",\"alpha\" and \"snapshot\"
are valid words in version string."
  (let ((pkg-desc (package-load-descriptor (expand-file-name dirname package-user-dir))))
    (if pkg-desc
	(let ((name (package-desc-name pkg-desc))
	      (version (package-desc-version pkg-desc)))
	  (list (prin1-to-string name) (mapcar 'prin1-to-string version))))))
(advice-add 'elpamr--extract-info-from-dir :override #'elpamr--extract-info-from-dir-override)

(provide 'my-elpa-mirror)
