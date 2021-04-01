(defvar my-grep-commands
      '(("directory" . "grep --color=always -R . -nH --null -e ")
	("git" . "PAGER=\"\" git grep -nH -e "))
      "List of preconfigured grep commands.")

(defun my-grep (command-args)
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (if current-prefix-arg
	   (let ((grep-command (cdr (assoc (completing-read "Select grep command: " my-grep-commands nil t) my-grep-commands))))
	     (list (read-shell-command "Run grep (like this): " grep-command 'grep-history default)))
	 ;; Default grep behavior from emacs-26.1.
	 (list (read-shell-command "Run grep (like this): " grep-command 'grep-history default))))))
  (grep command-args))

(provide 'my-grep)
