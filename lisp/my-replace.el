;;;###autoload
(defun my-query-replace-multi (replace-list &optional delimited start end backward)
  "replace list example:
MyFunction YourFunction
myfunction yourfunction
MYFUNCTION YOURFUNCTION"
  (interactive
   (list
    (let ((str (read-from-minibuffer"replace list: "))
	  (rs (if current-prefix-arg (read-from-minibuffer "record separator: ") "\n"))
	  (fs (if current-prefix-arg (read-from-minibuffer "field separator: ") " ")))
      (mapcar (lambda (a) (split-string a fs t))
	      (split-string str rs t)))
    nil
    (if (and transient-mark-mode mark-active)
	(region-beginning))
    (if (and transient-mark-mode mark-active)
	(region-end))
    nil))
  (let ((case-fold-search nil))
    (mapcar (lambda (a)
	      (save-excursion (apply 'query-replace a)))
	    replace-list)))

(provide 'my-replace)
