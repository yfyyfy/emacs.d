(require 'cygwin-mount)

(defmacro with-cygwin-mount-activated (&rest body)
  `(let ((cygwin-mount-activated-orig cygwin-mount-activated))
    (prog2
	(if (not cygwin-mount-activated-orig)
	    (cygwin-mount-activate))
	(progn ,@body)
      (if (not cygwin-mount-activated-orig)
	  (cygwin-mount-deactivate)))))

(provide 'my-cygwin-mount)
