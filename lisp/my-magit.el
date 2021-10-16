;; https://emacs.stackexchange.com/questions/19440/magit-extremely-slow-in-windows-how-do-i-optimize
;; WORKAROUND https://github.com/magit/magit/issues/2395

(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-untracked-files)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

;;;###autoload
(defun magit-staging ()
  (interactive)
  (require 'magit)
  (magit-mode-setup #'magit-staging-mode))

(provide 'my-magit)
