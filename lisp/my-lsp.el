(defun my-lsp-ui-doc-toggle ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
	(lsp-ui-doc-mode -1)
	(lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode 1)))

(provide 'my-lsp)
