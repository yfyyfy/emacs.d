(defun my-lsp-ui-doc-toggle ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
	(lsp-ui-doc-mode -1)
	(lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode 1)))

(defun my-lsp-eslint-install ()
  "Insall server for lsp-eslint.
lsp-install-server for lsp-eslint@lsp-mode-8.0.0 won't work (fixed in 20220425.1046)."
  (let ((url "https://github.com/emacs-lsp/lsp-server-binaries/blob/master/dbaeumer.vscode-eslint-2.2.2.vsix?raw=true")
	(install-path (f-join lsp-server-install-dir "eslint-patch/unzipped"))
	(download-path (make-temp-file "ext" nil ".zip")))
    (if (f-exists? install-path)
	(message "vscode-eslint is already installed in %s. Check if it is copied or symlinked to %s" install-path lsp-eslint-unzipped-path)
      (url-copy-file url download-path t)
      (lsp-unzip download-path install-path)
      (f-delete download-path)
      (message "Installed vscode-eslint in %s. Copy or symlink the directory to %s" install-path lsp-eslint-unzipped-path))))

(provide 'my-lsp)
