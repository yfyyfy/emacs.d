(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection (list "typescript-language-server" "--stdio"))
		  :remote? t
		  :server-id 'ts-ls-remote
		  ;; Copied from lsp-pylsp.el, commenting out server-id. download-server-fn may not work on remote.
		  :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
		  :priority -2
		  :completion-in-comments? t
		  :initialization-options (lambda ()
					    (list :plugins lsp-clients-typescript-plugins
						  :logVerbosity lsp-clients-typescript-log-verbosity
						  :tsServerPath (lsp-package-path 'typescript)))
		  :ignore-messages '("readFile .*? requested by TypeScript but content not available")
		  ;; :server-id 'ts-ls
		  :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename))
		  :download-server-fn (lambda (_client callback error-callback _update?)
					(lsp-package-ensure
					 'typescript
					 (-partial #'lsp-package-ensure
						   'typescript-language-server
						   callback
						   error-callback)
					 error-callback))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection (list "node" "/workspace/.cache-lsp/eslint-patch/unzipped/extension/server/out/eslintServer.js" "--stdio"))
		  :remote? t
		  :server-id 'eslint-remote
		  ;; Copied from lsp-eslint.el, commenting out server-id. download-server-fn may not work on remote.
		  :activation-fn (lambda (filename &optional _)
				   (when lsp-eslint-enable
				     (or (string-match-p (rx (one-or-more anything) "."
							     (or "ts" "js" "jsx" "tsx" "html" "vue" "svelte")eos)
							 filename)
					 (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode 'svelte-mode))))
		  :priority -1
		  :completion-in-comments? t
		  :add-on? t
		  :multi-root t
		  :notification-handlers (ht ("eslint/status" #'lsp-eslint-status-handler))
		  :request-handlers (ht ("workspace/configuration" #'lsp-eslint--configuration)
					("eslint/openDoc" #'lsp-eslint--open-doc)
					("eslint/probeFailed" #'lsp-eslint--probe-failed))
		  :async-request-handlers (ht ("eslint/confirmESLintExecution" #'lsp-eslint--confirm-local))
		  ;; :server-id 'eslint
		  :initialized-fn (lambda (workspace)
				    (with-lsp-workspace workspace
				      (lsp--server-register-capability
				       (lsp-make-registration
					:id "random-id"
					:method "workspace/didChangeWatchedFiles"
					:register-options? (lsp-make-did-change-watched-files-registration-options
							    :watchers
							    `[,(lsp-make-file-system-watcher
								:glob-pattern "**/.eslintr{c.js,c.yaml,c.yml,c,c.json}")
							      ,(lsp-make-file-system-watcher
								:glob-pattern "**/.eslintignore")
							      ,(lsp-make-file-system-watcher
								:glob-pattern "**/package.json")])))))
		  :download-server-fn (lambda (_client callback error-callback _update?)
					(let ((tmp-zip (make-temp-file "ext" nil ".zip")))
					  (delete-file tmp-zip)
					  (lsp-download-install
					   (lambda (&rest _)
					     (condition-case err
						 (progn
						   (lsp-unzip tmp-zip lsp-eslint-unzipped-path)
						   (funcall callback))
					       (error (funcall error-callback err))))
					   error-callback
					   :url (lsp-vscode-extension-url "dbaeumer" "vscode-eslint" "2.1.23")
					   :store-path tmp-zip)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
		  :remote? t
		  :server-id 'pylsp-remote
		  ;; Copied from lsp-pylsp.el, commenting out server-id.
		  :major-modes '(python-mode cython-mode)
		  :priority -1
		  ;; :server-id 'pylsp
		  :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
		  :initialized-fn (lambda (workspace)
				    (with-lsp-workspace workspace
				      (lsp--set-configuration (lsp-configuration-section "pylsp"))))))

(provide 'my-lsp-register-remote-client)
