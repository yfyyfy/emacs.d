;; Basic functions
(defun my-transpose-cons-list (cons-list)
  (mapcar #'(lambda (ele) (cons (cdr ele) (car ele))) cons-list))

;; Path
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'Info-default-directory-list (locate-user-emacs-file "info"))
(add-to-list 'Info-default-directory-list (expand-file-name "~/local/info"))

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(let ((default-directory (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(autoload 'my-elpamr-create-mirror-for-installed "my-elpa-mirror" nil t)
(autoload 'my-elpamr-restore-from-mirror "my-elpa-mirror" nil t)
(when (and (not (file-directory-p package-user-dir))
	   (yes-or-no-p (format "package-user-dir (%s) does not exist. Restore from local mirror? " package-user-dir)))
  (my-elpamr-restore-from-mirror nil t))

;; el-get
(require 'my-el-get)
(setq my-el-get-package-list
      '(el-get
	helm-next-error
	java-mode-indent-annotations
	setup-cygwin
	w32-symlinks
	windows-path
	emacsmirror/visual-basic-mode))
(my-el-get-activate-packages-install-if-necessary)

;; Path-conversion utility
(require 'my-cygwin-mount)
(cond ((eq system-type 'windows-nt)
       (setq debug-on-error t)
       (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
       (require 'setup-cygwin)
       (setq package-user-dir (expand-file-name package-user-dir)))
      ((eq system-type 'cygwin)
       (setq cygwin-mount-cygwin-bin-directory "/usr/bin")
       (require 'windows-path)
       (windows-path-activate)))
(defun cygwin-mount-activate-around (f &rest args)
  (let ((cygwin-mount-activated-before cygwin-mount-activated)
	(res (apply f args)))
    (when (not cygwin-mount-activated-before)
      ;; Windows -> Cygwin
      (let ((cygwin-mount-table--internal (my-transpose-cons-list cygwin-mount-table--internal)))
	(setq exec-path (mapcar 'cygwin-mount-substitute-longest-mount-name exec-path))
	;; (setenv "HOME" (cygwin-mount-substitute-longest-mount-name (getenv "HOME")))
	(setenv "SHELL" (cygwin-mount-substitute-longest-mount-name (getenv "SHELL")))
	(setq shell-file-name (cygwin-mount-substitute-longest-mount-name shell-file-name))
	(and (boundp 'explicit-shell-file-name) (setq explicit-shell-file-name (cygwin-mount-substitute-longest-mount-name explicit-shell-file-name)))
	(and (boundp 'package-user-dir)         (setq package-user-dir (cygwin-mount-substitute-longest-mount-name package-user-dir)))
	(and (boundp 'scheme-program-name)      (setq scheme-program-name (cygwin-mount-substitute-longest-mount-name scheme-program-name)))))
    res))
(advice-add 'cygwin-mount-activate :around #'cygwin-mount-activate-around)
(defun cygwin-mount-deactivate-around (f &rest args)
  (let ((cygwin-mount-activated-before cygwin-mount-activated)
	(exec-path-windows-nt (mapcar 'cygwin-mount-substitute-longest-mount-name exec-path))
	;; (env-HOME-windows-nt (cygwin-mount-substitute-longest-mount-name (getenv "HOME")))
	(env-SHELL-windows-nt (cygwin-mount-substitute-longest-mount-name (getenv "SHELL")))
	(shell-file-name-windows-nt (cygwin-mount-substitute-longest-mount-name shell-file-name))
	(explicit-shell-file-name-windows-nt (and (boundp 'explicit-shell-file-name) (cygwin-mount-substitute-longest-mount-name explicit-shell-file-name)))
	(package-user-dir-nt                 (and (boundp 'package-user-dir)         (cygwin-mount-substitute-longest-mount-name package-user-dir)))
	(scheme-program-name-windows-nt      (and (boundp 'scheme-program-name)      (cygwin-mount-substitute-longest-mount-name scheme-program-name)))
	(res (apply f args)))
    ;; Cygwin -> Windows
    (when cygwin-mount-activated-before
      (setq exec-path exec-path-windows-nt)
      ;; (setenv "HOME" env-HOME-windows-nt)
      (setenv "SHELL" env-SHELL-windows-nt)
      (setq shell-file-name shell-file-name-windows-nt)
      (and (boundp 'explicit-shell-file-name) (setq explicit-shell-file-name explicit-shell-file-name-windows-nt))
      (and (boundp 'package-user-dir)         (setq package-user-dir package-user-dir-nt))
      (and (boundp 'scheme-program-name)      (setq scheme-program-name scheme-program-name-windows-nt)))
    res))
(advice-add 'cygwin-mount-deactivate :around #'cygwin-mount-deactivate-around)

;; exe-Path etc.
(when (eq window-system 'w32)
  (let ((exec-path-pre (list
			"/usr/local/bin"
			"/usr/bin"))
	(exec-path-post (list
			 "~/local/bin"
			 "~/mybin")))
    (when (eq system-type 'windows-nt)
      (setq exec-path-pre (mapcar 'cygwin-mount-substitute-longest-mount-name exec-path-pre))
      (setq exec-path-post (mapcar 'cygwin-mount-substitute-longest-mount-name exec-path-post)))
    (setq exec-path-pre (mapcar 'expand-file-name exec-path-pre))
    (setq exec-path-post (mapcar 'expand-file-name exec-path-post))
    (setq exec-path (append exec-path-pre exec-path exec-path-post)))
  (setenv "LC_CTYPE" "ja_JP.utf8") ; This fixes problems for shell, eg) svn st
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  ;; (keyboard-translate ?\\ ?_)
  ;; (keyboard-translate ?_ ?\\)

  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'meta)

  (cond ((eq system-type 'cygwin)
	 (setenv "PATH" (mapconcat 'identity exec-path ":")))
	((eq system-type 'windows-nt)
	 (setenv "PATH" (mapconcat 'identity exec-path ";")) 
	 (setenv "SHELL" "/bin/bash")
	 (setenv "LANG" "ja_JP.UTF-8"))))
(when (eq system-type 'windows-nt)
  (require 'my-ntemacs-coding-system))

;; Env
(setenv "LC_TIME" "C")
(setenv "LC_MESSAGES" "C")

;; Modes
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode)
(global-git-gutter-mode)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(transient-mark-mode 0)
(iswitchb-mode 1)
(global-hl-line-mode 1)

;; Variables
(setq line-move-visual nil)
(setq isearch-lax-whitespace nil)
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default electric-indent-mode t)
(setq locale-coding-system 'utf-8-unix) ; This fixes the problem with japanese-characters in the term-mode.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Key-bindings
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-H" 'help-for-help)
(global-set-key "\C-cvt" 'toggle-truncate-lines)
(global-set-key "\C-c\C-f" 'my-frame-set-frame)
(global-unset-key [mouse-2])
(global-unset-key [prior])
(global-unset-key [next])
(global-unset-key [insert])

;; Workaround for term-mode
(when (eq system-type 'windows-nt)
  (require 'fakecygpty)
  (fakecygpty-activate))

;; Migemo
(with-eval-after-load 'migemo
  (setq migemo-dict
	(with-cygwin-mount-activated
	 (let ((migemo-dicts (list
			      "/usr/share/cmigemo/utf-8/migemo-dict"
			      "/usr/local/share/cmigemo/utf-8/migemo-dict"
			      "/usr/share/migemo/dict/utf-8/migemo-dict"
			      "/usr/local/share/migemo/dict/utf-8/migemo-dict")))
	   (catch 'exists
	     (dolist (migemo-dict-internal migemo-dicts)
	       (when (file-exists-p migemo-dict-internal)
		 (if (eq system-type 'windows-nt)
		     ;; Windows binary requires Windows-style path.
		     (setq migemo-dict-internal (cygwin-mount-substitute-longest-mount-name migemo-dict-internal)))
		 (throw 'exists migemo-dict-internal)))))))

  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary migemo-dict)
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)

  (when (eq system-type 'windows-nt)
    ;; Re-interpret system-type as windows-nt so that migemo-get-pattern removes the additional \r in the output of Windows binary.
    (defun migemo-get-pattern-around (f &rest args)
      (let ((system-type 'windows-nt))
	(apply f args)))
    (advice-add 'migemo-get-pattern :around #'migemo-get-pattern-around)))

(autoload 'migemo-isearch-toggle-migemo "migemo" "Toggle migemo mode in isearch." t)
(autoload 'migemo-toggle-isearch-enable "migemo" nil t)
(defun helm-migemo-mode-around (f &rest args)
  (let ((migemo-loaded-before (featurep 'migemo))
	(migemo-loaded (require 'migemo nil 'noerror)))
    (prog1
	(apply f args)
      (if (and migemo-loaded (not migemo-loaded-before))
	  (setq migemo-isearch-enable-p nil)))))
(advice-add 'helm-migemo-mode :around #'helm-migemo-mode-around)

;; Dired
(when (eq system-type 'windows-nt)
  ;; Use ls for dired.
  (setq ls-lisp-use-insert-directory-program "ls")
  (defun insert-directory-around (f &rest args)
    "Pass Unix-form path to ls."
    (with-cygwin-mount-activated
       (let* ((cygwin-mount-table--internal (my-transpose-cons-list cygwin-mount-table--internal))
	      (file (cygwin-mount-substitute-longest-mount-name (car args)))
	      (newargs (cons file (cdr args))))
	 (apply f newargs))))
  (advice-add 'insert-directory :around #'insert-directory-around))
(with-eval-after-load "dired"
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;; VC
(setq log-edit-require-final-newline nil)
(add-hook 'vc-dir-mode-hook
	  '(lambda ()
	     (vc-dir-hide-state 'ignored)
	     (local-set-key "h" '(lambda ()
				   (interactive)
				   (vc-dir-hide-state 'unregistered)))))

(defadvice vc-dir (before vc-dir-before activate)
  (interactive
   (let ((prefix (prefix-numeric-value current-prefix-arg)))
     (list
      (let ((dirname (read-directory-name "VC status for directory: "
					  default-directory default-directory t nil)))
	(if (or (eq prefix 16) (eq prefix 64))
	    (file-truename dirname)
	  dirname))
      (if (or (eq prefix 4) (eq prefix 64))
	  (intern
	   (completing-read
	    "Use VC backend: "
	    (mapcar (lambda (b) (list (symbol-name b)))
		    vc-handled-backends)
	    nil t nil nil)))))))

;; Git-gutter
;; (require 'git-gutter-fringe)
(setq git-gutter:handled-backends '(git svn))
(defun my-git-gutter-nearest-backends (backends)
  (require 'cl-lib)
  (let* ((lengths
	  (mapcar
	   #'(lambda (elt)
	       (if (git-gutter:vcs-check-function elt)
		   (let ((rootdir
			  (locate-dominating-file default-directory
						  (plist-get '(git ".git" svn ".svn" hg ".hg" bzr ".bzr")
							     elt))))
		     (length rootdir))
		 -1))
	   backends))
	 (maxlen
	  (apply 'max lengths))
	 (pos
	  (if (>= maxlen 0)
	      (cl-position maxlen lengths)
	    -1)))
    (if (>= pos 0)
	(nth pos backends)
      nil)))
(defun git-gutter:in-repository-p-override ()
  (setq-local git-gutter:vcs-type (my-git-gutter-nearest-backends git-gutter:handled-backends)))
(advice-add 'git-gutter:in-repository-p :override #'git-gutter:in-repository-p-override)

;; Diff-hl
(defun my-dired-mode-hook ()
  (diff-hl-dired-mode-unless-remote)
  (unless (display-graphic-p)
    (unless (and (boundp 'diff-hl-margin-minor-mode) diff-hl-margin-minor-mode)
      (diff-hl-margin-mode))))
(add-hook 'dired-mode-hook #'my-dired-mode-hook)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(defun my-git-gutter-mode-hook ()
  (when git-gutter-mode
    (autoload 'diff-hl-diff-goto-hunk "diff-hl" nil t)
    (local-set-key "\C-xv=" 'diff-hl-diff-goto-hunk)
    (local-set-key "\C-xv[" 'git-gutter:previous-hunk)
    (local-set-key "\C-xv]" 'git-gutter:next-hunk)))
(add-hook 'git-gutter-mode-hook #'my-git-gutter-mode-hook)
(defun my-diff-hl-mode-hook ()
  (when diff-hl-mode
    (local-set-key "\C-xv=" 'diff-hl-diff-goto-hunk)
    (local-set-key "\C-xv[" 'diff-hl-previous-hunk)
    (local-set-key "\C-xv]" 'diff-hl-next-hunk)))
(add-hook 'diff-hl-mode-hook #'my-diff-hl-mode-hook)

;; SKK
(setq skk-user-directory (locate-user-emacs-file ".ddskk"))
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)
(setq dired-bind-jump nil)              ; Prevents C-x C-j from being overridden.
(setq skk-kakutei-key [henkan])         ; Alter mode-change key (skk-latin-mode -> skk-j-mode) (originally "l")
(add-hook 'isearch-mode-hook
	  '(lambda ()
	     (when (and (boundp 'skk-mode)
			skk-mode
			skk-isearch-mode-enable)
	       (skk-isearch-mode-setup))))
(add-hook 'isearch-mode-end-hook
	  '(lambda ()
	     (when (and (featurep 'skk-isearch)
			skk-isearch-mode-enable)
	       (skk-isearch-mode-cleanup))))

;; TRAMP
(setq password-cache-expiry nil)
(with-eval-after-load 'tramp
  (if (eq system-type 'windows-nt)
      (setq tramp-default-method "plink")
    (setq tramp-default-method "ssh"))
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-autosave")))

;; Flycheck
(with-eval-after-load 'flycheck
  (unless (display-graphic-p)
    (setq-default flycheck-indication-mode 'left-margin)
    (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode))
  (when (eq system-type 'windows-nt)
    ;; The default python executable was python on Flycheck-31, but python3 on the latest version.
    ;; However on Windows, venv creates python, not python3.
    (setq flycheck-json-python-json-executable "python")
    (setq flycheck-python-flake8-executable "python")
    (setq flycheck-python-pylint-executable "python")
    (setq flycheck-python-pycompile-executable "python"))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (require 'tide) ;; Load tsx-tide.
  (flycheck-add-next-checker 'tsx-tide '(t . javascript-eslint))
  ;; ;; Bring tsx-tide, jsx-tide and javascript-eslint to the end of `flycheck-checkers'.
  ;; (setq flycheck-checkers
  ;;       (append (delete 'tsx-tide flycheck-checkers)
  ;; 	      '(tsx-tide)))
  ;; 
  ;; (setq flycheck-checkers
  ;;       (append (delete 'jsx-tide flycheck-checkers)
  ;; 	      '(jsx-tide)))
  ;; 
  ;; (setq flycheck-checkers
  ;;       (append (delete 'javascript-eslint flycheck-checkers)
  ;; 	      '(javascript-eslint)))

  ;; flycheck-checkers
  (flycheck-define-generic-checker 'general-tide
    "A [JT]SX? syntax checker using tsserver."
    :start #'tide-flycheck-start
    :verify #'tide-flycheck-verify
    :modes '(web-mode js2-jsx-mode rjsx-mode)
    :predicate (lambda () t))
  (add-to-list 'flycheck-checkers 'general-tide)
  (flycheck-add-next-checker 'general-tide '(t . javascript-eslint)))

;; Elisp
(add-hook 'lisp-interaction-mode-hook
	  '(lambda()
	     (setq tab-width 8)))
(add-hook 'emacs-lisp-mode-hook
	  '(lambda()
	     (setq tab-width 8)))

;; CC
(setq c-default-style
      `((java-mode . "java")
	(awk-mode . "awk")
	(c++-mode . "k&r")
	(other . "gnu")))
(add-hook 'c-mode-hook
	  '(lambda()
	     (c-set-style "cc-mode")))
(add-hook 'c++-mode-hook
	  '(lambda()
	     (c-set-style "cc-mode")
	     (setq require-final-newline nil)
	     (setq indent-tabs-mode t)
;		 (setq tab-width 8)
	     (setq tab-width 2)
	     (setq c-basic-offset 2)))

;; Java
(defun my-java-mode-hook()
  (java-mode-indent-annotations-setup)
  (setq indent-tabs-mode t))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Fortran
(defadvice fortran-indent-to-column (around fortran-indent-to-column-around)
  (let ((indent-tabs-mode nil))
    ad-do-it
    (save-excursion
      (beginning-of-line)
      (if (looking-at "      ")
	  (replace-match "  ")))))

(add-hook 'fortran-mode-hook
	  '(lambda ()
	     (setq tab-width 6)
	     (setq c-basic-offset 2)
	     (setq fortran-if-indent 2)
	     (setq fortran-do-indent 2)
	     (setq indent-tabs-mode t)
	     (local-set-key "\C-c\C-c" 'comment-region)
	     (ad-activate 'fortran-indent-to-column)))

;; Python
(require 'my-python-venv)
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'my-python-venv-add-venv-to-exec-path)
(add-hook 'python-mode-hook #'flycheck-mode)
;; Jedi
(with-eval-after-load 'python-environment
  ;; Python executable for default flycheck syntax checker.
  (setq flycheck-python-pycompile-executable (python-environment-bin "python")))
(with-eval-after-load 'jedi-core
  (setq jedi:complete-on-dot t))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook #'add-node-modules-path)
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.[jt]sx?\\'"))
)
(defun my-web-mode-hook ()
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (when
      (and buffer-file-name
	   (string-match "^[jt]sx?$" (file-name-extension buffer-file-name)))
    (my-setup-tide-mode)
    (setq web-mode-enable-auto-quoting nil)
    (local-set-key "\C-c\C-c" 'comment-region)
    ;; (setq flycheck-disabled-checkers '(tsx-tide jsx-tide))
    ;; (setq flycheck-disabled-checkers '(tsx-tide javascript-eslint))
    ))
(with-eval-after-load 'web-mode
  (setq web-mode-engines-alist
	'(("django" . "\\.html\\'")
	  ("ejs" . "\\.ejs\\'"))))

;; TypeScript
(defun my-setup-tide-mode ()
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode)
  (company-mode-on))
(add-hook 'typescript-mode-hook
          (lambda ()
	    (my-setup-tide-mode)
	    (setq indent-tabs-mode nil)))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; JavaScript
(with-eval-after-load 'js2-mode
  (setq js2-strict-trailing-comma-warning nil))
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
(add-hook 'js-mode-hook
	  (lambda ()
 	    (setq js-indent-level 2)
 	    (setq indent-tabs-mode nil)
 	    (local-set-key "\C-c\C-c" 'comment-region)))
(add-hook 'rjsx-mode-hook
          (lambda ()
	    (my-setup-tide-mode)))
(add-hook 'rjsx-mode-hook #'add-node-modules-path)
(add-hook 'rjsx-mode-hook #'flycheck-mode)

;; http://blog.binchen.org/posts/indent-jsx-in-emacs.html
(defun js-jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))
(advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket)

;; scheme
(setq scheme-program-name "/usr/bin/guile")
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)

;; VB
(add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|vbs\\|vba\\|vb\\)$" . visual-basic-mode))
(setq visual-basic-mode-indent 4)

;; Org
(add-hook 'org-load-hook 'my-org-load-hook)
(defun my-org-load-hook ()
  (setq org-enforce-todo-dependencies t)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
;  (setq org-export-html-postamble nil)
  (setq org-directory "~/org.d")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files '("~/org.d/notes.org"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "cygstart %s")))

;; Disable automatic rearrangement of the agenda file set.
(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-defkey org-mode-map "\C-c[" 'undefined)
	     (org-defkey org-mode-map "\C-c]" 'undefined)))

(with-eval-after-load 'org
  (setq org-babel-python-command "python3")
  (add-to-list 'org-babel-load-languages '(shell . t))
  (add-to-list 'org-babel-load-languages '(python . t)))

;; automatically change to DONE when all children are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Formatting function to use before exporting org to html.
(defun my-org-feedline ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (= (forward-line 1) 0)
      (back-to-indentation)
      (unless (string-match "[*-+0-9]" (char-to-string (char-after)))
    (end-of-line 0)
    (unless (string-match "\\\\\\\\" (buffer-substring (- (point) 2) (point)))
      (if (eq (get-char-property (1- (point)) 'face) 'org-link)
	  (insert " \\\\")
	(insert "\\\\")))
    (forward-line 1)))))

;; Recentf
(require 'recentf-ext)
(setq recentf-exclude '(tramp-tramp-file-p))
(setq recentf-max-saved-items nil)
(setq recentf-filename-handlers
      (let* ((cygwin-drive "c")
	     (desktop-drive "d")
	     (documents-drive "d")
	     (downloads-drive "d")
	     (conv-list `(((format "^/cygdrive/%s/cygwin\\(64\\)?/home/" ,cygwin-drive) . "/home/")
			  ((format "^%s:/cygwin64/" ,cygwin-drive) . (format "%s:/cygwin/" ,cygwin-drive))
			  ((format "^%s:/cygwin/home/" ,cygwin-drive) . "/home/")
			  ((format "^%s:/Users/\\([^/]*\\)/Desktop/" ,desktop-drive) . "/home/\\1/Desktop/")
			  ((format "^%s:/Users/\\([^/]*\\)/Documents/" ,documents-drive) . "/home/\\1/Documents/")
			  ((format "^%s:/Users/\\([^/]*\\)/Downloads/" ,downloads-drive) . "/home/\\1/Downloads/"))))
	(mapcar #'(lambda (elt) `(lambda (name) (replace-regexp-in-string ,(car elt) ,(cdr elt) name))) conv-list)))

;; Helm
(defun my-helm-mini ()
  "helm-mini + helm-find."
  (interactive)
  (require 'helm-for-files) ;; For helm-source-recentf < helm-mini-default-sources < helm-source-recentf
  (require 'helm-find) ;; For helm-source-findutils
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
	  (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources (append helm-mini-default-sources '(helm-source-findutils))
	:buffer "*helm mini*"
	:ff-transformer-show-only-basename nil
	:truncate-lines helm-buffers-truncate-lines))
;; (global-set-key [?\C-;] 'my-helm-mini)
(global-set-key [67108923] 'my-helm-mini)
(defun my-helm-do-grep (dir)
  (interactive (list (if current-prefix-arg
			 (read-directory-name "Directory: " default-directory nil t)
		       default-directory)))
  (helm-do-grep-1 (list dir) t))
(add-hook 'helm-after-initialize-hook
  '(lambda ()
     (helm-migemo-mode 1)))
(my-el-get-load "helm-next-error") ;; Enable M-g M-p/M-g M-n for helm.

;; helm-gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
;; (setq helm-gtags-suggested-key-mapping t) ; Use custom-set-variables for setting this value is recommended.
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map "\C-cf" 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map "\C-cg" 'helm-gtags-find-pattern)
  (define-key helm-gtags-mode-map "\C-cs" 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map "\C-cr" 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map "\C-cd" 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map "\C-]" 'helm-gtags-find-tag-from-here)
  (define-key helm-gtags-mode-map (kbd "C-<") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C->") 'helm-gtags-next-history))

(when (eq system-type 'windows-nt)
  (defun helm-gtags--real-file-name-around (f &rest args)
    (let ((cygwin-mount-table--internal (my-transpose-cons-list cygwin-mount-table--internal))
	  (name (apply f args)))
      (cygwin-mount-substitute-longest-mount-name name)))
  (advice-add 'helm-gtags--real-file-name :around #'helm-gtags--real-file-name-around)

  (defun helm-gtags--set-parsed-file-around (f &rest args)
    (let ((cygwin-mount-table--internal (my-transpose-cons-list cygwin-mount-table--internal))
	  (name (apply f args)))
      (setq helm-gtags--parsed-file (cygwin-mount-substitute-longest-mount-name name))))
  (advice-add 'helm-gtags--set-parsed-file :around #'helm-gtags--set-parsed-file-around))

;; isearch/helm-swoop/helm-occur integration.
;; http://emacs.rubikitch.com/helm-swoop-helm-occur/
;;; migemoなしでhelm-swoop
(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  (interactive)
  (let (helm-migemo-mode)
    (helm-swoop :$query $query :$multiline $multiline)))

(defun isearch-forward-or-helm-swoop-or-helm-occur (use-helm-swoop)
  (interactive "p")
  (let (current-prefix-arg
	(helm-swoop-pre-input-function 'ignore))
    (call-interactively
     (cond
       ((eq use-helm-swoop 1) 'isearch-forward)
       ;; C-u C-s -> helm-occur/swoop depending on buffe-size.
       ((eq use-helm-swoop 4) (if (< 1000000 (buffer-size)) 'helm-occur 'helm-swoop))
       ;; C-u C-u C-s -> helm-swoop w/o migemo.
       ((eq use-helm-swoop 16) 'helm-swoop-nomigemo)))))
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop-or-helm-occur)

;; Ediff
;; http://dev.ariel-networks.com/articles/emacs/part7/
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

;; Frame
(setq default-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)))
(require 'my-frame)
(my-frame-set-alpha 80)
(add-hook 'emacs-startup-hook
	  #'(lambda () (my-frame-modify-frame-geometry 0)))
(if (eq system-type 'windows-nt)
    (define-key global-map [remap suspend-frame] 'my-show-or-hide-frame))

;; Misc
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq mode-require-final-newline nil) ;; 'ask
(setq split-height-threshold nil)
(setq split-width-threshold nil)
(eval-after-load "color-moccur"
  '(progn
     (setq moccur-split-word t)))

;; (font-family-list)
;; (x-list-fonts "*")
(defun my-font-family-available (&rest font-families)
  (let ((available-p-list
	 (mapcar #'(lambda (font-family) (if (member font-family (font-family-list)) t nil)) font-families)))
    (eval `(and ,@available-p-list))))
(cond ((my-font-family-available "Ricty Diminished")
       (set-face-attribute 'default nil :family "Ricty Diminished" :height 120)
       (set-fontset-font nil 'japanese-jisx0208 (cons "Ricty Diminished" "iso10646-1"))
       (set-fontset-font nil 'japanese-jisx0212 (cons "Ricty Diminished" "iso10646-1"))
       (set-fontset-font nil 'katakana-jisx0201 (cons "Ricty Diminished" "iso10646-1")))
      ((my-font-family-available "Consolas" (string-as-multibyte "\203\201\203C\203\212\203I"))
       (set-face-attribute 'default nil :family "Consolas" :height 110)
       (set-fontset-font t 'japanese-jisx0208 (font-spec :family "\203\201\203C\203\212\203I")) ; メイリオ
       (add-to-list 'face-font-rescale-alist '(".*\203\201\203C\203\212\203I.*" . 1.1)))
      ((my-font-family-available "Consolas" "KanjiStrokeOrders")
       (set-face-attribute 'default nil :family "Consolas" :height 110)
       (set-fontset-font t 'japanese-jisx0208 (font-spec :family "KanjiStrokeOrders"))
       (add-to-list 'face-font-rescale-alist '(".*KanjiStrokeOrders.*" . 1.2))))

;; Magit
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(with-eval-after-load 'magit
  (set-face-attribute 'magit-branch-current nil :inverse-video t)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-gitflow-popup-key "C-c f"))
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(autoload 'magit-staging "my-magit" nil t)

;; MSVC
(setq my-use-msvc nil)
(when (and my-use-msvc (eq system-type 'windows-nt))
  (require 'cedet.config)
  (require 'flymake.config)
  (require 'setup.auto-complete)
  (require 'setup.yasnippet)
  (require 'setup.ac-clang)
  (setq ac-clang-async-autocompletion-manualtrigger-key "C-<tab>")
  ;; Remove double-quotation in CFLAGS' path (msvc.el treats the double-quoted absolute paths as relative ones).
  (defun msvc-flags--regist-db-around (f &rest args)
    (dolist (cflag (cadr args))
      (while cflag
	(let ((value (car cflag)))
	  (when (string-match "^\"\\(.*\\)\"$" value)
	    (setq value (match-string 1 value))
	    (setq value (replace-regexp-in-string "^\\([a-zA-Z]\\):" (lambda (match) (downcase match)) value t))
	    (setcar cflag value)))
	(setq cflag (cdr cflag))))
    (apply f args))
  (advice-add 'msvc-flags--regist-db :around #'msvc-flags--regist-db-around)

  ;; Display compiler output in English.
  (defun msvc-mode-feature-build-solution/start-process-around (f &rest args)
    (let ((process (apply f args)))
      (and (boundp 'executing-msvc-mode-feature-build-solution)
	   (set-process-coding-system process))
      process))
  (defun msvc-mode-feature-build-solution-around (f &rest args)
    (advice-add 'start-process :around #'msvc-mode-feature-build-solution/start-process-around)
    (let (executing-msvc-mode-feature-build-solution)
      (apply f args))
    (advice-remove 'start-process #'msvc-mode-feature-build-solution/start-process-around))
  (advice-add 'msvc-mode-feature-build-solution :around #'msvc-mode-feature-build-solution-around)

  (require 'setup.msvc)
  (cygwin-mount-deactivate)) ;; @todo Activate mannually after loading finished: (cygwin-mount-activate)
(eval-after-load "yasnippet"
  '(progn
     (define-key yas-minor-mode-map [(C-tab)] 'yas/expand)
     (define-key yas-minor-mode-map [(tab)] nil)
     (define-key yas-minor-mode-map (kbd "TAB") nil)
     (define-key yas-keymap [(C-tab)] 'yas-next-field-or-maybe-expand)
     (define-key yas-keymap (kbd "TAB") nil)
     (define-key yas-keymap [(tab)] nil)
     (yas/global-mode 1)))

;; Company
(autoload 'company-mode-on "company" nil t)
(with-eval-after-load 'company
  ;; Select-next/previous with meta, not ctrl.
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; Show doc with M-d, not C-h.
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  ;; Complete if only one option is available, otherwise select next.
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; Etc.
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-selection-wrap-around t)
  ;; Faces
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgray")
  (set-face-attribute 'company-preview-common nil
                      :foreground "dark gray"
                      :background "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :background "steelblue"
                      :foreground "white")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground "red"))

;; Personal utils
(autoload 'my-diff-buffers "my-diff" nil t)
(autoload 'my-grep "my-grep" nil t)
(autoload 'my-query-replace-multi "my-replace" nil t)

;; Experimental

;; Load the experimental setting file.
(let ((filename (locate-user-emacs-file "init-sub-experimental.el")))
  (if (file-exists-p filename)
      (load-file filename)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ob-async magit magit-gitflow add-node-modules-path color-moccur ddskk git-gutter-fringe recentf-ext cmake-mode company company-irony csv-mode dash diff-hl elpa-mirror git-gutter helm helm-gtags helm-swoop htmlize jedi lsp-mode migemo php-mode py-isort rjsx-mode tide typescript-mode web-mode wgrep yaml-mode gnu-elpa-keyring-update cygwin-mount w3 msvc)))
 '(safe-local-variable-values
   (quote
    ((typescript-indent-level . 2)
     (nxml-child-indent . 1)
     (sgml-basic-offset . 2)
     (sgml-basic-offset . 4)
     (web-mode-script-padding . 2)
     (web-mode-style-padding . 2))))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 50)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)))))
