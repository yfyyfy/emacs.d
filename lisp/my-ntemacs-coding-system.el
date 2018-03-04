;;; https://www49.atwiki.jp/ntemacs/pages/16.html

(require 'cl)

;; IME の設定をした後には実行しないこと
;; (set-language-environment 'Japanese)

;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))
(setq process-coding-system-alist (delq (assoc "bash" process-coding-system-alist) process-coding-system-alist))

;; ldd の結果のキャッシュ
(defvar ldd-cache nil)

;; filename が cygwin のプログラムかどうか判定する
(defun cygwin-program-p (filename)
  (let ((target (and filename (executable-find filename))))
    (when target
      (cdr (or (assoc target ldd-cache)
	       (car (push (cons target
				(with-temp-buffer
				  (let ((w32-quote-process-args nil)) ; advice 中で再帰しないよう nil
				    ;; cygwin のライブラリをロードしているか判定
				    (when (eq (call-process "ldd" nil t nil (concat "\"" target "\"")) 0)
				      (goto-char (point-min))
				      (number-or-marker-p
				       (re-search-forward "cygwin[0-9]+\.dll" nil t))))))
			  ldd-cache)))))))

;; サブプロセスに渡すパラメータに SJIS のダメ文字対策を行い、さらに文字コードを cp932 にする
(defun convert-process-args (orig-fun prog-pos args-pos args)
  (let ((cygwin-quote (and w32-quote-process-args ; cygwin-program-p の再帰防止
			   (cygwin-program-p (nth prog-pos args)))))
    (if (nthcdr args-pos args)
	(setf (nthcdr args-pos args)
	      (mapcar (lambda (arg)
			(when w32-quote-process-args
			  (setq arg
				(concat "\""
					(if cygwin-quote
					    (replace-regexp-in-string "[\"\\\\]"
								      "\\\\\\&"
								      arg)
					  (replace-regexp-in-string "\\(\\(\\\\\\)*\\)\\(\"\\)"
								    "\\1\\1\\\\\\3"
								    arg))
					"\"")))
			(if (multibyte-string-p arg)
			    (encode-coding-string arg 'cp932)
			  arg))
		      (nthcdr args-pos args)))))

  (let ((w32-quote-process-args nil))
    (apply orig-fun args)))

(cl-loop for (func prog-pos args-pos) in '((call-process	0 4)
					   (call-process-region 2 6)
					   (start-process	2 3))
	 do (eval `(advice-add ',func
			       :around (lambda (orig-fun &rest args)
					 (convert-process-args orig-fun
							       ,prog-pos ,args-pos
							       args))
			       '((depth . 99)))))

(provide 'my-ntemacs-coding-system)
