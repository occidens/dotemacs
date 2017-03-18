(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(dired-use-ls-dired nil)
 '(engine/keymap-prefix "H-s")
 '(ergoemacs-mode-used "5.14.01-0")
 '(ergoemacs-theme nil)
 '(frame-background-mode
   (quote dark))
 '(frame-resize-pixelwise t)
 '(git-annex-commit nil)
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(user-\\|org-agenda-files\\|org-vcard-\\|org-journal-\\)" "~/.emacs.d/local/local.el" nil t)
     ("\\`\\(markdown-\\)" "~/.emacs.d/init/init-markdown.el" nil t)
     ("\\`\\(org-\\|deft-\\|\\(global-\\)?orglink-\\)" "~/.emacs.d/init/init-org.el" nil nil))))
 '(initsplit-pretty-print t)
 '(js2-basic-offset 2)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount
   (quote
    (1
     ((shift)
      . 1)
     ((control)))))
 '(rmail-user-mail-address-regexp nil)
 '(safe-local-variable-values
   (quote
    ((eval when
	   (and
	    (buffer-file-name)
	    (file-regular-p
	     (buffer-file-name))
	    (string-match-p "^[^.]"
			    (buffer-file-name)))
	   (emacs-lisp-mode)
	   (when
	       (fboundp
		(quote flycheck-mode))
	     (flycheck-mode -1))
	   (unless
	       (featurep
		(quote package-build))
	     (let
		 ((load-path
		   (cons ".." load-path)))
	       (require
		(quote package-build))))
	   (package-build-minor-mode)
	   (set
	    (make-local-variable
	     (quote package-build-working-dir))
	    (expand-file-name "../working/"))
	   (set
	    (make-local-variable
	     (quote package-build-archive-dir))
	    (expand-file-name "../packages/"))
	   (set
	    (make-local-variable
	     (quote package-build-recipes-dir))
	    default-directory))
     (eval font-lock-add-keywords nil
	   (quote
	    (("defexamples\\|def-example-group\\| => \\| !!> \\| ~>"
	      (0
	       (quote font-lock-keyword-face)))
	     ("(defexamples[[:blank:]]+\\(.*\\)"
	      (1
	       (quote font-lock-function-name-face))))))
     (eval font-lock-add-keywords nil
	   (quote
	    (("defexamples\\|def-example-group\\| => \\| !!> "
	      (0
	       (quote font-lock-keyword-face)))
	     ("(defexamples[[:blank:]]+\\(.*\\)"
	      (1
	       (quote font-lock-function-name-face)))))))))
 '(show-paren-mode t)
 '(solarized-distinct-doc-face t)
 '(sr-windows-default-ratio 80)
 '(tool-bar-style
   (quote text))
 '(vc-make-backup-files t)
 '(wg-prefix-key "(kbd \"H-w\")")
 '(wg-remember-frame-for-each-wg t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(form-feed-line
   ((t
     (:strike-through "#586e75")))))
