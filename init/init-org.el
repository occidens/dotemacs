;; Prefer org-plus-contrib if it is installed
(when (and (package-installed-p 'org)
	   (package-installed-p 'org-plus-contrib))
  (w/filter-load-path 'org))

(require 'org)

;; Global Org-mode Settings
;; See also local/org-settings

;; Always enable certain modes with org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Set Custom Variables
(customize-setq
 org-export-backends
 '(ascii html icalendar latex md gfm)
 org-use-speed-commands t
 org-export-allow-bind-keywords t)

;; Additional Packages

(require 'ob-table)
(require 'ox-gfm)
(when (display-graphic-p)
  (add-to-list 'w/darwin-modules 'org-mac-protocol))

;; Don't prompt when evaluating lisp code blocks
(defun ww/org-confirm-babel-evaluate (lang body)
  "Return t to ask and nil to not ask"
  (not (or (string= lang "ruby") (string= lang "emacs-lisp"))))

(setq org-confirm-babel-evaluate 'ww/org-confirm-babel-evaluate)

;;
;; (defadvice org-sbe (around get-err-msg activate)
;;   "Issue messages at errors"
;;   (condition-case err
;;       (progn ad-do-it
;; 	     ((error debug)
;; 	      (message "Error in org-sbe: %S" err)
;; 	      (signal (car err) (cdr err))))))

(defun w/org-buffer-files ()
  "Return list of opened orgmode buffer files"
  (mapcar #'buffer-file-name (org-buffer-list 'files t)))

;; Refiling Settings (source: http://permalink.gmane.org/gmane.emacs.orgmode/34029)
;; any headline with level <= 2 is a target
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)
			   (w/org-buffer-files :maxlevel . 2)))

;; provide refile targets as paths, including the file name
;; (without directory) as level 1 of the path
(setq org-refile-use-outline-path 'file)

;; allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; refile only within the current buffer
(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

;; Link Prefixes
;; TODO: Define `f~' and `fv' based on system-specific paths

(setq org-link-abbrev-alist
      `(("f"  . "file:%s")
	("f~" . "file+sys:~/%s")
	("fs" . "file+sys:%s")
	("fv" . "file+sys:/Volumes/%s")
	("fe" . "file+emacs:%s")))

;; Additional Link Types

(org-add-link-type "zotero" (lambda (path)
			      (browse-url (concat "zotero:" path))))

;; Spell checking

(defun w/org-ispell-skip-setup ()
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist
	       (list (org-make-options-regexp '("OPTIONS" "STARTUP")))))

(add-hook 'org-mode-hook 'w/org-ispell-skip-setup)

(add-hook 'org-mode-hook 'auto-fill-mode)

;; Key bindings

(defun w/org-set-key-bindings ()
  (local-set-key (kbd "H-t t") 'org-toggle-link-display)
  (local-set-key (kbd "H-t p") 'org-toggle-pretty-entities))

(add-hook 'org-mode-hook #'w/org-set-key-bindings)

(setq org-export-async-debug t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-orglink-mode t)
 '(org-export-allow-bind-keywords t)
 '(org-export-backends
   (quote
    (org gfm md latex icalendar html ascii jekyll)))
 '(org-footnote-auto-adjust t)
 '(org-jekyll-include-yaml-front-matter t)
 '(org-latex-packages-alist
   (quote
    (("" "color" nil)
     ("" "listings" nil))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m org-elisp-symbol)))
 '(org-mouse-features
   (quote
    (context-menu move-tree yank-link activate-stars activate-bullets activate-checkboxes)))
 '(org-src-fontify-natively nil)
 '(org-src-window-setup
   (quote other-window))
 '(org-use-speed-commands t)
 '(orglink-activate-in-modes
   (quote
    (emacs-lisp-mode ruby-mode)))
 '(orglink-mode-lighter " Lnk"))

(provide 'init-org)
