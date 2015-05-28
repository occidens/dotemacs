;; Prefer org-plus-contrib if it is installed
(when (and (package-installed-p 'org)
	   (package-installed-p 'org-plus-contrib))
  (w/filter-load-path 'org))

(require 'org)

;; Global Org-mode Settings
;; See also local/org-settings

;; Set Custom Variables
(customize-setq
 org-export-backends
 '(ascii html icalendar latex md gfm)
 org-use-speed-commands t
 org-export-allow-bind-keywords t)

;; Additional Packages

(require 'ob-table)
(require 'ox-gfm)
(add-to-list 'w/darwin-modules 'org-mac-protocol)

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

;; Refiling Settings (source: http://permalink.gmane.org/gmane.emacs.orgmode/34029)
;; any headline with level <= 2 is a target
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

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

;; Key bindings
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "H-h") 'org-toggle-link-display)))

(provide 'init-org)
