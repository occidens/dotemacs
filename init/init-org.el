(require 'org)

;; Global Org-mode Settings
;; See also local/org-settings

;; Additional Packages

(require 'ob-table)

(setq org-export-allow-bind-keywords t)


;; Don't prompt when evaluating lisp code blocks
(defun ww/org-confirm-babel-evaluate (lang body)
  "Return t to ask and nil to not ask"
  (not (string= lang "emacs-lisp")))

(setq org-confirm-babel-evaluate 'ww/org-confirm-babel-evaluate)

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

(provide 'init-org)
