;; William West's Emacs Configuration
;;
;;

;; Fundamental Utility Functions
(defun w/filter (condp lst)
  "Rudimentary filter.

Delete elements in LST for which CONDP returns nil or which
are nil to begin with.

Source: https://github.com/hrs/dotfiles"
  (delq nil
	(mapcar #'(lambda (x) (and (funcall condp x) x)) lst)))

(defun w/filter-not (condp lst)
  ""
  (delq nil
	(mapcar #'(lambda (x)
		    (and (not (funcall condp x)) x)) lst)))

(defun w/canonical-path (path)
  "Return the canonical path of PATH.

Returned string is guaranteed to contain a trailing path
separator due to the call to `file-name-as-directory'"
  (file-truename (file-name-as-directory path)))

(defun w/same-directory-p (canonical-path other-path)
  "Return t if OTHER-PATH points to the same directory as CANONICAL-PATH.

Assumes that CANONICAL-PATH has been verified with `w/canonical-path'"
  (string-equal canonical-path (w/canonical-path other-path)))

;;Base Load Path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customized Emacs configuration")

(defsubst add-to-load-path (dir)
  (add-to-list 'load-path (concat dotfiles-dir dir)))

(defsubst dotfiles-path (path)
  (concat dotfiles-dir path))

(add-to-load-path "init")
(add-to-load-path "systems")

;; Functions for later load-path manipulation

(defun w/package-dir (pkg)
  "Return the directory from which PKG is loaded.

Scans `package-alist'"
  (package-desc-dir (cadr (assq pkg package-alist))))

(defun w/filter-load-path (pkg)
  "Remove path associated with PKG from `load-path'"
  (let ((pkg-dir (w/canonical-path (w/package-dir pkg))))
    (setq load-path (w/filter-not
		     (apply-partially 'w/same-directory-p pkg-dir)
		     load-path))))

;; Macro for setting customizations
;; Refer to the following for more information on this problem
;; - http://lists.gnu.org/archive/html/help-gnu-emacs/2013-08/msg00543.html
;; - http://stackoverflow.com/questions/18542892/how-do-i-programatically-set-a-custom-variable-in-emacs-lisp/18552615#18552615
;; - http://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
;; - http://ergoemacs.org/emacs/emacs_custom_system.html
(defmacro customize-setq (&rest forms)
  (let ((tail forms)
	(comment
	 (format "Set by call to `customize-setq' in %s, %s"
		 (buffer-file-name) (what-line)))
	sym val acc)
    (while tail
      (setq sym (car tail)
	    val (cadr tail)
	    tail (cddr tail)
	    acc (append acc `((customize-set-variable ',sym ,val ,comment)))))
    `(progn ,@acc)))

;; Load system-specific configuration
;; See http://irreal.org/blog/?p=1331
(load (symbol-name system-type) t)

(put 'upcase-region 'disabled nil)

(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

;; Package Setup using Cask and Pallet
(setq package-enable-at-startup nil) ; Ensure packages aren't loaded redundantly
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)			     ; Calls (package-initialize)

;; Backup Settings

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
(setq backup-each-save-mirror-location (dotfiles-path "backups"))

;; Based on http://www.emacswiki.org/emacs/BackupDirectory
(setq
 ;; Don't clobber symlinks
 backup-by-copying      t
 ;; Don't litter fs with backup files
 backup-directory-alist '((".*" . "~/.saves"))
 ;; Prune old backup versions
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
 ;; Use versioned backups
 version-control        t)

;;Themes
(require 'init-solarized)

;;MODES

;;Org
(require 'init-org)

;;hideshow-org

(require 'hideshow-org)
(global-set-key "\C-ch" 'hs-org/minor-mode)

;;Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing GitHub Flavored Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;Use gfm-mode when in a wiki directory
(add-to-list 'auto-mode-alist '("wiki.*\\.md\\'" . gfm-mode))

;;Dos-Mode
(add-to-list 'load-path "~/.emacs.d/modes/dos-mode")
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;; Maxima
(add-to-list 'load-path "/opt/local/share/maxima/5.22.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;;YAML
(add-to-list 'load-path "~/.emacs.d/modes/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;Org-journal
(add-to-list 'load-path "~/.emacs.d/org/org-journal")
(require 'org-journal)

;;Org-octopresss
(add-to-list 'load-path "~/.emacs.d/org/org-octopress")
(require 'ox-jekyll)

;;orgmode-mediawiki
(add-to-list 'load-path "~/.emacs.d/code/gemein/orgmode-mediawiki")
(require 'ox-mediawiki)

;;mediawiki-mode
(add-to-list 'load-path "~/.emacs.d/modes/mediawiki-mode")
(require 'mediawiki)

;;outline mode easy bindings
;;TODO: also review https://github.com/tj64/outshine
(add-to-list 'load-path "~/.emacs.d/misc")
(add-hook 'outline-mode-hook 'my-outline-easy-bindings)
(add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
(defun my-outline-easy-bindings ()
       (require 'outline-mode-easy-bindings nil t))

;;Ergoemacs
;;TODO: Sort out key mapping problems and give it another go
;;(require 'init-ergoeamcs)

;;ace-jump-mode
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "H-c SPC") 'ace-jump-mode)

;; Ruby
(require 'init-ruby)

;; Search Engines
(require 'engine-mode)

(defengine orglist
  "http://search.gmane.org/?query=%s&group=gmane.emacs.orgmode"
  "o l")

(defengine worg
  "http://www.google.com/cse?cx=002987994228320350715%3Az4glpcrritm&ie=UTF-8&q=%s&sa=Search&siteurl=orgmode.org%2Fworg%2F&ref=orgmode.org%2F&ss=1119j179857j8"
  "o w")

;; Load local stuff
(add-to-list 'load-path "~/.emacs.d/local")
(unless (require 'org-settings nil t) (princ "No org settings found"))
(unless (require 'blog-settings nil t) (princ "No blog settings found"))

;; Define custom file
(setq custom-file "~/.emacs.d/local/custom.el")
(load custom-file)

;; (cond ((< emacs-major-version 22)
;;             ;; Emacs 21 customization.
;;             (setq custom-file "~/.custom-21.el"))
;;            ((and (= emacs-major-version 22)
;;                  (< emacs-minor-version 3))
;;             ;; Emacs 22 customization, before version 22.3.
;;             (setq custom-file "~/.custom-22.el"))
;;            (t
;;             ;; Emacs version 22.3 or later.
;;             (setq custom-file "~/.emacs-custom.el")))
     
;;      (load custom-file)


;;Restrict backends handled by VC
(setq vc-handled-backends '(SVN Git))

;;Global keys

(global-set-key (kbd "H-g") 'magit-status)   ;HyperGit!
(global-set-key (kbd "H-l") 'org-store-link) ;HyperLink
