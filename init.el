;; William West's Emacs Configuration
;;
;;

;; Fundamental Utility Functions and Macros

(defmacro w/with-lexical-binding (&rest forms)
  "Evaluate FORMS with lexical binding using `eval'"
  (declare (indent 1) (debug t))
  `(eval '(progn ,@forms) t))

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

(defun w/symbol-to-filename (sym &optional unsafe)
  "Convert SYM to a string with UNSAFE chars removed"
  (let ((unsafe-chars (or unsafe '("/" ":" "\\")))
	(safe-char "-"))
    (replace-regexp-in-string
     (regexp-opt unsafe-chars) safe-char
     (symbol-name sym))))

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
(add-to-load-path "lisp")



;; PID File
;; Adapted from http://devblog.avdi.org/2011/06/17/make-emacs-server-write-a-pid-file/
(defvar w/pid-file nil
  "File holding current PID of this Emacs.

The PID file is written by `w/write-pid-file' on startup and
deleted by `w/delete-pid-file' on shutdown.")

(defun w/write-pid-file ()
  (let ((temporary-file-directory (concat dotfiles-dir "run"))
	(prefix (format "emacs-%s~" emacs-version)))
    (make-directory temporary-file-directory :parents)
    (setq w/pid-file (make-temp-file prefix nil ".pid"))
    (with-temp-file w/pid-file
      (insert (format "%d\n" (emacs-pid))))))

(defun w/delete-pid-file ()
  (when (and w/pid-file (file-exists-p w/pid-file))
    (delete-file w/pid-file)))

(add-hook 'emacs-startup-hook #'w/write-pid-file)
(add-hook 'kill-emacs-hook #'w/delete-pid-file)


;; Functions for later load-path manipulation

(defun w/package-dir (pkg)
  "Return the directory from which PKG is loaded.

Scans `package-alist'"
  (if (fboundp 'package-desc-dir)
      (package-desc-dir (cadr (assq pkg package-alist)))
    (warn "This version of `package.el' does not support `package-desc-dir'")
    nil))

(defun w/filter-load-path (pkg)
  "Remove path associated with PKG from `load-path'"
  (let ((pkg-dir (w/package-dir pkg)))
    (when pkg-dir
      (setq load-path (w/filter-not
		       (apply-partially 'w/same-directory-p
					(w/canonical-path pkg-dir))
		       load-path)))))

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



(put 'upcase-region 'disabled nil)

(setq inhibit-startup-screen t)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Package Setup using Cask and Pallet
(setq package-enable-at-startup nil) ; Ensure packages aren't loaded redundantly
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)			     ; Calls (package-initialize)


;; System-specific initialization

(defmacro w/defsystem (sym)
  (let ((hook (intern (format "w/%s-hook" sym)))
	(modules (intern (format "w/%s-modules" sym)))
	(require-modules (intern (format "w/%s-require-modules" sym)))
	(run--hooks (intern (format "w/run-%s-hooks" sym))))
    `(prog1
	 (defvar ,hook nil
	   ,(format "Hook called on %s systems" sym))
       (defvar ,modules nil
	 ,(format "Packages to require when system is %s" sym))
       (defun ,require-modules ()
	 ,(format "Require modules listed in `%s'" modules)
	 (mapcar (lambda (p) (when p (require p))) ,modules))
       (add-hook ',hook ',require-modules)
       (defun ,run--hooks ()
	 (run-hooks ',hook)))))

(w/defsystem darwin)
(w/defsystem gnu/linux)
;; (w/defsystem windows-nt)
;; (w/defsystem cygwin)

(defun w/initsystem ()
  "Load the system-specific init file an set up hooks.

Based on an original idea from http://irreal.org/blog/?p=1331"
  (let ((sys-name (symbol-name system-type))
	(run-sys-hook (intern (format "w/run-%s-hooks" system-type))))
    (message "Initializing system-specific settings for %s..." sys-name)
    (load (w/symbol-to-filename system-type) t)
    (add-hook 'emacs-startup-hook run-sys-hook)))

(w/initsystem)

;; Trash Setup
(setq delete-by-moving-to-trash t)

;; Dired Setup
(put 'dired-find-alternate-file 'disabled nil)

;; Editing Functions
(require 'editing)

;; Bell Throttling
(defvar w/bell-throttle-value 1)

(defun w/throttle-bell ()
  "Set up throttle for the bell

Adapted from URL `https://gist.github.com/CrypticTemple', using lexical
binding."
  (setq ring-bell-function
	(w/with-lexical-binding
	    (let ((ringable t))
	      (lambda () "Throttle bell ringing to once per second"
		(when ringable
		  (ding)
		  (setq ringable nil)
		  (run-at-time 1 nil (lambda () (setq ringable t)))))))))

(w/throttle-bell)

;; Backup Settings

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
(setq backup-each-save-mirror-location (dotfiles-path "backups"))

;; Based on http://www.emacswiki.org/emacs/BackupDirectory
(setq
 ;; Don't clobber symlinks
 backup-by-copying      t
 ;; Don't litter fs with backup files
 backup-directory-alist `((,tramp-file-name-regexp . nil)
			  (".*" . "~/.saves"))
 ;; Prune old backup versions
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
 ;; Use versioned backups
 version-control        t)

;;Appearance and Workgroups
(require 'init-themes)

(when (and (display-graphic-p)
	   (not (daemonp)))
  (require 'workgroups2)
  (setq wg-session-load-on-start t
	wg-prefix-key (kbd "H-w"))
  (workgroups-mode 1))

;;MODES

;;Org
(require 'init-org)

;; Web Development
(require 'init-web)

;; R
(require 'init-stats)

;;hideshow-org

(require 'hideshow-org)
(global-set-key "\C-ch" 'hs-org/minor-mode)

;; Markdown - use gfm-mode when in a wiki directory
(add-to-list 'auto-mode-alist '("wiki.*\\.md\\'" . gfm-mode))

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
(define-key global-map (kbd "H-c SPC") 'ace-jump-mode)

;; Ruby
;; TODO: review how this interacts with `exec-path-from-shell-initialize'
;;(require 'init-ruby)

;; Search Engines
(require 'init-engine)

;; Git
(require 'init-git)

;; Load local stuff
(add-to-load-path "local")
(unless (require 'local-org nil t) (princ "No org settings found"))
(unless (require 'local-blog nil t) (princ "No blog settings found"))

;; Custom configuration
(require 'initsplit)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;VC Configuration
(setq vc-handled-backends '(SVN Git)
      vc-follow-symlinks t)

(progn
  (autoload 'emr-show-refactor-menu "emr")
  (define-key prog-mode-map (kbd "H-z") 'emr-show-refactor-menu)
  (eval-after-load "emr" '(emr-initialize)))

(require 'multiple-cursors)

;; Protect Emacs and Package sources
;; See this [[http://bit.ly/1JWvZLv][StackExchange post]]
;; Also possible to set up view-mode with (eval . (view-mode 1))

(dir-locals-set-class-variables
 'emacs
 '((nil . ((buffer-read-only . t)))))

;; TODO: work out how to protect `package-user-dir' without blocking
;; package installation.
;; (let ((pkg-dirs (cons package-user-dir package-directory-list)))
(let ((pkg-dirs package-directory-list))
  (mapcar (lambda (dir) (dir-locals-set-directory-class dir 'emacs))
	  pkg-dirs))

;; Prevent orglink mode from lisp-interaction-mode

(advice-add #'turn-on-orglink-mode-if-desired :before-until
	    (lambda () "Do not enable orglink mode in lisp-interaction-mode"
	      (eq major-mode 'lisp-interaction-mode)))

;;Global keys

(progn
  (global-set-key (kbd "H-l") 'org-store-link) ;HyperLink
  ;(global-set-key (kbd "H-h e") 'emacs-index-search)
  ;(global-set-key (kbd "H-h l") 'elisp-index-search)
  ;(global-set-key (kbd "H-h h") 'info-apropos)
  ;; Multiple Cursors
  (global-set-key (kbd "H-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "H-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "H-/") 'mc/mark-all-like-this)
  ;; Buffers
  (global-set-key (kbd "H-k")   'bury-buffer)
  (global-set-key (kbd "H-p p") 'golden-ratio-mode)
  (global-set-key (kbd "H-p w") 'golden-ratio-toggle-widescreen)
  (global-set-key (kbd "H-p a") 'golden-ratio-adjust)
  (global-set-key (kbd "H-=")   'text-scale-increase)
  (global-set-key (kbd "H--")   'text-scale-decrease)
  (global-set-key (kbd "H-0")   (lambda () (interactive) (text-scale-increase 0))))

;;Start server
(if (server-running-p server-name)
    (message "Server not started because server \"%s\" already running"
	     server-name)
  (server-start))

;; Debug errors in startup that occur after this init file is
;; run. Note that the --debug-init command line option only enables
;; debugging for this init file.
(add-hook 'after-init-hook
          (lambda () (setq debug-on-error t)))
(put 'erase-buffer 'disabled nil)
