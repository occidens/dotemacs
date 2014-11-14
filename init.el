;;Base Load Path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customized Emacs configuration")

(add-to-list 'load-path (concat dotfiles-dir "init"))

;;Spelling
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/opt/local/bin")

(put 'upcase-region 'disabled nil)

(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

;; Set up ELPA
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

;; Backup Settings
;; Based on http://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '((".*" . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;Themes
(require 'init-solarized)

;;MODES

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


;; Ergoemacs
;;(add-to-list 'load-path "~/.emacs.d/modes/ergoemacs-mode")
;;(require 'ergoemacs-mode)
;;(setq ergoemacs-theme nil) ;; Use Standard kbd theme
;;(setq ergoemacs-keyboard-layout "us") ;; QWERTY
;;(ergoemacs-mode 1)
;; There is no menu key on the mac
;; Map Caps Lock to <f13> with PCKeyboardHack
;; https://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en
;; Then map <f13> to <menu>
;;(define-key key-translation-map (kbd "<f13>") (kbd "<menu>"))


;; Load local stuff
(load "~/.emacs.d/local/local.el")

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


;; Org-mode Settings

(setq org-export-allow-bind-keywords t)

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

;;Restrict backends handled by VC
(setq vc-handled-backends '(SVN Git))
