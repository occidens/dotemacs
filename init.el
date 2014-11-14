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

;;orgmode-mediawiki
(add-to-list 'load-path "~/.emacs.d/code/gemein/orgmode-mediawiki")
(require 'ox-mediawiki)

;;mediawiki-mode
(add-to-list 'load-path "~/.emacs.d/modes/mediawiki-mode")
(require 'mediawiki)

;;php-mode
(add-to-list 'load-path "~/.emacs.d/modes/php-mode")
(require 'php-mode)

;;outline mode easy bindings
;;TODO: also review https://github.com/tj64/outshine
(add-to-list 'load-path "~/.emacs.d/misc")
(add-hook 'outline-mode-hook 'my-outline-easy-bindings)
(add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
(defun my-outline-easy-bindings ()
       (require 'outline-mode-easy-bindings nil t))

;; Load local stuff
;;(load "~/.emacs.d/local")

;; Define custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
