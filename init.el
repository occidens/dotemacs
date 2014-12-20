;; William West's Emacs Configuration
;;
;;

;;Base Load Path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customized Emacs configuration")

(defsubst add-to-load-path (dir)
  (add-to-list 'load-path (concat dotfiles-dir dir)))

(add-to-load-path "init")
(add-to-load-path "systems")

(load (symbol-name system-type) t)	;After http://irreal.org/blog/?p=1331

;;Spelling
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/opt/local/bin")

(put 'upcase-region 'disabled nil)

(setq delete-by-moving-to-trash t)
(setq inhibit-startup-screen t)

;; Package Setup using Cask and Pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
;; Currently pallet-mode clobbers non-ELPA recipes
;; See https://github.com/rdallasgray/pallet/issues/32
;;(pallet-mode t)

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
