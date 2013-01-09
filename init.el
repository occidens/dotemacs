;;Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing GitHub Flavored Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;Use gfm-mode when in a wiki directory
(add-to-list 'auto-mode-alist '("wiki.*\\.md\\'" . gfm-mode))

;; Maxima
(add-to-list 'load-path "/opt/local/share/maxima/5.22.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;; Load local stuff
(load "~/.emacs.d/local")

;; Define custom file
(setq custom-file "~/.emacs.d/custom.el")
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
