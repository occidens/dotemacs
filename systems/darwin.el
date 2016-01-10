;; William West's Emacs Configuration

;; Modifier Key Configuration
;;
;; The following configuration enables reasonable interoperability
;; between the Mac keyboard and Das keyboard
;;
;; Caps Lock and Right Control = Control
;; Control Fn and Left Control = Meta
;;
;; To implement, [[https://pqrs.org/osx/karabiner/seil.html.en][Seil]] is used to map Caps Lock to Right Control
;;
;; References
;; - http://stevelosh.com/blog/2012/10/a-modern-space-cadet/
;; - http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; - http://irreal.org/blog/?p=145
;; - http://msol.io/blog/tech/2014/03/10/work-more-efficiently-on-your-mac-for-developers
;;
;; TODO: check that Seil is running with pgrep Seil
;; TODO: Do we want this when we are running in a terminal?
;;
(setq ns-function-modifier      'hyper
      ns-control-modifier       'hyper
      ns-right-control-modifier 'control)

;; Set exec path from shell
(exec-path-from-shell-initialize)

;;Spelling
(setq ispell-program-name "aspell")

;; Trashing
(osx-trash-setup)

;;Source Directory
;;TODO resolve version
(setq source-directory "~/Code/gemein/Machinor/emacs-24.4")
