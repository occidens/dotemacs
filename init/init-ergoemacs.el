;; Ergoemacs
(add-to-list 'load-path "~/.emacs.d/modes/ergoemacs-mode")
(require 'ergoemacs-mode)
(setq ergoemacs-theme nil) ;; Use Standard kbd theme
(setq ergoemacs-keyboard-layout "us") ;; QWERTY
(ergoemacs-mode 1)
;; There is no menu key on the mac
;; Map Caps Lock to <f13> with PCKeyboardHack
;; https://pqrs.org/macosx/keyremap4macbook/pckeyboardhack.html.en
;; Then map <f13> to <menu>
;;(define-key key-translation-map (kbd "<f13>") (kbd "<menu>"))

(provide 'init-ergoemacs)
