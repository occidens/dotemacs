;; William West's Emacs Configuration

;; Assign Mac fn key to hyper
;; See http://irreal.org/blog/?p=1450 and
;;     http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
(setq ns-function-modifier 'hyper)

;;Spelling
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/opt/local/bin")

;;Source Directory
;;TODO resolve version
(setq source-directory "~/Code/gemein/Machinor/emacs-24.4")
