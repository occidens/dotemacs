;;Set up solarized dark and light themes
;;Adapted from https://github.com/bradleywright/emacs-d/blob/master/init-solarized.el
;;and https://github.com/bradleywright/emacs-d/blob/master/init-osx.el
;;For use with https://github.com/sellout/emacs-color-theme-solarized
;;TODO: examine the difference between sellout's theme and https://github.com/bbatsov/solarized-emacs
;;TODO: revise the functions now that there is a single theme with light and dark variants

(defun w/toggle-theme (theme-1 theme-2)
  "Turn off the current theme and turn on a new one. Order of args does not matter"
  (cond
   ((custom-theme-enabled-p theme-1)
    (w/switch-theme theme-2 theme-1))
   ((custom-theme-enabled-p theme-2)
    (w/switch-theme theme-1 theme-2))))

(defun w/switch-theme (theme-to-enable theme-to-disable)
  "Enable one theme and disable the other"
  (disable-theme theme-to-disable)
  (when (custom-theme-enabled-p theme-to-enable)
    (enable-theme theme-to-enable))
  (load-theme theme-to-enable t))

(defun w/toggle-solarized ()
  "Toggles between solarized light and dark"
  (interactive)
  (w/toggle-theme 'solarized-light 'solarized-dark))

(defun w/load-solarized ()
  "Load Solarized"
  (load-theme 'solarized t))

(add-hook 'after-init-hook 'w/load-solarized)

(provide 'init-solarized)
