;;; init-themes.el --- William West's Emacs Theme Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  William West

;; Author: William West <occidens@gmail.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I am currently using [[https://github.com/bbatsov/solarized-emacs][bbatsov's implementation]] instead of
;; [[https://github.com/sellout/emacs-color-theme-solarized][sellout's implementation]] of Solarized for Emacs, since the former has
;; full support for the special faces used by Magit.

;; Portions of this configuration were adapted from
;;  https://github.com/bradleywright/emacs-d/blob/master/init-solarized.el
;;  https://github.com/bradleywright/emacs-d/blob/master/init-osx.el

;;; Code:

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

(defun w/solarized-settings ()
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)

  ;; ;; Don't change the font for some headings and titles
  ;; (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)

  ;; ;; Use less bolding
  ;; (setq solarized-use-less-bold t)

  ;; ;; Use more italics
  ;; (setq solarized-use-more-italic t)

  ;; ;; Use less colors for indicators such as git:gutter, flycheck and similar
  ;; (setq solarized-emphasize-indicators nil)

  ;; ;; Don't change size of org-mode headlines (but keep other size-changes)
  ;; (setq solarized-scale-org-headlines nil)

  ;; ;; Avoid all font-size changes
  ;; (setq solarized-height-minus-1 1)
  ;; (setq solarized-height-plus-1 1)
  ;; (setq solarized-height-plus-2 1)
  ;; (setq solarized-height-plus-3 1)
  ;; (setq solarized-height-plus-4 1)
  )

(defun w/load-solarized ()
  "Load Solarized"
  (w/solarized-settings)
  (load-theme 'solarized-dark t))

(add-hook 'after-init-hook 'w/load-solarized)

(provide 'init-themes)
;;; init-themes.el ends here
