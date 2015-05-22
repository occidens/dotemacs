;;; init-git.el --- Git-related initialization       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  William West

;; Author: William West <occidens@quindecim.local>
;; Keywords: vc

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

;; 

;;; Code:

(global-magit-wip-save-mode t)

(unless (member "wip-save" (magit-get-all "magit.extension"))
  (magit-run-git "config" "--global" "--add" "magit.extension" "wip-save"))

(if (display-graphic-p)
    (require 'git-gutter-fringe)
  (require 'git-gutter))

(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)

(progn
  (global-set-key (kbd "H-g g")   'magit-status)   ;HyperGit!
  (global-set-key (kbd "H-g x")   'magit-git-command)
  ;; git time machines
  (global-set-key (kbd "H-g t w") 'git-wip-timemachine-toggle)
  (global-set-key (kbd "H-g t t") 'git-timemachine-toggle)
  ;; git-gutter
  (global-set-key (kbd "H-g t g") 'git-gutter:toggle)
  (global-set-key (kbd "H-g /")	  'git-gutter:popup-hunk)
  (global-set-key (kbd "H-g ,")	  'git-gutter:previous-hunk)
  (global-set-key (kbd "H-g .")   'git-gutter:next-hunk))

(provide 'init-git)
;;; init-git.el ends here
