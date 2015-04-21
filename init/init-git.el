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

(progn
  (global-set-key (kbd "H-g g") 'magit-status)   ;HyperGit!
  (global-set-key (kbd "H-g C-t") 'git-wip-timemachine-toggle)
  (global-set-key (kbd "H-g t") 'git-timemachine-toggle))


(provide 'init-git)
;;; init-git.el ends here
