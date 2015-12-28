;;; init-web.el --- Wm. West Emacs Config - Web Development

;; Copyright (C) 2015  William West

;; Author: William West <occidens@gmail.com>
;; Keywords: languages, lisp

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

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'indent-guide-mode) ;See also highlight-indentation-mode
(add-hook 'web-mode-hook 'indent-guide-mode)
(setq-default js-indent-level 2)

(defun w/web-mode-init ()
  ;; (setq-local indent-tabs-mode nil)
  ;; (setq-local tab-width 4)
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'w/web-mode-init)

(provide 'init-web)
;;; init-web.el ends here
