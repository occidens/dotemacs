;;; editing.el --- William West's Collection of Editing Commands

;; Copyright (C) 2015 William West

;; Author: William West <occidens@gmail.com>
;; Keywords: convenience

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

(defun offset-to (move-func)
  (let ((start (point)))
    (funcall move-func)
    (- (point) start)))

(defun move-defun-forward ()
  (interactive)
  (let ((offset (offset-to 'end-of-defun)))
    (transpose-sexps 1)
    (end-of-defun)
    (backward-char offset)))
	
(defun move-defun-backward ()
  (interactive)
  (let ((offset (offset-to 'beginning-of-defun)))
    (transpose-sexps 1)
    (beginning-of-defun 2)
    (backward-char offset)))

(defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max)))
        (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(provide 'editing)
;;; editing.el ends here
