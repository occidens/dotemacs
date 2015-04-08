;;; kalends.el --- Calendrical Functions             -*- lexical-binding: t; -*-

;; Copyright (C) 2015  William West

;; Author: William West <occidens@gmail.com>
;; Keywords: calendar

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

;; Calendrical functions
;; For use in diary-style sexp entries
(defun w/weekday (date)
  (memq (calendar-day-of-week date) '(1 2 3 4 5)))

(defun w/weekend (date)
  (memq (calendar-day-of-week date) '(0 6)))

(defmacro w/defdays ()
  (let* ((days-of-week
	  '(sunday monday tuesday wednesday thursday friday saturday))
	 defuns				;so we can use in dolist
	 (expansion
	  (dotimes (d 7 defuns)
	    (let* ((day-name
		    (nth d days-of-week))
		   (fun-name
		    (intern (format "ww/%s" day-name)))
		   (fun-docstring
		    (concat "Return t if DATE is a "
			    (capitalize (format "%s" day-name)))))
	      (setq defuns
		    (cons
		     `(defun ,fun-name (date)
			,fun-docstring
			(eq (calendar-day-of-week date) ,d))
		     defuns))))))
    `(progn ,@expansion)))

(ww/defdays)

(provide 'kalends)
;;; kalends.el ends here
