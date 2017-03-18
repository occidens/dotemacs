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

(defun w/unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun w/unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; TODO: Understand exactly how indent is stored with text properties
(defun w/right-indent (from to)
  (interactive "*r")
  (increase-right-margin from to 1)
  (unless auto-fill-function
    (fill-region from to nil t t)))

(defun w/right-dedent (from to)
  (interactive "*r")
  (decrease-right-margin from to 1)
  (unless auto-fill-function
    (fill-region from to nil t t)))

(defun w/ns-copy-unfilled-including-secondary ()
  (interactive)
  (let ((selection (buffer-substring (point) (mark t))))
    (with-temp-buffer
      (insert selection)
      (w/unfill-region (point-min) (point-max))
      (kill-ring-save (point-min) (point-max))
      (ns-store-selection-internal
       'SECONDARY (buffer-substring (point-min) (point-max))))))

(progn
  (global-set-key (kbd "M-Q") 'w/unfill-paragraph)
  (global-set-key (kbd "H-[") 'w/right-indent)
  (global-set-key (kbd "H-]") 'w/right-dedent))

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defun w/scratch ()
  (interactive)
  (let ((scratch-buffer (w/get-scratch-buffer-create)))
    (if (equal (current-buffer) scratch-buffer)
	(switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer))))

(defalias 'scratch #'w/scratch)

(defun w/get-scratch-buffer-create (&optional name)
  "Get or create a scratch buffer in the same way as in `startup.el'

The name of the buffer may be optionally specified with NAME;
otherwise \"*scratch\". Return the buffer."
  (let ((buf (get-buffer-create (or name "*scratch*"))))
    (with-current-buffer buf
      (when (zerop (buffer-size))
	(insert initial-scratch-message)
	(set-buffer-modified-p nil))
      (when (eq major-mode 'fundamental-mode)
	(funcall initial-major-mode)))
    buf))

(defun w/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.

Source: [[http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/][Emacs Redux]]"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'rename-file-and-buffer 'w/rename-file-and-buffer)


(defun w/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting.

Source: [[http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/Emacs][Emacs Redux]]"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defalias 'delete-file-and-buffer 'w/delete-file-and-buffer)

;TODO is this safe given `magit-auto-revert-mode'?
(defun w/touch-file-and-buffer ()
  "Touch file and buffer"
  (interactive)
  (let ((fn (buffer-file-name)))
    (when (and fn
	       (verify-visited-file-modtime (current-buffer))
	       (not (buffer-modified-p)))
      (let ((now (current-time)))
	(and (set-file-times (buffer-file-name) now)
	     (set-visited-file-modtime now))))))

(defalias 'touch-file-and-buffer 'w/touch-file-and-buffer)

(defun w/find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards.

Source [[https://www.emacswiki.org/emacs/FindingNonAsciiCharacters][EmacsWiki]]"
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defalias 'find-first-non-asciaa-char 'w/find-first-non-ascii-char)

(defun w/insert-uuid ()
  (interactive)
  (require 'uuid)
  (insert (replace-regexp-in-string "-" "" (uuid-string))))

(global-set-key (kbd "H-u") 'w/insert-uuid)

(provide 'editing)
;;; editing.el ends here
