(defun outlookify ()
  (interactive)
  (replace-string "\n" ";" nil (point-min) (point-max)))

(defun outlookify2 ()
  (interactive)
  (save-excursion
    (setq the-start (region-beginning))
    (setq the-end (region-end))
    (goto-char the-start)
    (while (search-forward "\n" the-end t) (replace-match ";" nil t))
    )
)

(defun sort-usernames (reverse beg end)
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse 'forward-line 'end-of-line 
		   (lambda () (forward-char 2)))))))
  
