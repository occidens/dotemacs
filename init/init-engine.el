(require 'engine-mode)

(customize-setq engine/keymap-prefix "H-s")

(defengine orglist
  "http://search.gmane.org/?query=%s&group=gmane.emacs.orgmode"
  "o l")

(defengine worg
  "http://www.google.com/cse?cx=002987994228320350715%3Az4glpcrritm&ie=UTF-8&q=%s&sa=Search&siteurl=orgmode.org%2Fworg%2F&ref=orgmode.org%2F&ss=1119j179857j8"
  "o w")

(defun w/gmail-correspondent-query (name)
  (let ((encname (url-hexify-string name)))
  (format "to%%3A%s+OR+from%%3A%s" encname encname)))

(defengine gmail-correspondent
  "https://mail.google.com/mail/#search/to%%3A%s+OR+from%%3A%s"
  "g c")

(provide 'init-engine)
