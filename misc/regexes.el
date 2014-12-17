(defun keymap-to-mediawiki ()
(while (re-search-forward "[ a-z(-]*[[\"]\\(.*\\)[]\"]\\s-*'\\(.*\\))" nil t)
(replace-match ";\\1:\\2"))