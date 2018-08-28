;;; init.el -*- lexical-binding: t; -*-

 ;; support font glyphs on mac with railwaycat's port
 (when (and (eq system-type `darwin) (window-system))
   (mac-auto-operator-composition-mode))

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a
buffer. If the buffer is one of the `bad-cleanup-modes' then no
re-indenting and un-tabification is done."
    (interactive)
    (unless (member major-mode bad-cleanup-modes)
      (progn
        (indent-buffer)
        (untabify-buffer)))
    (delete-trailing-whitespace))

  (defun ckp/smart-to-ascii (beg end)
    "Replace smart quotes and dashes with their ASCII equivalents"
    (interactive "r")
    (format-replace-strings smart-to-ascii
                            nil beg end))
