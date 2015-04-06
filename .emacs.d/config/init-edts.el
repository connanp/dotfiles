;; disable ac mode cause it sucks
(setq edts-complete-inhibit t)

(require-package 'edts)

(add-hook 'after-init-hook 'load-edts)
(defun load-edts ()
  (require 'edts-start))

(provide 'init-edts)
