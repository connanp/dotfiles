;;; private/cfg/autoload/counsel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun counsel-yank-zsh-history ()
        "Yank the zsh history"
        (interactive)
        (let (hist-cmd collection val)
          (shell-command "history -r") ; reload history
          (setq collection
                (nreverse
                 (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                                 (buffer-string))
                               "\n"
                               t)))

          (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection)) ;; for zsh

          (when (and collection (> (length collection) 0)
                     (setq val (if (= 1 (length collection)) (car collection)
                                 (ivy-read (format "Zsh history:") collection))))
            ;; (setq val (replace-regexp-in-string "^:[^;]*;" "" val))
            ;; (setq val (replace-regexp-in-string ".*;" "" val))
            (kill-new val)
            (message "%s => kill-ring" val))))
