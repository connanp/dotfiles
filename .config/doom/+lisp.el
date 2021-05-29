;;; +lisp.el -*- lexical-binding: t; -*-

(use-package! sly
  :defer-incrementally t
  :after lispyville
  :config
  (setq sly-kill-without-query t
        sly-repl-history-remove-duplicates t
        sly-repl-history-trim-whitespaces t
        sly-complete-symbol-function 'sly-flex-completions
        sly-net-coding-system 'utf-8-unix)

  (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
  (sp-local-pair '(sly-mrepl-mode) "`" "`" :actions nil))

;; (defun insert-literal-left-bracket (fun arg)
;;   "Advice for inserting `[` instead of the movement when inside a string."
;;   (if (inside-string-q)
;;       (lispy-wrap-brackets arg)
;;     (funcall fun arg)))

;; (defun insert-literal-right-bracket (fun arg)
;;   "Advice for inserting `]` instead of the movement when inside a string."
;;   (if (inside-string-q)
;;       (lispy-wrap-brackets arg)
;;     (funcall fun arg)))

;; I thought that not being able to insert [] easily in strings/etc would annoy me.
;; Turns out that I could just get used to hitting S-]
;; (after! lispy
;;   (advice-add #'lispy-backward :around #'insert-literal-left-bracket)
;;   (advice-add #'lispy-forward :around #'insert-literal-right-bracket))

;; (use-package! lispy
;;   :config
;;   ;; default includes special which has movement keys in insert mode
;;   (lispy-set-key-theme '(lispy c-digits)))

(use-package! lispyville
  :after-call after-find-file
  :defer-incrementally t
  :config
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     (prettify insert)
     (atom-movement normal visual)
     slurp/barf-lispy
     (wrap normal insert)
     additional
     additional-insert
     (additional-movement visual motion)
     (additional-wrap normal insert)
     (escape insert)))
  ;; use evil visual mode
  (lispyville-enter-visual-when-marking))

(provide '+lisp)

;;; +lisp.el ends here
