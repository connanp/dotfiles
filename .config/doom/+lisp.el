;;; +lisp.el -*- lexical-binding: t; -*-

(after! smartparens
  (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
  (sp-local-pair '(sly-mrepl-mode) "`" "`" :actions nil))

(setq sly-kill-without-query t
      sly-repl-history-remove-duplicates t
      sly-repl-history-trim-whitespaces t
      sly-complete-symbol-function 'sly-flex-completions
      sly-net-coding-system 'utf-8-unix)

(setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode))

(provide '+lisp)

;;; +lisp.el ends here
