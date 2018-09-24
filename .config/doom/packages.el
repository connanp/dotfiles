;; -*- no-byte-compile: t; -*-
;;; private/cfg/packages.el

;; using the patched version of Emacs from bin/doom patch-macos feature.
(when IS-MAC
  (package! exec-path-from-shell :disable t))

(when (featurep! :lang common-lisp)
  (package! common-lisp-snippets))

(package! ob-async)
(package! dired-du)
(package! default-text-scale)
(package! vlf)
(package! s)
(package! request)
(package! request-deferred)
(package! org-jira)
(package! em-smart)