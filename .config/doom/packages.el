;; -*- no-byte-compile: t; -*-
;;; private/cfg/packages.el

;; using the patched version of Emacs from bin/doom patch-macos feature.
(when IS-MAC
  (package! exec-path-from-shell :disable t))

(when (featurep! :lang common-lisp)
  (package! common-lisp-snippets))

(when (featurep! :lang markdown +pandoc)
  (package! ox-gfm))

(package! esh-autosuggest)
(package! circadian)
;; (package! ob-async)
(package! dired-du)
(package! default-text-scale)
(package! vlf)
(package! s)
(package! request)
(package! request-deferred)
;; (package! org-jira)
(package! magithub :disable t)
(package! magit-todos :disable t)
(package! eshell-did-you-mean :disable t)

(package! counsel-tramp)
(package! docker-tramp)

(when (featurep! :lang go)
  (package! go-impl))

(when (featurep! :ui emoji)
  (package! emoji-cheat-sheet-plus))
(package! alert)
(package! slack)

;; https://github.com/hlissner/doom-emacs/issues/4498
(package! benchmark-init)

(if (file-exists-p (expand-file-name "~/local-pkgs.el"))
    (load! (expand-file-name "~/local-pkgs.el")))
