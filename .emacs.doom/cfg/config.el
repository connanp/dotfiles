;;; private/cfg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+so-long")

(after! so-long
  :config
  (so-long-enable)
  (setq so-long-minor-modes (append '(rainbow-delimiters-mode) so-long-minor-modes)))

(setq doom-theme 'doom-outrun
      doom-outrun-brighter-comments nil
      doom-outrun-comment-bg nil
      doom-themes-padded-modeline nil)

(when IS-MAC
  (setq mac-mouse-wheel-smooth-scroll t))

(setq text-scale-mode-step 1.05
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 14 :weight 'medium)
      doom-font (font-spec :family "Iosevka" :size 14 :weight 'medium)
      doom-big-font (font-spec :family "Iosevka" :size 19 :weight 'light)
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-treemacs-enable-variable-pitch t)

(setq-default line-spacing 0.1)

(after! magit
  :config
  (add-hook 'magit-revision-mode-hook (lambda () (setq line-spacing 0))))

(when (member "Iosevka" (font-family-list))
  (setq doom-font (font-spec :family "Iosevka" :size 14)))

;; emacs can already page.
(setenv "PAGER" "cat")

(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'du 'dired-du-mode)
(defalias 'fd 'find-dired)
(defalias 'gd 'magit-diff-unstaged)
(defalias 'gds 'magit-diff-staged)
(defalias 'gl 'magit-log)
(defalias 'lml 'list-matching-lines)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rb 'revert-buffer)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)
(defalias 'sh 'shell)
(defalias 'sl 'sort-lines)


(after! ivy
  :config
  (setq ivy-count-format "(%d/%d) "))

(after! avy
  :config
  (setq avy-all-windows 'all-frames))

(after! parinfer
  :config
  (setq parinfer-auto-switch-indent-mode t
      parinfer-auto-switch-indent-mode-when-closing t)

  (def-modeline-segment! +parinfer)
  (if (bound-and-true-p parinfer-mode)
      (if (eq parinfer--mode 'indent)
          ">" ")")))

;; "do what i mean" will let dired work with multiple window panes to do copying/moving between them
(setq dired-dwim-target t)

(after! org-babel
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (ruby . t)
     (python . t)
     (ipython . t)
     (sh . t)
     (shell . t)))

  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Fix an incompatibility between the ob-async and ob-ipython packages
  (setq ob-async-no-async-languages-alist '("ipython")))

(add-hook! magit-mode (setq magit-popup-show-common-commands nil
                            magit-gh-pulls-pull-detail-limit 200))


(defvar bad-cleanup-modes '(python-mode yaml-mode)
  "List of modes where `cleanup-buffer' should not be used")

;; fuck smart quotes
(defcustom smart-to-ascii '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ;; en-dash
                            ("\x2013" . "-")
                            ;; em-dash
                            ("\x2014" . "-"))
  "Map of smart quotes to their replacements"
  :type '(repeat (cons (string :tag "Smart Character  ")
                       (string :tag "Ascii Replacement"))))

(setq
 ;; Emacs GC related
 ;; Allow font-lock-mode to do background parsing
 jit-lock-stealth-time 1
 ;; jit-lock-stealth-load 200
 jit-lock-chunk-size 1000
 jit-lock-defer-time 0.05
 ;; i only use git
 vc-handled-backends '(git))

;; all shell histories
(setq history-delete-duplicates t)

(after! counsel
  :config
  ;; (if (executable-find "rg")
  ;;     (setq counsel-grep-base-command
  ;;           "rg -i -M 120 --no-heading --line-number --color never %s"))
  (setq counsel-rg-base-command
        "rg -zS -M 120 --no-heading --line-number --color never %s ."))

(def-package! warnings
    :defer t
    :config
    (push '(undo discard-info) warning-suppress-types))

(def-package! vlf
  :config
  (setq large-file-warning-threshold (* 25 1024 1024)))
(def-package! s)
(def-package! request)
(def-package! request-deferred)

;; shell-mode echos every command and `stty -echo' doesn't change that fact
(setq comint-process-echoes t)

(after! projectile
  :config
  ;; messes with tramp, so much file check spam
  (projectile-mode -1)

  ;; https://github.com/bbatsov/projectile/issues/657
  (add-hook 'find-file-hook
            (lambda ()
              (if (locate-dominating-file default-directory ".git")
                  (projectile-mode 1))))

  (setq projectile-file-exists-local-cache-expire (* 5 60))

  (defvar-local ckp/projectile-project-name-cache nil
    "Cached value of projectile-project-name")

  (defadvice projectile-project-name (around ckp/projectile-project-name activate)
    (if (not ckp/projectile-project-name-cache)
        (setq ckp/projectile-project-name-cache ad-do-it))
    (setq ad-return-value ckp/projectile-project-name-cache)))


(after! tramp
  :config

  (defun ckp/shell-set-hook ()
    "Allows packages such as `projectile' to work when initializing/finding files."
    (when (file-remote-p (buffer-file-name))
      (let ((vec (tramp-dissect-file-name (buffer-file-name))))
        ;; all remote hosts will default to /bin/bash because OSX we use homebrew.
        (unless (string-match-p "localhost" (tramp-file-name-host vec))
          (setq-local shell-file-name "/bin/bash")))))

  (add-hook 'find-file-hook #'ckp/shell-set-hook)

  ;; Turn of auto-save for tramp files
  (defun tramp-set-auto-save ()
    (auto-save-mode -1))

  (setq tramp-default-method "sshx"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        ;; annoying when $HOME does not exist
        tramp-histfile-override nil
        ;; remote files are not updated outside of tramp/emacs so this is more performant
        remote-file-name-inhibit-cache 90
        ;; just read from the cache
        tramp-completion-reread-directory-timeout nil
        tramp-verbose 1
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo" "logbash"))))))))

  (add-to-list 'tramp-remote-path "/apollo/env/envImprovement/bin")
  (add-to-list 'tramp-remote-path (concat "/home/" (user-login-name) "/.toolbox/bin"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; if you use `expand-file-name' on a mac and try for linux.. yeah.. won't work ;)
  (add-to-list 'tramp-remote-path (concat "/home/" (user-login-name) "/bin")))
;; (add-to-list 'tramp-remote-path "/usr/local/sbin")



(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(after! eshell
  :config
  (when (< emacs-major-version 27)
    (load! "+patch-eshell-26"))

  (set-company-backend! 'eshell-mode 'company-capf)
  ;; find and chmod behave differently from Emacs than their Unix counterparts
  (setq eshell-prefer-lisp-functions nil)

  (defun eshell/lcd (&optional directory)
    "Localized version of `eshell/cd' when in a remote path."
    (if (file-remote-p default-directory)
        (with-parsed-tramp-file-name default-directory nil
          (eshell/cd (tramp-make-tramp-file-name
                      (tramp-file-name-method v)
                      (tramp-file-name-user v)
                      (tramp-file-name-domain v)
                      (tramp-file-name-host v)
                      (tramp-file-name-port v)
                      (or directory "")
                      (tramp-file-name-hop v))))
      (eshell/cd directory)))

  (defun eshell/.. (&optional level)
    "Go up LEVEL directories"
    (interactive)
    (let ((level (or level 1)))
      (eshell/cd (make-string (1+ level) ?.))
      (eshell/ls)))

  (defun eshell/unpack (file)
    (let ((command (some (lambda (x)
                           (if (string-match-p (car x) file)
                               (cadr x)))
                         '((".*\.tar.bz2" "tar xjf")
                           (".*\.tar.gz" "tar xzf")
                           (".*\.bz2" "bunzip2")
                           (".*\.rar" "unrar x")
                           (".*\.gz" "gunzip")
                           (".*\.tar" "tar xf")
                           (".*\.tbz2" "tar xjf")
                           (".*\.tgz" "tar xzf")
                           (".*\.zip" "unzip")
                           (".*\.Z" "uncompress")
                           (".*" "echo 'Could not unpack the file:'")))))
      (eshell-command-result (concat command " " file))))

  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))

  ;; history
  (setq eshell-cmpl-cycle-completions t
        eshell-history-size 1024
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-input-filter-initial-space t
        eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

  ;; aliases
  ;; paste these or put in ~/.emacs.d/.cache/eshell/alias
  ;; alias dt gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"
  ;; alias epoch date +%s
  ;; alias get curl -s -XGET $*
  ;; alias post curl -s -XPOST $*
  ;; alias put curl -s -XPUT $*
  (set-eshell-alias!
   "e"     "find-file $1"
   "ee"    "find-file-other-window $1"
   "ff"    "find-file $1"
   "emacs" "find-file $1"
   "l"     "ls -lhoG"
   "ll"    "ls -lAhoG"
   "d"     "dired $1"
   "j"     "cd ${bookmark-get-filename $1}"
   "gl"    "(call-interactively 'magit-log-current)"
   "gd"    "magit-diff-unstaged"
   "gds"   "magit-diff-staged"
   "gc"    "magit-commit"
   "dt"    "gdate \"+%Y-%m-%dT%H:%M:%S.%3N%zZ\""
   "epoch" "date +%s"
   "clear" "clear-scrollback") ; more sensible than default

  ;; used as an eshell/alias, the current directory isn't used, so it must be a function
  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  ;;; Extra execution information
  (defvar ckp/eshell-status-p t
    "If non-nil, display status before prompt.")
  (defvar ckp/eshell-status--last-command-time nil)
  (make-variable-buffer-local 'ckp/eshell-status--last-command-time)
  (defvar ckp/eshell-status-min-duration-before-display 1
    "If a command takes more time than this, display its duration.")

  (defun ckp/eshell-status-display ()
    (when ckp/eshell-status--last-command-time
      (let ((duration (time-subtract (current-time) ckp/eshell-status--last-command-time)))
        (setq ckp/eshell-status--last-command-time nil)
        (when (> (time-to-seconds duration) ckp/eshell-status-min-duration-before-display)
          (format "#[STATUS] End time %s, duration %.3fs\n"
                  (format-time-string "%F %T" (current-time))
                  (time-to-seconds duration))))))

  (defun ckp/eshell-status-record ()
    (setq ckp/eshell-status--last-command-time (current-time)))

  (add-hook 'eshell-pre-command-hook 'ckp/eshell-status-record))

(after! esh-module
  :config
  ;; Don't print the banner.
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))

(after! em-term
  :config
  (dolist (p '("watch"))
    (add-to-list 'eshell-visual-commands p))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show" "sudo" "vi" "visudo"))))

(after! em-ls
  :config
  (defun ted-eshell-ls-find-file-at-point (point)
    "RET on Eshell's `ls' output to open files."
    (interactive "d")
    (find-file (buffer-substring-no-properties
                (previous-single-property-change point 'help-echo)
                (next-single-property-change point 'help-echo))))

  (defun pat-eshell-ls-find-file-at-mouse-click (event)
    "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
    (interactive "e")
    (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
    (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
    (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
    (defvar ted-eshell-ls-keymap map))

  (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
    "Eshell's `ls' now lets you click or RET on file names to open them."
    (add-text-properties 0 (length ad-return-value)
                         (list 'help-echo "RET, mouse-2: visit this file"
                               'mouse-face 'highlight
                               'keymap ted-eshell-ls-keymap)
                         ad-return-value)
    ad-return-value))

;; take care of shell escape codes in compilation-mode buffers.
(ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun func-region (start end func)
    "run a function over the region between START and END in current buffer."
    (save-excursion
      (let ((text (delete-and-extract-region start end)))
        (insert (funcall func text)))))

;; site-local things
(when (file-exists-p "~/local.el")
  (load "~/local.el"))
