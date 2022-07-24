;;; config.el -*- lexical-binding: t; -*-
;;(setq comp-deferred-compilation-deny-list '("vterm" "evil-collection-vterm"))
(setq custom-file "~/.emacs-custom.el")
;; site-local things
(load custom-file)

(load! "+bindings")
(load! "+dired")
(when (featurep! :term eshell)
  (load! "+eshell"))

(load! "+functions")
(load! "+lisp")
(when (featurep! :tools debugger)
  (load! "+debug"))
(after! sql
  (load! "+sql"))

(setq-default
 user-full-name "Connan Pearson"
 user-mail-address "connanp@gmail.com")

(setq-default global-visual-line-mode t
              fill-column 120)

(after! company
  ;; never auto-suggest to prevent typing delays
  (setq company-idle-delay nil))

(when (<= emacs-major-version 26)
  (load! "+so-long-pre-27")
  (after! so-long
    (so-long-enable)

    (mapc (apply-partially 'add-to-list 'so-long-minor-modes)
          '(rainbow-delimiters-mode diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode))))

;; (when (>= emacs-major-version 27)
;;   ;; will be included in installation in 27.1
;;   (if (stringp (find-library-name "so-long"))
;;       (require 'so-long)
;;     (load! "+so-long"))
;;   (after! so-long
;;       (global-so-long-mode 1)

;;       ;; Additional buffer-local minor modes to disable.
;;       (mapc (apply-partially 'add-to-list 'so-long-minor-modes)
;;             '(rainbow-delimiters-mode diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode))
;;       ;; Additional variables to override.
;;       (mapc (apply-partially 'add-to-list 'so-long-variable-overrides)
;;             '((show-trailing-whitespace . nil)
;;               (truncate-lines . nil)))))

(setq doom-theme 'doom-ephemeral)

;; TODO replace with custom-theme-set-faces!
;; monochrome theme so that unbalanced parens are obvious.
(doom-themes-set-faces nil
  '(rainbow-delimiters-unmatched-face :foreground 'unspecified :inherit 'error)
  '(rainbow-delimiters-base-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-1-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-2-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-3-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-4-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-5-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-6-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-7-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-8-face :foreground (doom-color 'fg-alt))
  '(rainbow-delimiters-depth-9-face :foreground (doom-color 'fg-alt))
  ;; org headlines inherit this value
  '(outline-1 :background nil))

;; emacs 27+ bug
;; https://github.com/hlissner/doom-emacs/issues/1988
;; (custom-set-faces!
;;   '((hl-line solaire-hl-line-face org-indent
;;      outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8)
;;     :extend t))

(when IS-MAC
  (setq mac-mouse-wheel-smooth-scroll t))

(setq text-scale-mode-step 1.05
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

(setq-default line-spacing 0.1)
(setq doom-font (font-spec :family "fixed" :size 14 :weight 'medium)
      doom-modeline-icon nil)
(when (member "FiraCode Nerd Font" (font-family-list))
  (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'medium)
        doom-big-font (font-spec :family "FiraCode Nerd Font" :size 19 :weight 'light)
        doom-modeline-icon t))

(when (member "Libre Baskerville" (font-family-list))
  (setq doom-variable-pitch-font (font-spec :family "Libre Baskerville" :size 16)))

; make sure all frames have the font spec from above. when calling emacsclient, new frames won't have the customizations without this.
; specifically happens when launching with emacs --daemon
(set-face-attribute 'default nil :font doom-font)

;; performance
(setq flyspell-issue-message-flag nil)

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
  (setq ivy-count-format "(%d/%d) "))

(after! avy
  (setq avy-all-windows 'all-frames))

(when (featurep! :lang common-lisp)
  (use-package! common-lisp-snippets))

(when (featurep! :lang org)
  (after! org
    (load! "+org")
    (load! "+org-time-tracking")))

(defvar bad-cleanup-modes '(python-mode yaml-mode)
  "List of modes where `cleanup-buffer' should not be used")

;; fuck smart quotes
(defcustom smart-to-ascii '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
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
  (setq counsel-rg-base-command
        "rg -zS -M 120 --no-heading --line-number --color never %s .")

  (defun counsel-esh-directory-history ()
    "Browse Eshell history."
    (interactive)
    (require 'em-dirs)
    (ivy-read
     "Directory History: "
     (cl-loop for index from 0 for dir in (ring-elements eshell-last-dir-ring)
              collect
              (cons
               (format "%-10s %-30s"
                       (propertize (format "cd -%d" index) 'face 'font-lock-builtin-face)
                       dir)
               (format "-%d" index)))
     :action
     (lambda (arg)
       (eshell/cd (cdr arg))
       (when (featurep 'em-smart)
         (eshell-smart-goto-end))))))


(after! warnings
    (push '(undo discard-info) warning-suppress-types))

(use-package! vlf
  :init
  (setq large-file-warning-threshold (* 3 1024 1024))
  (require 'vlf-setup))
(use-package! s)
(use-package! request)
(use-package! request-deferred)

;; TODO this is buffer-local and may not even be needed anymore.
;; shell-mode echos every command and `stty -echo' doesn't change that fact
;; (setq comint-process-echoes t)

(after! projectile
  ;; messes with tramp, so much file check spam
  ;; (projectile-mode -1)

  (setq projectile-project-search-path '("~/Development" "~/repos"))

  ;; https://github.com/bbatsov/projectile/issues/657
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (if (locate-dominating-file default-directory ".git")
  ;;                 (projectile-mode 1))))

  (setq projectile-file-exists-local-cache-expire (* 5 60))
  (defvar ckp/projectile-project-name-cache nil)

  (defadvice projectile-project-name (around ckp/projectile-project-name activate)
    (if (not ckp/projectile-project-name-cache)
        (set (make-local-variable 'ckp/projectile-project-name-cache) ad-do-it))
    (setq ad-return-value ckp/projectile-project-name-cache)))


(after! tramp

  ;; (defun ckp/shell-set-hook ()
  ;;   "Allows packages such as `projectile' to work when initializing/finding files."
  ;;   (when (file-remote-p (buffer-file-name))
  ;;     (let ((vec (tramp-dissect-file-name (buffer-file-name))))
  ;;       ;; all remote hosts will default to /bin/bash because OSX we use homebrew.
  ;;       (unless (string-match-p "localhost" (tramp-file-name-host vec))
  ;;         (setq-local shell-file-name "/bin/bash")))))

  ;; (add-hook 'find-file-hook #'ckp/shell-set-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "projectile"))))

  (setq tramp-default-method "sshx"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)
        tramp-shell-prompt-pattern "\\(?:^\\|\n\\)\\[\\]\\|[^#$%>\n]*#?[#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        tramp-connection-timeout 5
        ;; annoying when $HOME does not exist
        tramp-histfile-override "/tmp/.connanp_tramp_history"
        ;; remote files are not updated outside of tramp/emacs so this is more performant
        remote-file-name-inhibit-cache (* 5 60)
        ;; oob copy only
        tramp-copy-size-limit nil
        tramp-verbose 1
        tramp-backup-directory-alist backup-directory-alist
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo" "logbash"))))))))

  (add-to-list 'tramp-remote-path (concat "/home/" (user-login-name) "/bin"))
  (add-to-list 'tramp-remote-path (concat "/home/" (user-login-name) "/.local/bin"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

(defun ckp/copy-buffer ()
  "Copy entire buffer to kill ring."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(require 'bookmark)

(after! magit
  (add-hook! 'magit-revision-mode-hook (setq line-spacing 0))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)

  ;; https://magit.vc/manual/magit/Performance.html
  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
        magit-module-sections-nested nil)
  (add-hook! magit-mode (setq magit-popup-show-common-commands nil
                              magit-gh-pulls-pull-detail-limit 200))
  ;; flyspell blocks too long when loading aspell.. otherwise i'd love to enable this.
  ;; (add-hook! 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  )

(add-to-list 'auto-mode-alist '("\\.\\(?:te\\|if\\)\\'" . m4-mode))

(set-file-template! "/.*_test\\.go$" :mode 'go-mode :project t :trigger "go_tests_file")
(set-file-template! "\\.te$" :mode 'm4-mode :project t :trigger "se_template_file")
(set-file-template! "\\.if$" :mode 'm4-mode :project t :trigger "se_if_file")

;; open all folds
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

;; flycheck isn't really useful in these modes
(add-hook! '(text-mode-hook) (flycheck-mode -1))

(setq confirm-kill-emacs nil)

(set-frame-parameter nil 'fullscreen 'maximized)

;; show ISO week numbers
(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                  (calendar-iso-from-absolute)
                  (calendar-absolute-from-gregorian (list month day year))))
        'font-lock-face 'font-lock-function-name-face))


(setq company-global-modes '(not erc-mode message-mode help-mode gud-mode))

;; save buffers when losing focus
(defun save-all ()
  (when (frame-focus-state)
    (save-some-buffers t)))

(add-function :after after-focus-change-function #'save-all)

;; (use-package! counsel-tramp
;;   :config
;;   (add-hook! 'counsel-tramp-pre-command-hook
;;     (projectile-mode 0)
;;     (editorconfig-mode 0))
;;   (add-hook! 'counsel-tramp-quit-hook
;;     (projectile-mode 1)
;;     (editorconfig-mode 1)))

(when (featurep! :app write +langtool)
  (setq langtool-language-tool-jar (expand-file-name "~/.local/LanguageTool-4.6/languagetool-commandline.jar")))

(after! go-mode
  (add-hook! 'go-mode-hook indent-tabs-mode t))

(add-hook! '(text-mode-hook markdown-mode-hook) #'+word-wrap-mode)

(setq ispell-dictionary "en")
(after! spell-fu
  (setq spell-fu-ignore-modes (list #'yaml-mode))
  (add-hook 'yaml-mode-hook #'spell-fu-mode-disable) ; inherits text-mode hook...
  (setq spell-fu-idle-delay 0.5)  ; default is 0.25
  )

(after! deft
  (setq
   deft-directory (expand-file-name "~/notes")
   deft-default-extension "md"))

(after! (:and vterm evil-collection)
  (setq vterm-shell "/usr/bin/zsh"
        vterm-max-scrollback 10000)
  (evil-collection-vterm-setup)
  (general-def 'insert vterm-mode-map "C-<escape>" #'vterm-send-escape)

  (push (list "find-file-below"
              (lambda (path)
                (if-let* ((buf (find-file-noselect path))
                          (window (display-buffer-below-selected buf nil)))
                    (select-window window)
                  (message "Failed to open file: %s" path))))
        vterm-eval-cmds)

  (add-hook! 'vterm-mode-hook
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state))

  ;; yank-pop in libvterm uses separate kill-ring and the buffer is marked read-only
  ;; using vterm-insert will correctly paste.
  (define-advice counsel-yank-pop (:around (fun &rest args))
  (if (equal major-mode 'vterm-mode)
      (let ((counsel-yank-pop-action-fun (symbol-function
                                          'counsel-yank-pop-action))
            (last-command-yank-p (eq last-command 'yank)))
        (cl-letf (((symbol-function 'counsel-yank-pop-action)
                   (lambda (s)
                     (let ((inhibit-read-only t)
                           (last-command (if (memq last-command
                                                   '(counsel-yank-pop
                                                     ivy-previous-line
                                                     ivy-next-line))
                                             'yank
                                           last-command))
                           (yank-undo-function (when last-command-yank-p
                                                 (lambda (_start _end)
                                                   (vterm-undo)))))
                       (cl-letf (((symbol-function 'insert-for-yank)
                                  'vterm-insert))
                         (funcall counsel-yank-pop-action-fun s))))))
          (apply fun args)))
    (apply fun args))))

(after! python-mode
  (set-ligatures! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")

  (setq python-indent-guess-indent-offset-verbose nil)
  )

(use-package! string-inflection)
(use-package! window-end-visible)

(after! mu4e
  (defun skip-spam-mu4e-query (query)
    (if (string-match-p "\\<maildir:" query)
        ;; If the query already has a maildir: restriction, don't
        ;; rewrite it.
        query
      ;; Otherwise restrict the search to the maildirs I marked as
      ;; relevant in this context and exclude boring maildirs such as
      ;; spam and trash.
      (let ((q (concat query
                       ;; (when th/mu4e-context-maildir-regexp
                       ;;   (format " AND maildir:\"/%s/\"" th/mu4e-context-maildir-regexp))
                       ;; Don't consider boring groups such as trash and spam.
                       (format " AND NOT maildir:\"/%s/\""
                               (regexp-opt '("/peoplegis.com/Spam" "/peoplegis.com/Trash"
                                             "/peoplegis.com/automated/mail"))))))
        ;;(message "Rewritten query: %s" q)
        q)))

  (setq mu4e-query-rewrite-function
        #'skip-spam-mu4e-query)

  (defun my/mu4e-delete-page ()
    (interactive)
    (set-mark (window-start))
    (goto-char (window-end-visible))
    (activate-mark)
    (mu4e-headers-mark-for-trash)
    (mu4e-mark-execute-all t)
    (deactivate-mark)
    (goto-char (window-start)))

  (map! (:map mu4e-headers-mode-map
    :n "]d" #'my/mu4e-delete-page))

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        mu4e-index-cleanup nil
        ;; folders are labels in fastmail just like gmail
        mu4e-index-lazy-check t
        mu4e-get-mail-command "mbsync pgis-default"
        mu4e-root-maildir "~/Maildir"
        mu4e-attachment-dir "~/Downloads/email-attachments"
        mu4e-update-interval (* 60 15))

  (set-email-account! "peoplegis.com"
  '((mu4e-sent-folder       . "/peoplegis.com/Sent")
    (mu4e-drafts-folder     . "/peoplegis.com/Drafts")
    (mu4e-trash-folder      . "/peoplegis.com/Trash")
    (mu4e-refile-folder     . (lambda (msg)
                                (cond
                                 ;; support requests
                                 ((string-prefix-p "/peoplegis.com/SRs"
                                                (mu4e-message-field msg :maildir))
                                  "/peoplegis.com/SRs/Archive")
                                 ((string-prefix-p "/peoplegis.com/automated"
                                                (mu4e-message-field msg :maildir))
                                  "/peoplegis.com/Trash")
                                 ((mu4e-message-contact-field-matches msg :to "do_not_reply@peoplegis.com"))
                                 ;; messages sent by me go to the sent folder
                                 ((mu4e-message-sent-by-me msg)
                                  mu4e-sent-folder)
                                 ;; everything else
                                 (t  "/peoplegis.com/Archive"))))
    (smtpmail-smtp-user     . "connan@peoplegis.com")
    (mu4e-maildir-shortcuts . ((:maildir "/peoplegis.com/INBOX" :key ?i)
                               (:maildir "/peoplegis.com/SRs" :key ?s)
                               (:maildir "/peoplegis.com/automated" :key ?p)
                               (:maildir "/peoplegis.com/Archive" :key ?a)
                               (:maildir "/peoplegis.com/Sent" :key ?z)))
    (mu4e-compose-signature . "---\nConnan"))
  t)

  (add-to-list 'mu4e-bookmarks
             '("maildir:/peoplegis.com/INBOX OR maildir:/peoplegis.com/SRs" "Inbox" ?z))
  (add-to-list 'mu4e-bookmarks
             '("from:budgets@costalerts.amazonaws.com OR subject:\"AWS Budgets:\"" "AWS Budget" ?a)))



(use-package! aws-mode
  :bind ;; some functions which make sense to bind to something
  ("C-c a a" . aws)
  ("C-c a l" . aws-login)
  ("C-c a i" . aws-organizations-get-account-id)
  ("C-c a n" . aws-organizations-get-account-name)
  :load-path "~/repos/aws.el"
  :custom
  ;; (aws-vault t) ;; when t use aws-vault cmd to get into aws session
  (aws-output "json") ;; optional: yaml, json, text (default: yaml)
  (aws-organizations-account "cpearson") ;; profile of organizations account. organizations commands are automatically executed against this account, when specified
  (setq aws-profile "cpearson"))

(use-package! aws-evil
  :after (aws-mode evil)
  :load-path "~/repos/aws.el")

(load "~/local.el" 'noerror 'nomessage)

;;; config.el ends here
