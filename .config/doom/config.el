;;; config.el -*- lexical-binding: t; -*-
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

(setq doom-theme 'doom-homage-black)
(use-package! circadian
  :config
  (setq calendar-latitude 47.603230)
  (setq calendar-longitude -122.330276)
  (setq circadian-themes '((:sunrise . doom-gruvbox-light)
                           ("12:00" . doom-opera-light)
                           ("15:00" . doom-nord-light)
                           ("19:00" . doom-outrun-electric)
                           ("20:00" . doom-palenight)
                           (:sunset  . doom-city-lights)
                           ("22:00" . doom-homage-black)))

  (add-hook! circadian-after-load-theme-hook (lambda (theme) (setq doom-theme theme)))
  (add-hook! circadian-before-load-theme-hook (disable-theme doom-theme))

  (circadian-setup))

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
(when (member "Fira Code" (font-family-list))
  (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'medium)
        doom-big-font (font-spec :family "Fira Code" :size 19 :weight 'light)
        doom-modeline-icon t))

(when (member "Libre Baskerville" (font-family-list))
  (setq doom-variable-pitch-font (font-spec :family "Libre Baskerville" :size 16)))

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

  (setq projectile-project-search-path '("~/workspace/"))

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
  (add-to-list 'tramp-remote-path (concat "/home/" (user-login-name) "/.toolbox/bin"))
  (add-to-list 'tramp-remote-path "/apollo/env/envImprovement/bin")
  (add-to-list 'tramp-remote-path "/apollo/env/ApolloCommandLine/bin")
  (add-to-list 'tramp-remote-path "/apollo/env/BrazilThirdPartyTool/bin")
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

(use-package! counsel-tramp
  :config
  (add-hook! 'counsel-tramp-pre-command-hook
    (projectile-mode 0)
    (editorconfig-mode 0))
  (add-hook! 'counsel-tramp-quit-hook
    (projectile-mode 1)
    (editorconfig-mode 1)))

(when (featurep! :app write +langtool)
  (setq langtool-language-tool-jar (expand-file-name "~/.local/LanguageTool-4.6/languagetool-commandline.jar")))

(add-hook! 'go-mode-hook indent-tabs-mode t)

(add-hook! '(text-mode-hook markdown-mode-hook) #'+word-wrap-mode)

(setq ispell-dictionary "en")
(after! spell-fu
  (setq spell-fu-idle-delay 0.5)
  (remove-hook! 'text-mode #'spell-fu-mode))  ; default is 0.25

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

;; slack api is fast moving and package needs maintenance
;; (load! "+slack.el")

(use-package! string-inflection)

(load "~/local.el" 'noerror 'nomessage)

;;; config.el ends here
