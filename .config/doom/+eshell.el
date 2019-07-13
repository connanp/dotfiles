;;; ~/.dotfiles/.config/doom/+eshell.el -*- lexical-binding: t; -*-

(after! eshell
  (require 'em-smart)
  (when (< emacs-major-version 27)
    (load! "+patch-eshell-26"))

  ;; support mwinit prompts
  (setq eshell-password-prompt-regexp "\\(\\(?:PIN for \\|adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\).*:\\s *\\'")

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

  (defalias 'eshell/dh 'counsel-esh-directory-history)
  (defalias 'eshell/h 'counsel-esh-history)

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
   "ff"    "+ivy/projectile-find-file"
   "fd"    "counsel-projectile-find-dir"
   "/p"    "+ivy/project-search"
   "/d"    "+ivy/project-search-from-cwd"
   "l"     "ls --color=always -lhoG"
   "ll"    "ls --color=always -lAhoG"
   "d"     "deer $1"
   "gl"    "(call-interactively 'magit-log-current)"
   "gd"    "magit-diff-unstaged"
   "gds"   "magit-diff-staged"
   "gc"    "magit-commit"
   "gr"    "cd ${vc-git-root \\'git-root}"
   "gs"    "magit-status"
   "dt"    "gdate \"+%Y-%m-%dT%H:%M:%S.%3N%zZ\""
   "epoch" "date +%s"
   "clear" "clear-scrollback") ; more sensible than default

  (defun eshell/j (location)
    (eshell/cd (bookmark-location location)))

  (defun pcomplete/j ()
    (while (pcomplete-here (bookmark-all-names) nil 'identity)))

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

  (add-hook 'eshell-pre-command-hook 'ckp/eshell-status-record)
  (add-hook 'eshell-post-command-hook 'ckp/eshell-status-display))


(after! esh-module
  ;; Don't print the banner.
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list)
  (push 'eshell-smart eshell-modules-list))


(after! em-smart
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))


(after! em-term
  (dolist (p '("watch"))
    (add-to-list 'eshell-visual-commands p))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show" "sudo" "vi" "visudo"))
        eshell-visual-options '(("aws" "--help"))))


(after! em-ls
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

(def-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)

  (when (featurep! :completion helm)
    (defun setup-eshell-helm-completion ()
      (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete))

    (add-hook 'eshell-mode-hook #'setup-eshell-helm-completion))

  (when (featurep! :completion ivy)
    (defun setup-eshell-ivy-completion ()
      (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
      ;; only if you want to use the minibuffer for completions instead of the
      ;; in-buffer interface
      (setq-local ivy-display-functions-alist
                  (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                        ivy-display-functions-alist)))

    (add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion)))
