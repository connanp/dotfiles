;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
  ;; Base distribution to use. This is a layer contained in the directory
  ;; `+distribution'. For now available distributions are `spacemacs-base'
  ;; or `spacemacs'. (default 'spacemacs)
  dotspacemacs-distribution 'spacemacs
  ;; List of additional paths where to look for configuration layers.
  ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
  dotspacemacs-configuration-layer-path '("~/.spacemacs-layers/")
  ;; List of configuration layers to load. If it is the symbol `all' instead
  ;; of a list then all discovered layers will be installed.
  dotspacemacs-configuration-layers '((auto-completion :variables
                                                       auto-completion-enable-snippets-in-popup t
                                                       auto-completion-return-key-behavior nil
                                                       auto-completion-tab-key-behavior 'cycle
                                                       auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
                                                       auto-completion-enable-sort-by-usage t
                                                       auto-completion-enable-help-tooltip 'manual
                                                       :disabled-for org erc)
                                      better-defaults
                                      (c-c++ :variables
                                             c-c++-enable-clang-support t)
                                      (colors :variables
                                              colors-enable-nyan-cat-progress-bar t)
                                      csv
                                      dash
                                      emacs-lisp
                                      evil-commentary
                                      (evil-snipe :variables
                                                  evil-snipe-enable-alternate-f-and-t-behaviors t)
                                      fasd
                                      git
                                      gtags
                                      (go :variables
                                          gofmt-command "goimports")
                                      graphviz
                                      html
                                      javascript
                                      lua
                                      markdown
                                      org
                                      osx
                                      pandoc
                                      (python :variables
                                              python-test-runner 'pytest)
                                      restclient
                                      (ruby :variables
                                            ruby-version-manager 'rbenv
                                            ruby-test-runner 'rspec)
                                      semantic
                                      (shell :variables
                                             shell-default-term-shell "/bin/zsh"
                                             shell-default-shell 'eshell)
                                      shell-scripts
                                      (spell-checking :variables spell-checking-enable-by-default nil)
                                      syntax-checking
                                      (version-control :variables
                                                       version-control-diff-tool 'diff-hl)
                                      yaml

                                      ;; personal layers
                                      my-org
                                      my-spelling)

  ;; A list of packages and/or extensions that will not be install and loaded.
  dotspacemacs-excluded-packages '()
  ;; List of additional packages that will be installed without being
  ;; wrapped in a layer. If you need some configuration for these
  ;; packages then consider to create a layer, you can also put the
  ;; configuration in `dotspacemacs/config'.
  dotspacemacs-additional-packages '(material-theme
                                     epresent
                                     vlf
                                     mediawiki
                                     ox-mediawiki)
  ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
  ;; are declared in a layer which is not a member of
  ;; the list `dotspacemacs-configuration-layers'
  dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/user-init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
  ;; do not let custom variables pollute this file
  custom-file (concat dotspacemacs-directory "custom.el")
  ;; Specify the startup banner. Default value is `official', it displays
  ;; the official spacemacs logo. An integer value is the index of text
  ;; banner, `random' chooses a random text banner in `core/banners'
  ;; directory. A string value must be a path to a .PNG file.
  ;; If the value is nil then no banner is displayed.
  ;; dotspacemacs-startup-banner 'official
  dotspacemacs-startup-banner 'official
  dotspacemacs-editing-style 'vim
  ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
  dotspacemacs-verbose-loading nil
  dotspacemacs-startup-buffer-responsive t
  dotspacemacs-scratch-mode 'text-mode
  ;; List of items to show in the startup buffer. If nil it is disabled.
  ;; Possible values are: `recents' `bookmarks' `projects'.
  ;; (default '(recents projects))
  dotspacemacs-startup-lists '(bookmarks (recents . 10) projects)
  ;; List of themes, the first of the list is loaded when spacemacs starts.
  ;; Press <SPC> T n to cycle to the next theme in the list (works great
  ;; with 2 themes variants, one dark and one light)
  dotspacemacs-themes '(material monokai spacemacs-dark spacemacs-light)
  ;; If non nil the cursor color matches the state color.
  dotspacemacs-colorize-cursor-according-to-state t
  ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
  ;; size to make separators look not too crappy.
  dotspacemacs-default-font '(("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
                              ("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1))
  ;; The leader key
  dotspacemacs-leader-key "SPC"
  ;; The leader key accessible in `emacs state' and `insert state'
  ;; (default "M-m")
  dotspacemacs-emacs-leader-key "M-m"
  ;; Major mode leader key is a shortcut key which is the equivalent of
  ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
  dotspacemacs-major-mode-leader-key ","
  ;; Major mode leader key accessible in `emacs state' and `insert state'.
  ;; (default "C-M-m)
  dotspacemacs-major-mode-emacs-leader-key "C-M-m"
  ;; The command key used for Evil commands (ex-commands) and
  ;; Emacs commands (M-x).
  ;; By default the command key is `:' so ex-commands are executed like in Vim
  ;; with `:' and Emacs commands are executed with `<leader> :'.
  dotspacemacs-emacs-command-key "SPC"
  dotspacemacs-ex-command-key ":"
  ;; If non nil `Y' is remapped to `y$'. (default t)
  dotspacemacs-remap-Y-to-y$ t
  ;; Location where to auto-save files. Possible values are `original' to
  ;; auto-save the file in-place, `cache' to auto-save the file to another
  ;; file stored in the cache directory and `nil' to disable auto-saving.
  ;; (default 'cache)
  dotspacemacs-auto-save-file-location 'cache
  ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
  dotspacemacs-helm-resize t
  ;; if non nil, the helm header is hidden when there is only one source.
  ;; (default nil)
  dotspacemacs-helm-no-header nil
  ;; define the position to display `helm', options are `bottom', `top',
  ;; `left', or `right'. (default 'bottom)
  dotspacemacs-helm-position 'bottom
  ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
  ;; several times cycle between the kill ring content.
  dotspacemacs-enable-paste-micro-state t
  ;; Which-key delay in seconds. The which-key buffer is the popup listing
  ;; the commands bound to the current keystroke sequence. (default 0.4)
  dotspacemacs-which-key-delay 0.4
  ;; Which-key frame position. Possible values are `right', `bottom' and
  ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
  ;; right; if there is insufficient space it displays it at the bottom.
  ;; (default 'bottom)
  dotspacemacs-which-key-position 'bottom
  ;; If non nil a progress bar is displayed when spacemacs is loading. This
  ;; may increase the boot time on some systems and emacs builds, set it to
  ;; nil ;; to boost the loading time.
  dotspacemacs-loading-progress-bar nil
  ;; If non nil the frame is fullscreen when Emacs starts up.
  ;; (Emacs 24.4+ only)
  dotspacemacs-fullscreen-at-startup nil
  ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
  ;; Use to disable fullscreen animations in OSX."
  dotspacemacs-fullscreen-use-non-native nil
  ;; If non nil the frame is maximized when Emacs starts up.
  ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
  ;; (Emacs 24.4+ only)
  dotspacemacs-maximized-at-startup t
  ;; A value from the range (0..100), in increasing opacity, which describes
  ;; the transparency level of a frame when it's active or selected.
  ;; Transparency can be toggled through `toggle-transparency'.
  dotspacemacs-active-transparency 90
  ;; A value from the range (0..100), in increasing opacity, which describes
  ;; the transparency level of a frame when it's inactive or deselected.
  ;; Transparency can be toggled through `toggle-transparency'.
  dotspacemacs-inactive-transparency 90
  ;; If non nil unicode symbols are displayed in the mode line.
  dotspacemacs-mode-line-unicode-symbols t
  ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
  ;; scrolling overrides the default behavior of Emacs which recenters the
  ;; point when it reaches the top or bottom of the screen.
  dotspacemacs-smooth-scrolling t
  ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
  dotspacemacs-smartparens-strict-mode nil
  dotspacemacs-smart-closing-parenthesis nil
  ;; J K in visual move lines instead of joining them
  dotspacemacs-visual-line-move-text t
  dotspacemacs-retain-visual-state-on-shift t
  dotspacemacs-folding-method 'origami
  dotspacemacs-whitespace-cleanup 'changed
  dotspacemacs-line-numbers nil
  ;; Select a scope to highlight delimiters. Possible values are `any',
  ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
  ;; emphasis the current one). (default 'all)
  dotspacemacs-highlight-delimiters 'all
  ;; If non nil advises quit functions to keep server open when quitting.
  dotspacemacs-persistent-server nil
  ;; List of search tool executable names. Spacemacs uses the first installed
  ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
  ;; (default '("ag" "pt" "ack" "grep"))
  dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
  ;; The default package repository used if no explicit repository has been
  ;; specified with an installed package.
  ;; Not used for now.
  dotspacemacs-default-package-repository nil
  ;; User initialization goes here
  evil-escape-key-sequence "jk"
  ;; too annoying because new projects are never cached
  projectile-enable-caching nil
  ;; Emacs GC related
  ;; Allow font-lock-mode to do background parsing
  jit-lock-stealth-time 1
  ;; jit-lock-stealth-load 200
  jit-lock-chunk-size 1000
  jit-lock-defer-time 0.05
  ;; i only use git
  vc-handled-backends '(git)
  magit-popup-show-common-commands nil
  magit-gh-pulls-pull-detail-limit 200
  flycheck-check-syntax-automatically '(save mode-enabled)
  helm-ff-file-name-history-use-recentf nil
  helm-locate-command (pcase system-type
                        (`gnu/linux "locate -i -r %s")
                        (`berkeley-unix "locate -i %s")
                        (`windows-nt "es %s")
                        (`darwin "mdfind -name %s %s | egrep -v '/Library/(Caches|Mail)/'")
                        (t "locate %s"))
  ))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (use-package vlf-setup)
  (setq large-file-warning-threshold (* 25 1024 1024))

  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH"))

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'makefile-mode-hook 'whitespace-mode)

  (use-package warnings
    :defer t
    :config
    (push '(undo discard-info) warning-suppress-types))

  ;; (when (spacemacs/system-is-mac)
  ;;   (defcustom auth-sources '(macos-keychain-internet macos-keychain-generic "~/.authinfo" "~/.authinfo.gpg" "~/.netrc" "~/.netrc.gpg")))

  ;; TODO: delete me when https://github.com/syl20bnr/spacemacs/issues/5307 is in stable
  ;; (with-eval-after-load 'avy
  ;;   (when (and (not (fboundp 'avy--with-avy-keys))
  ;;              (fboundp 'avy-with))
  ;;     (defalias 'avy--with-avy-keys 'avy-with)))

  (defun byte-recompile-my-layers ()
    "Recompile all of the startup files"
    (interactive)
    (byte-recompile-directory "~/.spacemacs-layers/" 0))

  ;; buffer tidiness
  (defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defvar bad-cleanup-modes '(python-mode yaml-mode)
    "List of modes where `cleanup-buffer' should not be used")

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a
buffer. If the buffer is one of the `bad-cleanup-modes' then no
re-indenting and un-tabification is done."
    (interactive)
    (unless (member major-mode bad-cleanup-modes)
      (progn
        (indent-buffer)
        (untabify-buffer)))
    (delete-trailing-whitespace))

  ;; Perform general cleanup.
  (global-set-key (kbd "C-c n") #'cleanup-buffer)

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

  (defun my/smart-to-ascii (beg end)
    "Replace smart quotes and dashes with their ASCII equivalents"
    (interactive "r")
    (format-replace-strings smart-to-ascii
                            nil beg end))



  ;; take care of shell escape codes in compilation-mode buffers.
  (ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

  ;; nyan cat freezes eshell during scrolling (probably due to animation)
  (defun my-disable-nyan-cat-in-modes ()
    (if (eq major-mode 'eshell-mode)
        (nyan-mode -1)
      (nyan-mode)))
  (add-hook 'buffer-list-update-hook 'my-disable-nyan-cat-in-modes)

  (with-eval-after-load 'python
    (modify-syntax-entry ?_ "w" python-mode-syntax-table))

  ;; eshell

  ;; similar to setting bindkey -v in shell, but shell must use bindkey -e
  (evil-define-key 'normal term-raw-map "p" 'term-paste)
  (evil-define-key 'normal term-raw-map "j" 'term-send-down)
  (evil-define-key 'normal term-raw-map "k" 'term-send-up)
  (evil-define-key 'normal term-raw-map "/" 'term-send-reverse-search-history)
  (evil-define-key 'normal term-raw-map (kbd "C-c") 'term-send-raw)
  (evil-define-key 'insert term-raw-map (kbd "C-c") 'term-send-raw)

  ;; find and chmod behave differently from Emacs than their Unix counterparts
  (setq eshell-prefer-lisp-functions nil)
  ;; aliases
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; The 'ls' executable requires the Gnu version on the Mac
              (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                            "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -AlohG --color=always")))
              ;; if defined as an alias it won't use the working dir
              (defun eshell/gst ()
                ;; open in current dir
                (interactive)
                (magit-status default-directory)
                nil)
              (defalias 'gd 'magit-diff-unstaged)
              (defalias 'gds 'magit-diff-staged)
              (defalias 'gl 'magit-log)
              ;; paste these or put in ~/.emacs.d/.cache/eshell/alias
              ;; alias dt gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"
              ;; alias epoch date +%s
              ;; alias get curl -s -XGET $*
              ;; alias post curl -s -XPOST $*
              ;; alias put curl -s -XPUT $*
              (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))))

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
  (setq eshell-cmpl-cycle-completions nil
        eshell-save-history-on-exit t
        eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

  (defun my/create-or-switch-to-dev-buffer ()
    "Switch to the *eshell-dev* buffer, or create it"
    (interactive)
    (if (get-buffer "*eshell-dev*")
        (switch-to-buffer "*eshell-dev*")
      (let ((eshell-buffer-name "*eshell-dev*"))
        (eshell))))

  (spacemacs/set-leader-keys
    "asd" 'my/create-or-switch-to-dev-buffer)

  ;; (editorconfig-mode 1)

  (use-package tramp
    :defer 5
    :config
    ;; Turn of auto-save for tramp files
    (defun tramp-set-auto-save ()
      (auto-save-mode -1))
    (setq tramp-default-method "ssh"
          tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
          tramp-adb-program "adb"
          ;; use the settings in ~/.ssh/config instead of Tramp's
          tramp-use-ssh-controlmaster-options nil
          backup-enable-predicate
          (lambda (name)
            (and (normal-backup-enable-predicate name)
                 (not (let ((method (file-remote-p name 'method)))
                        (when (stringp method)
                          (member method '("su" "sudo"))))))))

    (use-package tramp-sh
      :config
      (add-to-list 'tramp-remote-path "/usr/local/sbin")
      (add-to-list 'tramp-remote-path "~/bin")))


  ;; Via: http://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
  ;; (setq helm-echo-input-in-header-line t)
  ;; (defun helm-hide-minibuffer-maybe ()
  ;;   (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;;       (overlay-put ov 'window (selected-window))
  ;;       (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
  ;;                               `(:background ,bg-color :foreground ,bg-color)))
  ;;       (setq-local cursor-type nil))))
  ;; (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  ;; W3M Home Page
  ;; (setq w3m-home-page "http://www.google.com")
  ;; ;; W3M default display images
  ;; (setq w3m-default-display-inline-images t)
  ;; (setq w3m-default-toggle-inline-images t)
  ;; ;; W3M use cookies
  ;; (setq w3m-command-arguments '("-cookie" "-F"))
  ;; (setq w3m-use-cookies t)
  ;; ;; Browse url function use w3m
  ;; (setq browse-url-browser-function 'w3m-browse-url)
  ;; ;; W3M view url new session in background
  ;; (setq w3m-view-this-url-new-session-in-background t)
  )

(when (file-exists-p "~/local.el")
  (load "~/local.el"))
