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
  dotspacemacs-configuration-layer-path '()
  ;; List of configuration layers to load. If it is the symbol `all' instead
  ;; of a list then all discovered layers will be installed.
  dotspacemacs-configuration-layers '((auto-completion :variables
                                                       auto-completion-use-tab-instead-of-enter t
                                                       auto-completion-enable-sort-by-usage t)
                                      osx
                                      git
                                      version-control
                                      fasd
                                      emacs-lisp
                                      (shell :variables
                                              shell-default-shell 'eshell)
                                      (colors :variables
                                              colors-enable-nyan-cat-progress-bar t)
                                      dash
                                      (perspectives :variables
                                                    perspective-enable-persp-projectile t)
                                      python
                                      javascript
                                      ruby
                                      (haskell :variables
                                               haskell-enable-hindent-style "chris-done")
                                      html
                                      go
                                      c-c++
                                      syntax-checking
                                      evil-commentary
                                      evil-snipe
                                      repl
                                      org
                                      markdown
                                      yaml
                                      restclient
                                      prodigy)
  ;; A list of packages and/or extensions that will not be install and loaded.
  dotspacemacs-excluded-packages '()
  ;; List of additional packages that will be installed without being
  ;; wrapped in a layer. If you need some configuration for these
  ;; packages then consider to create a layer, you can also put the
  ;; configuration in `dotspacemacs/config'.
  dotspacemacs-additional-packages '(material-theme
                                     editorconfig
                                     epresent)
  ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
  ;; are declared in a layer which is not a member of
  ;; the list `dotspacemacs-configuration-layers'
  dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
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
  ;; List of items to show in the startup buffer. If nil it is disabled.
  ;; Possible values are: `recents' `bookmarks' `projects'.
  ;; (default '(recents projects))
  dotspacemacs-startup-lists '(recents projects)
  ;; List of themes, the first of the list is loaded when spacemacs starts.
  ;; Press <SPC> T n to cycle to the next theme in the list (works great
  ;; with 2 themes variants, one dark and one light)
  dotspacemacs-themes '(material
                        material-light
                        zenburn
                        leuven)
  ;; If non nil the cursor color matches the state color.
  dotspacemacs-colorize-cursor-according-to-state t
  ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
  ;; size to make separators look not too crappy.
  dotspacemacs-default-font '("Source Code Pro"
                              :size 13
                              :weight normal
                              :width normal
                              :powerline-scale 1.1)
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
  dotspacemacs-command-key ":"
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
  projectile-enable-caching nil))

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
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

  (exec-path-from-shell-copy-envs '("PATH" "GOPATH"))

  ;; sync to mobile when idle
  (defvar my-org-mobile-sync-timer nil)
  (defvar my-org-mobile-sync-secs (* 60 20))

  (defun my-org-mobile-sync-pull-and-push ()
    (org-mobile-pull)
    (org-mobile-push))

  (defun my-org-mobile-sync-start ()
    "Start automated `org-mobile-push'"
    (interactive)
    (setq my-org-mobile-sync-timer
          (run-with-idle-timer my-org-mobile-sync-secs t
                               'my-org-mobile-sync-pull-and-push)))

  (defun my-org-mobile-sync-stop ()
    "Stop automated `org-mobile-push'"
    (interactive)
    (cancel-timer my-org-mobile-sync-timer))

  (my-org-mobile-sync-start)
  ;; (setq my-site-config "~/.config/emacs.el")
  ;; (if (file-exists-p my-site-config)
  ;;     (load-file my-site-config))

  ;; FIXME: remove this once merged from the develop branch
  ;; Add global evil-leader mappings. Used to access org-agenda
  ;; functionalities – and a few others commands – from any other mode.
  (evil-leader/set-key
    ;; org-agenda
    "ao#" 'org-agenda-list-stuck-projects
    "ao/" 'org-occur-in-agenda-files
    "aoa" 'org-agenda-list
    "aoe" 'org-store-agenda-views
    "aom" 'org-tags-view
    "aoo" 'org-agenda
    "aos" 'org-search-view
    "aot" 'org-todo-list
    ;; other
    "aoO" 'org-clock-out
    "aoc" 'org-capture
    "aol" 'org-store-link)

  ;; org-mode stuff
  (setq org-directory "~/org")
  (unless (file-exists-p org-directory)
    (make-directory org-directory))

  (setq my-inbox-org-file (concat org-directory "/inbox.org"))
  (setq org-default-notes-file my-inbox-org-file)
  (setq org-agenda-files (quote ("~/org")))
  (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

  (setq org-startup-folded t)
  (setq org-catch-invisible-edits 'error)

  (setq org-capture-templates
        '(("t" "todo" entry (file my-inbox-org-file "INBOX")
           "* TODO %?\n%U\n%a\n")
          ("n" "note" entry (file my-inbox-org-file "NOTES")
           "* %? :NOTE:\n%U\n%a\n")
          ("r" "respond" entry (file my-inbox-org-file "NOTES")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
          ("m" "meeting" entry (file my-inbox-org-file)
           "* MEETING %? :MEETING:\n%U")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %?\n%U\n")))

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  (setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

  (setq org-log-done t)

  (setq org-agenda-compact-blocks t)

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)
  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ; Exclude DONE state tasks from refile targets
  (defun my-verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'my-verify-refile-target)

  ;; active Org-babel languages
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ditaa . t)
      (dot . t)
      (python . t)
      (plantuml . t)
      (sh . t)))
  (setq org-plantuml-jar-path
    (expand-file-name "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar"))
  ; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

  ;; Always enable auto indent mode
  (setq org-indent-mode t)
  (setq org-indent-indentation-per-level 2)
  ;; fontify source code
  (setq org-src-fontify-natively t)
  ;; Use current window when switch to source block
  (setq org-src-window-setup 'current-window)
  ;; Disable prompting to evaluate babel blocks
  (setq org-confirm-babel-evaluate nil)
  ;; Disable add validation link when export to HTML
  (setq org-html-validation-link nil)

  ;; tagging
  ; Tags with fast selection keys
  (setq org-tag-alist '((:startgroup)
                        ("@errand" . ?e)
                        ("@home" . ?H)
                        (:endgroup)
                        ("WAITING" . ?w)
                        ("HOLD" . ?h)
                        ("PERSONAL" . ?P)
                        ("WORK" . ?W)
                        ("NOTE" . ?n)
                        ("CANCELLED" . ?c)
                        ("FLAGGED" . ??)))

  ; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key 'expert)

  ; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; Display tags farther right
  (setq org-agenda-tags-column -102)

  (setq org-remove-highlights-with-change nil)
  (setq org-deadline-warning-days 30)
  (defvar bh/hide-scheduled-and-waiting-next-tasks t)
  ;; agenda view
  ;; disable default org-mode agenda view for stuck projects
  (setq org-stuck-projects (quote ("" nil nil "")))
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  ;; removes the clutter of extra state change log details when multiple timestamps exist in a single entry
  (setq org-agenda-skip-additional-timestamps-same-entry t)

  (setq org-clone-delete-id t)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                (" " "Agenda"
                 ((agenda "" nil)
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil))))
                 nil))))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
  Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
      (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
          t))))

  (defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
      (widen)
      (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
  nil)

  (defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
  nil)

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                  (has-next ))
              (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
              (if has-next
                  nil
              next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                  (has-next ))
              (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
              (if has-next
                  next-headline
              nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
          ((bh/is-project-p)
              nil)
          ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
          (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
      ((bh/is-project-p)
          subtree-end)
      (t
          nil)))))

  (defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
      ((and bh/hide-scheduled-and-waiting-next-tasks
              (member "WAITING" (org-get-tags-at)))
          next-headline)
      ((bh/is-project-p)
          next-headline)
      ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
          next-headline)
      (t
          nil)))))

  (defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
  When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
  When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
          (next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
      ((bh/is-project-p)
          next-headline)
      ((and (not limit-to-project)
              (bh/is-project-subtree-p))
          subtree-end)
      ((and limit-to-project
              (bh/is-project-subtree-p)
              (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
      (t
          nil)))))

  (defun bh/skip-project-tasks ()
  "Show non-project tasks.
  Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
      ((bh/is-project-p)
          subtree-end)
      ((bh/is-project-subtree-p)
          subtree-end)
      (t
          nil)))))

  (defun bh/skip-non-project-tasks ()
  "Show project tasks.
  Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
          (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
      ((bh/is-project-p)
          next-headline)
      ((and (bh/is-project-subtree-p)
              (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
      ((not (bh/is-project-subtree-p))
          subtree-end)
      (t
          nil)))))

  (defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
      ((bh/is-project-p)
          subtree-end)
      (t
          nil)))))

  (defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
      (if (bh/is-subproject-p)
          nil
      next-headline)))

  ;; archiving
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")
  (defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                      (a-month-ago (* 60 60 24 (+ daynr 1)))
                      (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                      (this-month (format-time-string "%Y-%m-" (current-time)))
                      (subtree-is-current (save-excursion
                                          (forward-line 1)
                                          (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))

  (defun bh/prepare-meeting-notes ()
    "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
    (interactive)
    (let (prefix)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (untabify (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
            (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
          (goto-char (point-min))
          (kill-ring-save (point-min) (point-max))))))

  (defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                      (nth 2 (org-heading-components)))))
      (when mystate
      (save-excursion
          (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
              (org-todo "TODO")))))))

  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#263238"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
