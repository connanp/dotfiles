;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Pearson <connanp@a45e60c681f1.ant.amazon.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-org/pre-init-PACKAGE' and/or
;;   `my-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-org-packages
  '(org
    org-babel
    org-mobile-sync)
  "The list of Lisp packages required by the my-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-org/post-init-org ()
  "Initialize my org settings"
  (use-package org
    :init
    :config
    (progn
      (setq org-directory "~/org")
      (unless (file-exists-p org-directory)
        (make-directory org-directory))

      (setq my-inbox-org-file (concat org-directory "/inbox.org"))
      (setq org-default-notes-file (concat org-directory "/notes.org"))
      (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
      (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

      ;; enabled export backends
      (custom-set-variables '(org-export-backends '(ascii html latex md)))
      (setq
            ;; follow links by pressing ENTER on them
            org-return-follows-link t
            ;; allow changing between todo stats directly by hotkey
            org-use-fast-todo-selection t
            ;; syntax highlight code in source blocks
            org-src-fontify-natively t
            ;; for the leuven theme, fontify the whole heading line
            org-fontify-whole-heading-line t
            ;; force UTF-8
            org-export-coding-system 'utf-8
            ;; don't use ido completion (I use helm)
            org-completion-use-ido nil
            ;; start up org files with indentation (same as #+STARTUP: indent)
            org-startup-indented t
            ;; don't indent source code
            org-edit-src-content-indentation 0
            ;; don't adapt indentation
            org-adapt-indentation nil
            ;; preserve the indentation inside of source blocks
            org-src-preserve-indentation t
            ;; Imenu should use 3 depth instead of 2
            org-imenu-depth 3
            ;; put state change log messages into a drawer
            org-log-into-drawer t
            ;; special begin/end of line to skip tags and stars
            org-special-ctrl-a/e t
            ;; special keys for killing a headline
            org-special-ctrl-k t
            ;; don't adjust subtrees that I copy
            org-yank-adjusted-subtrees nil
            ;; try to be smart when editing hidden things
            org-catch-invisible-edits 'smart
            ;; blank lines are removed when exiting the code edit buffer
            org-src-strip-leading-and-trailing-blank-lines t
            ;; how org-src windows are set up when hitting C-c '
            org-src-window-setup 'current-window
            ;; Overwrite the current window with the agenda
            org-agenda-window-setup 'current-window
            ;; Use 100 chars for the agenda width
            org-agenda-tags-column -100
            ;; Use full outline paths for refile targets - we file directly with IDO
            org-refile-use-outline-path t
            ;; Targets complete directly with IDO
            org-outline-path-complete-in-steps nil
            ;; Allow refile to create parent tasks with confirmation
            org-refile-allow-creating-parent-nodes 'confirm
            ;; never leave empty lines in collapsed view
            org-cycle-separator-lines 0
            ;; Use cider as the clojure backend
            org-babel-clojure-backend 'cider
            ;; don't run stuff automatically on export
            org-export-babel-evaluate nil
            ;; export tables as CSV instead of tab-delineated
            org-table-export-default-format "orgtbl-to-csv"
            ;; start up showing images
            org-startup-with-inline-images t
            ;; always enable noweb, results as code and exporting both
            org-babel-default-header-args
            (cons '(:noweb . "yes")
                  (assq-delete-all :noweb org-babel-default-header-args))
            org-babel-default-header-args
            (cons '(:exports . "both")
                  (assq-delete-all :exports org-babel-default-header-args))
            ;; I don't want to be prompted on every code block evaluation
            org-confirm-babel-evaluate nil
            ;; Mark entries as done when archiving
            org-archive-mark-done t
            ;; Where to put headlines when archiving them
            org-archive-location "%s_archive::* Archived Tasks"
            ;; Sorting order for tasks on the agenda
            org-agenda-sorting-strategy
            '((agenda habit-down
                      time-up
                      priority-down
                      user-defined-up
                      effort-up
                      category-keep)
              (todo priority-down category-up effort-up)
              (tags priority-down category-up effort-up)
              (search priority-down category-up))
            ;; Enable display of the time grid so we can see the marker for the
            ;; current time
            org-agenda-time-grid
            '((daily today remove-match)
              #("----------------" 0 16 (org-heading t))
              (0900 1100 1300 1500 1700))
            ;; keep the agenda filter until manually removed
            org-agenda-persistent-filter t
            ;; show all occurrences of repeating tasks
            org-agenda-repeating-timestamp-show-all t
            ;; always start the agenda on today
            org-agenda-start-on-weekday nil
            ;; Use sticky agenda's so they persist
            org-agenda-sticky t
            ;; show 4 agenda days
            org-agenda-span 4
            ;; Do not dim blocked tasks
            org-agenda-dim-blocked-tasks nil
            ;; Compact the block agenda view
            org-agenda-compact-blocks t
            ;; Show all agenda dates - even if they are empty
            org-agenda-show-all-dates t
            ;; Agenda org-mode files
            org-agenda-files `(,(file-truename my-inbox-org-file)
                              ,(file-truename "~/org/todo.org")
                              ,(file-truename "~/org/amazon.org")
                              ,(file-truename "~/org/notes.org")
                              ,(file-truename "~/org/journal.org")))

      ;; Org todo keywords
      (setq org-todo-keywords
            '((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "TODO(t)"
                        "SOMEDAY(s)"
                        "INPROGRESS(i)"
                        "HOLD(h)"
                        "WAITING(w@/!)"
                        "NEEDSREVIEW(n@/!)"
                        "|" "DONE(d)")
              (sequence "TODO(t)" "INPROGRESS(i)" "|" "CANCELLED(c@/!)")))
      ;; Org faces
      (setq org-todo-keyword-faces
            '(("TODO" :foreground "red" :weight bold)
              ("INPROGRESS" :foreground "deep sky blue" :weight bold)
              ("SOMEDAY" :foreground "purple" :weight bold)
              ("NEEDSREVIEW" :foreground "#edd400" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)))
      ;; add or remove tags on state change
      (setq org-todo-state-tags-triggers
            '(("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
      ;; refile targets all level 1 and 2 headers in current file and agenda files
      (setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
      ;; quick access to common tags
      (setq org-tag-alist
            '(("home" . ?h)
              ("work" . ?w)
              ("ops" . ?o)
              ("docs" . ?d)
              ("recurring" . ?r)))
      ;; capture templates
      (setq org-capture-templates
            '(("t" "todo" entry (file my-inbox-org-file)
              "* todo %?\n%u\n")
              ("o" "operations" entry (file+headline "~/org/ops.org" "inbound")
               "* todo %? :ops:\n** tt: \n\n%u\n")
              ("tt" "tt" entry (file+headline "~/org/ops.org" "inbound")
               "* todo %?%:description :ops:\n** tt: %:link\n\n%i\n%u\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline "~/org/notes.org" "notes")
              "* %? :note:\n%u\n")
              ("e" "emacs note" entry (file+headline "~/org/notes.org" "emacs links")
              "* %? :note:\n%u\n")
              ;; for use with org-protocol
              ("l" "links" entry (file+headline "~/org/notes.org" "links")
               "* %?%(truncate-string-to-width \"%:description\" 80 nil nil t) :note:link:\n** source: %:link, %u\n\n%i")
              ("j" "journal" entry (file+datetree "~/org/journal.org")
              "* %?\n%u\n")))
      ;; Custom agenda command definitions
      (setq org-agenda-custom-commands
            '(("N" "Notes" tags "NOTE"
              ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
              ((agenda "" nil)
                ;; All items with the "REFILE" tag, everything in refile.org
                ;; automatically gets that applied
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                      (org-tags-match-list-sublevels nil)))
                ;; All "INPROGRESS" todo items
                (todo "INPROGRESS"
                      ((org-agenda-overriding-header "Current work")))
                ;; All headings with the "operations" tag
                (tags "ops/!"
                      ((org-agenda-overriding-header "Operations")))
                ;; All "NEESREVIEW" todo items
                (todo "NEEDSREVIEW"
                      ((org-agenda-overriding-header "Waiting on reviews")))
                ;; All "WAITING" items without a "support" tag
                (tags "WAITING-support"
                      ((org-agenda-overriding-header "Waiting for something")))
                ;; All TODO items
                (todo "TODO"
                      ((org-agenda-overriding-header "Task list")
                      (org-agenda-sorting-strategy
                        '(time-up priority-down category-keep))))
                ;; Everything on hold
                (todo "HOLD"
                      ((org-agenda-overriding-header "On-hold")))
                ;; All headings with the "recurring" tag
                (tags "recurring/!"
                      ((org-agenda-overriding-header "Recurring"))))
              nil)))

        ;; Exclude DONE state tasks from refile targets
        (defun my-org/verify-refile-target ()
          "Exclude todo keywords with a done state from refile targets"
          (not (member (nth 2 (org-heading-components)) org-done-keywords)))
        (setq org-refile-target-verify-function 'my-org/verify-refile-target)

        (defun my/save-all-agenda-buffers ()
          "Function used to save all agenda buffers that are
          currently open, based on `org-agenda-files'."
          (interactive)
          (save-current-buffer
            (dolist (buffer (buffer-list t))
              (set-buffer buffer)
              (when (member (buffer-file-name)
                            (mapcar 'expand-file-name (org-agenda-files t)))
                (save-buffer)))))

        ;; save all the agenda files after each capture
        (add-hook 'org-capture-after-finalize-hook 'my/save-all-agenda-buffers)

        (defun my-org/skip-non-archivable-tasks ()
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
      )))

(defun my-org/init-org-mobile-sync ()
  (use-package org-mobile-sync
    :init
    (progn
      (add-hook 'org-mode (lambda () (org-mobile-sync-mode 1))))))

(defun my-org/post-init-org-babel ()
  :init
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ditaa . t)
       (dot . t)
       (python . t)
       (ruby . t)
       (js . t)
       (plantuml . t)
       (sh . t)))

    ;; Use css for nicer colors with exported document stylesheets
    (setq org-html-head-extra
          "<link rel=\"stylesheet\" href=\"file:///Users/connanp/org/style/solarized-dark.css\" type=\"text/css\" />"
          org-html-head-include-default-style nil)

    (defun my-org/inline-css-hook (exporter)
      "Insert custom inline css to automatically set the
       background of code to whatever theme I'm using's background"
      (when (eq exporter 'html)
        (let* ((my-pre-bg (face-background 'default))
               (my-pre-fg (face-foreground 'default)))
          ;;(setq org-html-head-include-default-style nil)
          (setq
           org-html-head-extra
           (concat
            org-html-head-extra
            (format
             "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
             my-pre-bg my-pre-fg))))))

    (add-hook 'org-export-before-processing-hook #'my-org/inline-css-hook))

    ;; ensure this variable is defined
    (unless (boundp 'org-babel-default-header-args:sh)
      (setq org-babel-default-header-args:sh '()))

    ;; add a default shebang header argument shell scripts
    (add-to-list 'org-babel-default-header-args:sh
                 '(:shebang . "#!/usr/bin/env bash"))

    ;; add a default shebang header argument for python
    (add-to-list 'org-babel-default-header-args:python
                 '(:shebang . "#!/usr/bin/env python"))

    (setq org-plantuml-jar-path
          (expand-file-name "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar"))
                                        ; Use fundamental mode when editing plantuml blocks with C-c '
    (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

  :config
  (progn
    ;; Disable prompting to evaluate babel blocks
    (setq org-confirm-babel-evaluate nil)))



;;; packages.el ends here
