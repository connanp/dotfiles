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
    org-babel)
    ;; org-mobile-sync)
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
  (with-eval-after-load 'org
    (setq my-inbox-org-file "~/org/inbox.org"
          org-default-notes-file "~/org/notes.org"
          org-use-speed-commands t
          org-use-fast-todo-selection t
          org-treat-S-cursor-todo-selection-as-state-change nil ;; S-left/right state change
          org-return-follows-link t
          org-hide-emphasis-markers t
          org-outline-path-complete-in-steps nil
          org-src-fontify-natively t ;; Pretty code blocks
          org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-todo-keywords
          '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
            (sequence "|" "CANCELED(c)")))

    (defun ha/org-return (&optional ignore)
      "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
      (interactive "P")
      (if ignore
          (org-return)
        (cond
         ;; Open links like usual
         ((eq 'link (car (org-element-context)))
          (org-return))
         ;; lists end with two blank lines, so we need to make sure we are also not
         ;; at the beginning of a line to avoid a loop where a new entry gets
         ;; created with only one blank line.
         ((and (org-in-item-p)
               (not (bolp)))
          (if (org-element-property :contents-begin (org-element-context))
              (org-insert-heading)
            (beginning-of-line)
            (setf (buffer-substring (line-beginning-position)
                                    (line-end-position)) "")
            (org-return)))
         ((org-at-heading-p)
          (if (not (string= ""
                            (org-element-property :title (org-element-context))))
              (progn
                (org-end-of-meta-data)
                (org-insert-heading))
            (beginning-of-line)
            (setf (buffer-substring (line-beginning-position)
                                    (line-end-position)) "")))
         ((org-at-table-p)
          (if (-any? (lambda (x)
                       (not (string= "" x)))
                     (nth (- (org-table-current-dline)
                             1)
                          (org-table-to-lisp)))
              (org-return)
            ;; empty row
            (beginning-of-line)
            (setf (buffer-substring (line-beginning-position)
                                    (line-end-position)) "")
            (org-return)))
         (t (org-return)))))

    (defun ha/paste-html-to-org ()
      "Assumes the contents of the system clip/paste-board to be
HTML, this calls out to `pandoc' to convert it for the org-mode
format."
      (interactive)
      (let* ((clip (if (eq system-type 'darwin)
                       "pbpaste -Prefer rts"
                     "xclip -out -selection 'clipboard' -t text/html"))
             (format (if (eq mode-name "Org")
                         "org"
                       "markdown"))
             (pandoc (concat "pandoc -f rts -t " format))
             (cmd (concat clip " | " pandoc))
             (text (shell-command-to-string cmd)))
        (kill-new text)
        (yank)))

    (font-lock-add-keywords ; A bit silly but my headers are now
     'org-mode              ; shorter, and that is nice canceled
     `(("^\\*+ \\(TODO\\) "
        (1
         (progn
           (compose-region (match-beginning 1)
                           (match-end 1)
                           "⚑")
           nil)))
       ("^\\*+ \\(DOING\\) "
        (1
         (progn
           (compose-region (match-beginning 1)
                           (match-end 1)
                           "⚐")
           nil)))
       ("^\\*+ \\(CANCELED\\) "
        (1
         (progn
           (compose-region (match-beginning 1)
                           (match-end 1)
                           "✘")
           nil)))
       ("^\\*+ \\(DONE\\) "
        (1
         (progn
           (compose-region (match-beginning 1)
                           (match-end 1)
                           "✔")
           nil)))))
    ;; bullets in unicode
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0
                                (prog1 ()
                                  (compose-region (match-beginning 1)
                                                  (match-end 1)
                                                  "•"))))))

    (setq org-directory "~/org")
    (unless (file-exists-p org-directory)
      (make-directory org-directory))
    (setq my-inbox-org-file "~/org/inbox.org")
    (setq org-agenda-files `(,(file-truename my-inbox-org-file)
                             ,(file-truename "~/org/todo.org")
                             ,(file-truename "~/org/amazon.org")
                             ,(file-truename "~/org/notes.org")
                             ,(file-truename "~/org/journal.org")))

    ;; (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))
    ;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

    ;; enabled export backends
    (custom-set-variables '(org-export-backends '(ascii html latex md)))

    ;; (define-key org-mode-map [remap org-return] (lambda () (interactive)
    ;;                                               (if (org-in-src-block-p)
    ;;                                                   (org-return)
    ;;                                                 (org-return-indent))))

    ;; (define-key org-mode-map (kbd "RET")  #'ha/org-return)

    ;; add or remove tags on state change
    (setq org-todo-state-tags-triggers
          '(("CANCELED" ("CANCELED" . t))
            ("HOLD" ("HOLD" . t))
            (done ("DOING") ("HOLD"))
            ("TODO" ("DOING") ("CANCELED") ("HOLD"))
            ("DOING" ("CANCELED") ("HOLD"))
            ("DONE" ("DOING") ("CANCELED") ("HOLD"))))

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
             "* todo %?\n%u\n"
             :empty-lines 1)
            ("o" "operations" entry (file+headline "~/org/ops.org" "inbound")
             "* todo %? :ops:\n** tt: \n\n%u\n"
             :empty-lines 1)
            ("tt" "tt" entry (file+headline "~/org/ops.org" "inbound")
             "* todo %?%:description :ops:\n** tt: %:link\n\n%i\n%u\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline (file org-default-notes-file) "notes")
             "* %? :note:\n%u\n"
             :empty-lines 1)
            ;; for use with org-protocol
            ("l" "links" entry (file+headline "~/org/notes.org" "links")
             "* %?%(truncate-string-to-width \"%:description\" 80 nil nil t) :note:link:\n** source: %:link, %u\n\n%i"
             :empty-lines 1)
            ("j" "journal" entry (file+datetree "~/org/journal.org")
             "* %?\n%u\n"
             :empty-lines 1)))

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
              (todo "DOING"
                    ((org-agenda-overriding-header "Current work")))
              ;; All headings with the "operations" tag
              (tags "ops/!"
                    ((org-agenda-overriding-header "Operations")))
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

    (defun my-org/save-all-agenda-buffers ()
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
    (add-hook 'org-capture-after-finalize-hook 'my-org/save-all-agenda-buffers)

    (defun my-org/meeting-notes ()
      "Call this after creating an org-mode heading for where the notes for the meeting
should be. After calling this function, call 'meeting-done' to reset the environment."
      (interactive)
      (outline-mark-subtree)                              ;; Select org-mode section
      (narrow-to-region (region-beginning) (region-end))  ;; Only show that region
      (deactivate-mark)
      (delete-other-windows)                              ;; Get rid of other windows
      (text-scale-set 2)                                  ;; Text is now readable by others
      (fringe-mode 0)
      (message "When finished taking your notes, run meeting-done."))

    (defun my-org/meeting-done ()
      "Attempt to 'undo' the effects of taking meeting notes."
      (interactive)
      (widen)                                       ;; Opposite of narrow-to-region
      (text-scale-set 0)                            ;; Reset the font size increase
      (fringe-mode 1)
      (winner-undo))))

;; (defun my-org/init-org-mobile-sync ()
;;   (add-hook 'org-mode (lambda () (org-mobile-sync-mode 1))))

(defun my-org/post-init-org-babel ()
  (with-eval-after-load 'org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ditaa . t)
       (dot . t)
       (python . t)
       (ruby . t)
       (js . t)
       (go . t)
       ;; (plantuml . t)
       (sh . t)
       (shell . t)
       (sql . t)))

    (defun org-src-debug ()
      "Put a call to this function at the beginning of the org source block to debug it."
      (save-excursion
        (let ((pt (let ((case-fold-search t)) (org-babel-where-is-src-block-head))))
          (unless pt (error "Not at source block"))
          (goto-char pt)
          (org-edit-src-code)
          (let ((parse-sexp-ignore-comments t))
            (goto-char (point-min))
            (forward-sexp 2)
            (edebug-defun)))))

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

    (add-hook 'org-export-before-processing-hook #'my-org/inline-css-hook)

    ;; ensure this variable is defined
    (unless (boundp 'org-babel-default-header-args:shell)
      (setq org-babel-default-header-args:shell '()))

    ;; add a default shebang header argument shell scripts
    (add-to-list 'org-babel-default-header-args:shell
                 '(:shebang . "#!/usr/bin/env bash"))

    ;; add a default shebang header argument for python
    (add-to-list 'org-babel-default-header-args:python
                 '(:shebang . "#!/usr/bin/env python"))

    ;; (setq org-plantuml-jar-path
    ;;       (expand-file-name "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar"))
    ;;                                       ; Use fundamental mode when editing plantuml blocks with C-c '
    ;; (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

    ;; Disable prompting to evaluate babel blocks
    (setq org-confirm-babel-evaluate nil)))



;;; packages.el ends here
