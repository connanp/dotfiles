;;; +org.el --- My Org Configuration -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; This is taken from http://doc.norang.ca/org-mode.html
;;;
;;; Code:
(require 'f)

(add-hook! org-mode-hook (set-fill-column 120))

(defvar org-default-notes-file "~/org/notes.org" "Where I put my notes")
(defvar org-default-projects-dir "~/org/projects" "Primary GTD")
(defvar org-default-inbox-file "~/org/refile.org" "Incoming entries needing to be refiled")
(defvar org-default-tasks-file "~/org/tasks.org" "Small things without projects")
(defvar org-default-incubate-file "~/org/incubate.org" "Ideas for someday")
(defvar org-default-completed-file nil "Archive")

(def-package! ox-gfm
  :when (featurep! :lang markdown +pandoc))

(def-package! ox-confluence
  :when (featurep! :lang org +export))

(def-package! org-journal
  :config
  (defun my/org-journal-date-fmt (time)
    "Custom function to insert journal date header,
and some custom text on a newly created journal file."
    (when (= (buffer-size) 0)
      (insert
       (pcase org-journal-file-type
         (`daily "#+TITLE: Journal Entry")
         (`weekly "#+TITLE: Weekly Journal")
         (`monthly "#+TITLE: Monthly Journal")
         (`yearly "#+TITLE: Yearly Journal"))))
    (concat org-journal-date-prefix (format-time-string "%e %b %Y (%A)" time)))

  (defun view-journal ()
    "Open journal to latest entry."
    (interactive)
    (org-journal-new-entry t nil))

  (customize-save-variable 'org-journal-dir "~/journal/")
  (setq org-journal-file-type 'daily
        org-journal-date-format #'my/org-journal-date-fmt
        org-journal-time-format ""))


(setq org-capture-templates (list))
(setq org-capture-default-template "c")

(add-to-list 'org-capture-templates
              `("t" "Task Entry"        entry
                (file ,org-default-inbox-file)
                "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n\nFrom: %a"
                :empty-lines 1))

(add-to-list 'org-capture-templates
              `("c" "Item to Current Clocked Task" item
                (clock)
                "%i%?" :empty-lines 1))

(add-to-list 'org-capture-templates
              `("C" "Contents to Current Clocked Task" plain
                (clock)
                "%i" :immediate-finish t :empty-lines 1))

(add-to-list 'org-capture-templates
              `("f" "Code Reference with Comments to Current Task" plain
                (clock)
                "%(ha/org-capture-code-snippet \"%F\")\n\n   %?"
                :empty-lines 1))

(add-to-list 'org-capture-templates
              `("F" "Code Reference to Current Task" plain
                (clock)
                "%(ha/org-capture-code-snippet \"%F\")"
                :empty-lines 1 :immediate-finish t))

(add-to-list 'org-capture-templates
              '("n" "Thought or Note" entry
                (file org-default-notes-file)
                "* %?\n\n  %i\n\n  See: %a" :empty-lines 1))

(add-to-list 'org-capture-templates
              '("j" "Journal Note" entry
                (file org-journal-get-entry-path)
                "* %(format-time-string org-journal-time-format)%?\n%i\n\n  From: %a" :empty-lines 1))

(add-to-list 'org-capture-templates
             `("m" "Meeting Notes" entry
               (file+headline org-default-notes-file "Meeting Notes")
               "* %^{something} :MEETING:
SCHEDULED: %<%Y-%m-%d %H:%M>

*Attendees:*

- [X] connanp@
- [ ] %?


*Agenda:*
-
-

*Notes:*


" :empty-lines 1))

(add-hook! 'org-capture-before-finalize-hook #'org-align-all-tags)

(defun org-publish-org-to-gfm (plist filename pub-dir)
  "Publish an org file to md using ox-gfm."
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))

(setq org-publish-project-alist
      '(("wiki"
         :base-directory "~/org/amazon/wiki"
         :publishing-directory "/-:dev-dsk:org-wiki/"
         :publishing-function org-publish-org-to-gfm
         :section-numbers nil
         :with-toc t)))


(defun org-global-props (&optional property buffer)
  "Get the property lists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

(defun ckp/org-agenda-files-from-dir (directory)
  "Recursively list org files from a DIRECTORY."
  (ignore-errors (f-files directory
                          (lambda (f)
                            (s-equals? (f-ext f) "org"))
                          'recursive)))

(defun ckp/org-find-file ()
  "Find files from `org-directory'"
  (interactive)
  (let ((default-directory "~/org"))
    (ignore-errors (call-interactively #'find-file))))

(setq org-agenda-files (apply 'append
                              '("~/org")
                              (mapcar #'ckp/org-agenda-files-from-dir
                               '("~/org/amazon"
                                 "~/org/regions"))))

(setq org-enforce-todo-dependencies t
      org-insert-heading-respect-content nil
      org-reverse-note-order nil
      org-deadline-warning-days 1
      org-clone-delete-id t
      org-agenda-skip-additional-timestamps-same-entry t
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-blank-before-new-entry (quote ((heading . t)
                                         (plain-list-item . nil))))

;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-redeadline 'note ;; record when the deadline date of a tasks is modified
      org-log-reschedule 'time)

(setq org-log-note-headings '((done        . "CLOSING NOTE %t")
                              (state       . "State %-12s from %-12S %t")
                              (note        . "Note taken on %t")
                              (reschedule  . "Schedule changed on %t: %S -> %s")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline  . "Deadline changed on %t: %S -> %s")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile      . "Refiled on %t")
                              (clock-out   . "")))

(setq org-return-follows-link t)
(setq org-remove-highlights-with-change nil)
(setq org-read-date-prefer-future nil)
(setq org-list-demote-modify-bullet '(("+" . "-")
                                      ("*" . "-")
                                      ("1." . "-")
                                      ("1)" . "-")
                                      ("A)" . "-")
                                      ("B)" . "-")
                                      ("a)" . "-")
                                      ("b)" . "-")
                                      ("A." . "-")
                                      ("B." . "-")
                                      ("a." . "-")
                                      ("b." . "-")))
(setq split-width-threshold 9999) ;; Minimum width for splitting windows sensibly.
(setq org-adapt-indentation nil) ;; do not indent drawers/body according to heading level
(setq org-yank-adjusted-subtrees t)

(setq org-show-context-detail
      '((agenda . lineage) ;; instead of "local"
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . ancestors)))
(setq org-agenda-ignore-properties '(effort appt stats)) ;; http://orgmode.org/worg/agenda-optimization.html
(setq org-catch-invisible-edits "smart")

(setq org-todo-repeat-to-state "NEXT")

(setq org-use-fast-todo-selection t)

;; ! records timestamp of state change
;; @ is to record a note
;; use on enter/leave state e.g. (w!/@)
(setq org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
     (sequence "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "ABRT(c@/!)"))
   org-todo-keyword-faces
   '(("[-]" :inherit (font-lock-constant-face bold))
     ("[?]" :inherit (warning bold))
     ("PROJ" :inherit (bold default))
     ("HOLD" :inherit (warning bold))
     ("ABRT" :inherit (error bold))))

;; allows changing todo states with S-left and S-right skipping all of the normal processing when entering or leaving a todo state. This cycles through the todo states but skips setting timestamps and entering notes which is very convenient when all you want to do is fix up the status of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-tag-alist '((:startgroup)
                      ("@external" . ?e)
                      ("@team" . ?t)
                      ("@oncall" . ?o)
                      ("@design" . ?d)
                      (:endgroup)
                      ("MEETING" . ?m)
                      ("ABRT" . ?a)
                      ("FLAGGED" . ??)
                      (:startgroup)
                      ("@house" . ?H)
                      ("@maintenance" . ?M)
                      (:endgroup)))

;; set without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-checkbox-hierarchical-statistics t)

(setq org-cycle-separator-lines 0)

;; Targets include this file and any file contributing to the agenda
(setq org-refile-targets '((nil :maxlevel . 0)
                           (org-agenda-files :maxlevel . 1)))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path 'file
      ;; single step
      org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

(defun ckp/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'ckp/verify-refile-target)

(defun ckp/org-auto-exclude-agenda (tag)
  "Automatic task exclusion in the agenda views with / RET."
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'ckp/org-auto-exclude-agenda)

;; quick n' fast
(setq org-agenda-span 'day)

;; disable default agenda view for 'stuck' projects
(setq org-stuck-projects '("" nil nil ""))

(setq org-agenda-dim-blocked-tasks 'invisible)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; http://orgmode.org/org.html#Breaking-down-tasks
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; https://orgmode.org/worg/org-faq.html#ratpoison-for-agenda-highlighting
(add-hook 'org-finalize-agenda-hook
    (lambda () (remove-text-properties
               (point-min) (point-max) '(mouse-face t))))

(setq org-ellipsis " ⁂")

(add-hook! 'org-mode-hook
  (push '("TODO"  . ?⚑) prettify-symbols-alist)
  (push '("DONE"  . ?✔) prettify-symbols-alist)
  (push '("WAIT"  . ?…) prettify-symbols-alist)
  (push '("ABRT"  . ?✘) prettify-symbols-alist)
  (push '("SOMEDAY"  . ??) prettify-symbols-alist))

;; better timestamps in exporting
(defun my/filter-timestamp (input back _comm)
   "Remove <> around time-stamps from Org."
   (pcase back
     ((or `markdown `html)
      (replace-regexp-in-string "&[lg]t;" "" input))))

(add-to-list 'org-export-filter-timestamp-functions
             #'my/filter-timestamp)


(defun org-subtree-region ()
  "Return a list of the start and end of a subtree."
  (save-excursion
    (list (progn (org-back-to-heading) (point))
          (progn (org-end-of-subtree)  (point)))))

(defun org-refile-directly (file-dest)
  "Move the current subtree to the end of FILE-DEST.
If SHOW-AFTER is non-nil, show the destination window,
otherwise, this destination buffer is not shown."
  (interactive "fDestination: ")

  (defun dump-it (file contents)
    (find-file-other-window file-dest)
    (goto-char (point-max))
    (insert "\n" contents))

  (save-excursion
    (let* ((region (org-subtree-region))
           (contents (buffer-substring (first region) (second region))))
      (apply 'kill-region region)
      (if org-refile-directly-show-after
          (save-current-buffer (dump-it file-dest contents))
        (save-window-excursion (dump-it file-dest contents))))))

(defvar org-refile-directly-show-after nil
  "When refiling directly (using the `org-refile-directly'
function), show the destination buffer afterwards if this is set
to `t', otherwise, just do everything in the background.")

(defun org-refile-to-incubate ()
  "Refile (move) the current Org subtree to `org-default-incubate-fire'."
  (interactive)
  (org-refile-directly org-default-incubate-file))

(defun org-refile-to-task ()
  "Refile (move) the current Org subtree to `org-default-tasks-file'."
  (interactive)
  (org-refile-directly org-default-tasks-file))

(defun org-refile-to-personal-notes ()
  "Refile (move) the current Org subtree to `org-default-notes-file'."
  (interactive)
  (org-refile-directly org-default-notes-file))

(defun org-refile-to-completed ()
  "Refile (move) the current Org subtree to `org-default-completed-file',
unless it doesn't exist, in which case, refile to today's journal entry."
  (interactive)
  (if (and org-default-completed-file (file-exists-p org-default-completed-file))
      (org-refile-directly org-default-completed-file)
    (org-refile-directly (org-journal-get-entry-path))))

(defun org-rename-header (label)
  "Rename the current section's header to LABEL, and moves the
point to the end of the line."
  (interactive (list
                (read-string "Header: "
                             (substring-no-properties (org-get-heading t t t t)))))
  (org-back-to-heading)
  (search-forward (org-get-heading t t t t) nil t 1)
  (replace-match label 'literal))

(defun org-archive-subtree-as-completed ()
  "Archives the current subtree to today's current journal entry."
  (interactive)
  (ignore-errors
    ;; According to the docs for `org-archive-subtree', the state should be
    ;; automatically marked as DONE, but I don't notice that:
    (pcase (org-get-todo-state)
      ("ABRT" "ABRT")
      (_ (org-todo "DONE"))))

  (let* ((org-archive-file (or org-default-completed-file
                               (todays-journal-entry)))
         (org-archive-location (format "%s::" org-archive-file)))
     (org-archive-subtree)))


(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))


;; from howardisms -- useful capturing commands

(require 'which-func)

(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial-txt type headers code-snippet type)))

(defun ha/code-to-clock (&optional start end)
  "Send the currently selected code to the currently clocked-in org-mode task."
  (interactive)
  (org-capture nil "F"))

(defun ha/code-comment-to-clock (&optional start end)
  "Send the currently selected code (with comments) to the
currently clocked-in org-mode task."
  (interactive)
  (org-capture nil "f"))


;; ----------------------------------------------------------------------
;;  Functions to help convert content from the operating system's
;;  clipboard into org-mode-compatible text.
;; ----------------------------------------------------------------------

(defun shell-command-with-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun ha/get-clipboard ()
  "Returns a list where the first entry is the content type,
either :html or :text, and the second is the clipboard contents."
  (if (eq system-type 'darwin)
      (ha/get-mac-clipboard)
    (ha/get-linux-clipboard)))

(defun ha/get-linux-clipboard ()
  "Return the clipbaard for a Unix-based system. See `ha/get-clipboard'."
  (destructuring-bind (exit-code contents)
      (shell-command-with-exit-code "xclip" "-o" "-t" "text/html")
    (if (= 0 exit-code)
        (list :html contents)
      (list :text (shell-command-to-string "xclip -o")))))

(defun ha/get-mac-clipboard ()
  "Returns a list where the first entry is the content type,
either :html or :text, and the second is the clipboard contents."
  (destructuring-bind (exit-code contents)
      (shell-command-with-exit-code "osascript" "-e" "the clipboard as \"HTML\"")
    (if (= 0 exit-code)
        (list :html (ha/convert-applescript-to-html contents))
      (list :text (shell-command-to-string "osascript -e 'the clipboard'")))))

(defun ha/convert-applescript-to-html (packed-contents)
  "Applescript's clipboard returns the contents in a packed array.
Convert and return this encoding into a UTF-8 string."
  (cl-flet ((hex-pack-bytes (tuple) (string-to-number (apply 'string tuple) 16)))
    (let* ((data (-> packed-contents
                     (substring 10 -2) ; strips off the =«data RTF= and =»\= bits
                     (string-to-list)))
           (byte-seq (->> data
                          (-partition 2)  ; group each two hex characters into tuple
                          (mapcar #'hex-pack-bytes))))

      (decode-coding-string
       (mapconcat #'byte-to-string byte-seq "") 'utf-8))))

(defhydra hydra-org-refiler (org-mode-map "C-c s" :hint nil)
    "
  ^Navigate^      ^Refile^       ^Move^           ^Update^        ^Go To^        ^Dired^
  ^^^^^^^^^^---------------------------------------------------------------------------------------
  _k_: ↑ previous _t_: tasks     _m X_: projects  _T_: todo task  _g t_: tasks    _g X_: projects
  _j_: ↓ next     _i_: incubate  _m P_: personal  _S_: schedule   _g i_: incubate _g P_: personal
  _c_: archive    _p_: personal                   _D_: deadline   _g x_: inbox    _g C_: completed
  _d_: delete     _r_: refile                     _R_: rename     _g n_: notes
  "
    ("<up>" org-previous-visible-heading)
    ("<down>" org-next-visible-heading)
    ("k" org-previous-visible-heading)
    ("j" org-next-visible-heading)
    ("c" org-archive-subtree-as-completed)
    ("d" org-cut-subtree)
    ("t" org-refile-to-task)
    ("i" org-refile-to-incubate)
    ("p" org-refile-to-personal-notes)
    ("r" org-refile)
    ("m X" org-refile-to-projects-dir)
    ("m P" org-refile-to-personal-dir)
    ("T" org-todo)
    ("S" org-schedule)
    ("D" org-deadline)
    ("R" org-rename-header)
    ("g t" (find-file-other-window org-default-tasks-file))
    ("g i" (find-file-other-window org-default-incubate-file))
    ("g x" (find-file-other-window org-default-inbox-file))
    ("g c" (find-file-other-window org-default-completed-file))
    ("g n" (find-file-other-window org-default-notes-file))
    ("g X" (deer org-default-projects-dir))
    ("g P" (deer org-default-personal-dir))
    ("g C" (deer org-default-completed-dir))
    ("[\t]" (org-cycle))
    ("s" (org-save-all-org-buffers) "save")
    ("q" nil "quit"))


(set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.5 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
(set-popup-rule! "^CAPTURE.*\\.org$" :size 0.4 :side 'bottom :select t :autosave t)

(provide '+org)
;;; +org.el ends here
