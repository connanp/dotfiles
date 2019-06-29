;;; +org.el --- My Org Configuration -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; This is taken from http://doc.norang.ca/org-mode.html
;;;
;;; Code:
(require 'f)

(add-hook 'org-mode-hook (lambda () (set-fill-column 120)))

(setq org-default-notes-file "~/org/refile.org")

(def-package! ox-gfm
  :when (featurep! :lang markdown +pandoc))

(def-package! ox-confluence
  :when (featurep! :lang org +export))

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
  (f-files directory
           (lambda (f)
             (s-equals? (f-ext f) "org"))
           'recursive))

(defun ckp/org-find-file ()
  "Find files from `org-directory'"
  (let ((default-directory "~/org"))
    (call-interactively #'find-file)))

(setq org-agenda-files (apply 'append
                              '("~/org")
                              (mapcar #'ckp/org-agenda-files-from-dir
                               '("~/org/amazon"
                                 "~/org/regions"))))

(setq org-use-fast-todo-selection t)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      `(("NEXT" . ,(doom-color 'blue))
        ("WAITING" . ,(doom-color 'orange))
        ("HOLD" . ,(doom-color 'magenta))
        ("CANCELLED" . ,(doom-color 'grey))
        ("MEETING" . ,(doom-color 'dark-cyan))
        ("PHONE" . ,(doom-color 'dark-cyan))))
;; allows changing todo states with S-left and S-right skipping all of the normal processing when entering or leaving a todo state. This cycles through the todo states but skips setting timestamps and entering notes which is very convenient when all you want to do is fix up the status of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; tag management for todo states
(setq org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                     ("WAITING" ("WAITING" . t))
                                     ("HOLD" ("WAITING") ("HOLD" . t))
                                     (done ("WAITING") ("HOLD"))
                                     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
;;
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates '(("t" "todo" entry (file "~/org/refile.org")
                               "* TODO %?
:PROPERTIES:
:Via:
:END:
:LOGBOOK:
- State \"TODO\"       from \"\"           %U
:END:
%a"
                               :clock-in t :clock-resume t :empty-lines 1)
                              ("r" "respond" entry (file "~/org/refile.org")
                               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t :empty-lines 1)
                              ("n" "note" entry (file "~/org/refile.org")
                               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                              ("j" "Journal" entry (file+olp+datetree "~/org/diary.org")
                               "* %?\n%U\n" :clock-in t :clock-resume t :empty-lines 1)
                              ("w" "org-protocol" entry (file "~/org/refile.org")
                               "* TODO Review %c\n%U\n" :immediate-finish t :empty-lines 1)
                              ("m" "Meeting" entry (file "~/org/refile.org")
                               "* MEETING %:description :MEETING:
:PROPERTIES:
:Via: %:annotation
:END:
:LOGBOOK:
- State \"MEETING\"       from \"\"           %U
:END:
%^T"
                               :clock-in t :clock-resume t :empty-lines 1)
                              ("p" "Phone call" entry (file "~/org/refile.org")
                               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                              ("h" "Habit" entry (file "~/org/refile.org")
                               "* NEXT %?
%a
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:
:LOGBOOK:
- State \"NEXT\"       from \"\"           %U
:END:\n" :empty-lines 1)))

(setq org-tag-alist '((:startgroup)
                      ("@external" . ?e)
                      ("@team" . ?t)
                      ("@oncall" . ?o)
                      (:endgroup)
                      ("WAITING" . ?w)
                      ("HOLD" . ?h)
                      ("PERSONAL" . ?P)
                      ("WORK" . ?W)
                      ("NOTE" . ?n)
                      ("CANCELLED" . ?c)
                      ("FLAGGED" . ??)))

;; set without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Targets include this file and any file contributing to the agenda
(setq org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4)))

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

(setq org-clock-out-remove-zero-time-clocks t)

;; Remove empty LOGBOOK drawers on clock out
(defun ckp/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'ckp/remove-empty-drawer-on-clock-out 'append)

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

;; Do not dim blocked tasks
;; (setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defun ckp/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun ckp/is-project-p ()
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

(defun ckp/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (ckp/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun ckp/is-task-p ()
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

(defun ckp/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun ckp/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun ckp/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar ckp/hide-scheduled-and-waiting-next-tasks t)

(defun ckp/toggle-next-task-display ()
  (interactive)
  (setq ckp/hide-scheduled-and-waiting-next-tasks (not ckp/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if ckp/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun ckp/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (ckp/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun ckp/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (ckp/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (ckp/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun ckp/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (ckp/list-sublevels-for-projects-indented)
  (if (save-excursion (ckp/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((ckp/is-project-p)
            nil)
           ((and (ckp/is-project-subtree-p) (not (ckp/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun ckp/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((ckp/is-task-p)
        nil)
       (t
        next-headline)))))

(defun ckp/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((ckp/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun ckp/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and ckp/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((ckp/is-project-p)
        next-headline)
       ((and (ckp/is-task-p) (not (ckp/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun ckp/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((ckp/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (ckp/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (ckp/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun ckp/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((ckp/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((ckp/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun ckp/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((ckp/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (ckp/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (ckp/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun ckp/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((ckp/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun ckp/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (ckp/is-subproject-p)
        nil
      next-headline)))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
            ((org-agenda-overriding-header "Notes"
                (org-tags-match-list-sublevels t)))
            ("h" "Habits" tags-todo "STYLE=\"habit\"")
            ((org-agenda-overriding-header "Habits"
                (org-agenda-sorting-strategy)
                '(todo-state-down effort-up category-keep)))
            (" " "Agenda")
            ((agenda "" nil
                (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile"))
                    (org-tags-match-list-sublevels nil))
                (tags-todo "-CANCELLED/!"
                        ((org-agenda-overriding-header "Stuck Projects"
                            (org-agenda-skip-function 'ckp/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy)
                            '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                        ((org-agenda-overriding-header "Projects"
                            (org-agenda-skip-function 'ckp/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy)
                            '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                        ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                (if ckp/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                    " (including WAITING and SCHEDULED tasks)"))
                            (org-agenda-skip-function 'ckp/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy)
                            '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                        ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                (if ckp/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                    " (including WAITING and SCHEDULED tasks)"))
                            (org-agenda-skip-function 'ckp/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy)
                            '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                        ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                (if ckp/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                    " (including WAITING and SCHEDULED tasks)"))
                            (org-agenda-skip-function 'ckp/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy)
                            '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                        ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                (if ckp/hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                    " (including WAITING and SCHEDULED tasks)"))
                            (org-agenda-skip-function 'ckp/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled ckp/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines ckp/hide-scheduled-and-waiting-next-tasks))))
                (tags "-REFILE/"
                    ((org-agenda-overriding-header "Tasks to Archive"))
                    (org-agenda-skip-function 'ckp/skip-non-archivable-tasks)
                    (org-tags-match-list-sublevels nil))))
            nil)))


(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)

(provide '+org)
;;; +org.el ends here
