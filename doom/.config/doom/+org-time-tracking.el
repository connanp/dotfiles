;;; +org-time-tracking.el --- Org-mode time tracking configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Clock-in and out features, hooks, etc.
;;;
;;; Code:

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23
      ;; Resume clocking task on clock-in if the clock is open
      org-clock-in-resume t
      ;; Change tasks to NEXT when clocking in
      org-clock-in-switch-to-state 'ckp/clock-in-to-next
      ;; Separate drawers for clocking and logs
      org-drawers '("PROPERTIES" "LOGBOOK")
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      org-clock-into-drawer t
      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      org-clock-out-remove-zero-time-clocks t
      ;; Clock out when moving task to a done state
      org-clock-out-when-done t
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-clock-persist t
      ;; Do not prompt to resume an active clock
      org-clock-persist-query-resume nil
      ;; Enable auto clock resolution for finding open clocks
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      ;; Include current clocking task in clock reports
      org-clock-report-include-clocking-task t)

;; Remove empty LOGBOOK drawers on clock out
(defun ckp/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'ckp/remove-empty-drawer-on-clock-out 'append)

(setq ckp/keep-clock-running nil)

(defun ckp/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (ckp/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (ckp/is-project-p))
      "TODO"))))

(defun ckp/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun ckp/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq ckp/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (ckp/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (ckp/clock-in-organization-task-as-default)))))

(defun ckp/punch-out ()
  (interactive)
  (setq ckp/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun ckp/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun ckp/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when ckp/keep-clock-running
            (ckp/clock-in-default-task)))))))

;; in todo.org Tasks/Organization
(defvar ckp/organization-task-id "AD00FE0E-C9CF-4A39-8CD1-1E66BAEB01B5")

(defun ckp/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find ckp/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun ckp/clock-out-maybe ()
  (when (and ckp/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (ckp/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'ckp/clock-out-maybe 'append)

(require 'org-id)
(defun ckp/clock-in-task-by-id (id)
  "Clock in a task by ID."
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun ckp/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one.
Skip the default task and get the next one.
A prefix ARG forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))


(provide '+org-time-tracking)
;;; +org-time-tracking.el ends here
