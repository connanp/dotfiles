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
  (ignore-errors (f-files directory
                          (lambda (f)
                            (s-equals? (f-ext f) "org"))
                          'recursive)))

(defun ckp/org-find-file ()
  "Find files from `org-directory'"
  (let ((default-directory "~/org"))
    (call-interactively #'find-file)))

(setq org-agenda-files (apply 'append
                              '("~/org")
                              (mapcar #'ckp/org-agenda-files-from-dir
                               '("~/org/amazon"
                                 "~/org/regions"))))

(setq org-enforce-todo-dependencies t
      org-insert-heading-respect-content nil
      org-reverse-note-order nil
      org-deadline-warning-days 1
      org-blank-before-new-entry (quote ((heading . t)
                                         (plain-list-item . nil))))

;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
(setq org-log-done (quote time)
      org-log-into-drawer t
      org-log-redeadline (quote note) ;; record when the deadline date of a tasks is modified
      org-log-reschedule (quote time))

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
                                      ("1)" . "-")))
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
     (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)"))
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
                      (:endgroup)
                      ("WAIT" . ?w)
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

(setq org-checkbox-hierarchical-statistics t)

(setq org-cycle-separator-lines 0)

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

(setq org-ellipsis " ▼")

(add-hook! 'org-mode-hook
  (push '("TODO"  . ?▲) prettify-symbols-alist)
  (push '("DONE"  . ?✓) prettify-symbols-alist)
  (push '("WAITING"  . ?…) prettify-symbols-alist)
  (push '("CANCELLED"  . ?×) prettify-symbols-alist)
  (push '("SOMEDAY"  . ??) prettify-symbols-alist))

;; better timestamps in exporting
(defun my/filter-timestamp (input back _comm)
   "Remove <> around time-stamps from Org."
   (pcase back
     ((or `markdown `html)
      (replace-regexp-in-string "&[lg]t;" "" input))))

(set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.5 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
(set-popup-rule! "^CAPTURE.*\\.org$" :size 0.4 :side 'bottom :select t :autosave t)

(provide '+org)
;;; +org.el ends here
