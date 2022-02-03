;;; ~/.dotfiles/.config/doom/+debug.el -*- lexical-binding: t; -*-

;;
;; Debug Adapter Protocol
(use-package! dap-mode
  :after lsp-mode
  :config
  ;; c++
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  ;; python
  (require 'dap-python)
  ;; configure dap
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)
  (dap-mode t)
  (dap-ui-mode t))

;; Customize more after it's b
(with-eval-after-load 'dap-mode

  ;; customize company in repl-mode
  (add-hook! 'dap-ui-repl-mode-hook (setq-local company-minimum-prefix-length 1))

  ;; Override the built-in dap hydra to add additional keybindings
  (defhydra +dap-hydra (:color purple :hint nil :foreign-keys run)
    "
^Stepping^             ^Switch^           ^Breakpoints^          ^Eval^                        ^Debug
^^^^^^^^--------------------------------------------------------------------------------------------------------------------
_n_: Next          _ss_: Session          _bt_: Toggle          _ee_: Eval                     _dd_: Debug
_i_: Step in       _st_: Thread           _bd_: Delete          _er_: Eval region              _dr_: Debug recent
_o_: Step out      _sf_: Stack frame      _ba_: Add             _es_: Eval thing at point      _dl_: Debug last
_c_: Continue      _sl_: List locals      _bc_: Set condition   _eii_: Inspect                 _dt_: Debug edit template
_r_: Restart frame _sb_: List breakpoints _bh_: Set hit count   _eir_: Inspect region          ^ ^
_Q_: Disconnect    _sS_: List sessions    _bl_: Set log message _eis_: Inspect thing at point  ^ ^
^ ^                 ^ ^                   _bD_: Delete all      ^ ^                            ^ ^
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("c" dap-continue)
    ("r" dap-restart-frame)
    ("Q" dap-disconnect)

    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)

    ("bt" dap-breakpoint-toggle)
    ("bd" dap-breakpoint-delete)
    ("bD" dap-breakpoint-delete-all)
    ("ba" dap-breakpoint-add)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)

    ("ee" dap-eval)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("eii" dap-ui-inspect)
    ("eir" dap-ui-inspect-region)
    ("eis" dap-ui-inspect-thing-at-point)

    ("dd" dap-debug)
    ("dr" dap-debug-recent)
    ("dl" dap-debug-last)
    ("dt" dap-debug-edit-template)

    ("q" nil "quit"))

  ;; Display some debug windows on session startup
  ;; https://github.com/emacs-lsp/dap-mode/wiki/HowTo:-Display-debug-windows-on-session-startup
  (defun +dap/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun +dap/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (+dap/window-visible dap-ui--locals-buffer)
          (dap-ui-locals))
        (unless (+dap/window-visible dap-ui--sessions-buffer)
          (dap-ui-sessions)))))

  ;; hide windows upon session termination
  (defun +dap/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--sessions-buffer)
           (kill-buffer dap-ui--sessions-buffer))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))
      (and (get-buffer "*Breakpoints*")
           (kill-buffer "*Breakpoints*"))))

  (add-hook 'dap-stopped-hook '+dap/show-debug-windows)
  (add-hook 'dap-terminated-hook '+dap/hide-debug-windows)

  (dap-register-debug-template "GDB::Run Stringent"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :target nil
                                     :cwd (projectile-project-root))))


;;
;; Emacs gdb
(after! gdb-mi
  (setq-default gdb-show-main nil))

(defun my/gdb-mi ()
  "Use build folder from project root as start for selection of binary to debug."
  (interactive)
  (let* ((start-directory (concat (projectile-project-root) "build/bin"))
         (file-name (read-file-name "Select binary to debug: " start-directory))
         (cwd (concat " --cd=" (projectile-project-root))))
    (gdb (concat "gdb -i=mi " file-name cwd))
    (gdb-many-windows)))

;; https://emacs.stackexchange.com/questions/7991/how-can-i-delete-all-the-gdb-related-windows-buffers-after-q-in-gdb-cli-window
  (defun my/gud-kill-all-buffers ()
    "Kill all gud buffers including Debugger, Locals, Frames, Breakpoints."
    (interactive)
    (let ((gud-buffers '(gud-mode comint-mode gdb-locals-mode gdb-frames-mode gdb-breakpoints-mode)))
      (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
            (set-buffer buffer)
            (when (member major-mode gud-buffers)
              (setq count (1+ count))
              (kill-buffer buffer)
              (delete-other-windows))) ;; fix the remaining two windows issue
          (message "Killed %i buffer(s)." count)))))

(provide '+debug)
