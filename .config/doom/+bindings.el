;;; +bindings.el Keymap -*- lexical-binding: t; -*-

(map!
 :n "C-h" #'evil-window-left
 :n "C-j" #'evil-window-down
 :n "C-k" #'evil-window-up
 :n "C-l" #'evil-window-right
 :en "<f12>" #'org-agenda
 :en "<S-f12>" #'org-clock-goto
 :ein "C-<f12>" #'org-clock-in
 :vn "] n" #'narrow-or-widen-dwim
 (:desc "Quick Bookmark" :einv "<C-f6>" (lambda () (interactive) (bookmark-set "SAVED")))
 (:desc "Jump to Quick Bookmark" :einv "<f6>" (lambda () (interactive) (bookmark-jump "SAVED")))
 :nv "SPC / /" #'deadgrep

 (:map minibuffer-local-map
   "C-n" #'next-line-or-history-element
   "C-p" #'previous-line-or-history-element)

 (:map calendar-mode-map
   :n "o" #'calendar-other-month)

 (:map smartparens-mode-map
   :n "M-o" #'sp-open-line-below-sexp-command)

 (:leader
   (:prefix "f"
     (:desc "Find file in org dir" :n "o" #'ckp/org-find-file))
   (:prefix "t"
     :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode))

 (:leader
   (:prefix "z"
     :n "i" #'ckp/punch-in
     :n "o" #'ckp/punch-out
     :n "SPC" #'ckp/clock-in-last-task
     :n "h" #'ckp/hide-other
     :n "c" #'calendar
     :n "t" #'ckp/insert-inactive-timestamp
     :n "T" #'ckp/toggle-insert-inactive-timestamp))

 (:leader
   (:prefix "b"
     (:desc "Yank entire buffer" :n "Y" #'ckp/copy-buffer))
   (:desc "apps" :prefix "a"
     (:desc "Prodigy" :prefix "p"
       :n "S" #'prodigy)))

 (:after org
   (:map org-mode-map
     (:localleader
       :n "e" #'ckp/tangle-blocks-for-file
       :n "r" #'hydra-org-refiler/body
       :n "j" #'view-journal)))

 (:after term
   ;; similar to setting bindkey -v in shell, but shell must use bindkey -e
   (:map term-raw-map
     :n "p" #'term-paste
     :n "j" #'term-send-down
     :n "k" #'term-send-up
     :n "/" #'term-send-reverse-search-history
     "C-c" #'term-send-raw))

 (:after dired
   (:map dired-mode-map
     :n "Y" #'ckp/dired-rsync
     :n "=" #'ckp/ediff-files
     "M-=" #'ckp/ediff-files))

 (:after prodigy
   (:map prodigy-mode-map
     :n "h" #'prodigy-first
     :n "j" #'prodigy-next
     :n "k" #'prodigy-prev
     :n "l" #'prodigy-last
     :n "H" #'prodigy-display-process
     :n "J" #'prodigy-next-with-status
     :n "K" #'prodigy-prev-with-status
     :n "s" #'prodigy-start
     :n "d" #'prodigy-jump-dired
     :n "g" #'prodigy-jump-magit
     :n "Y" #'prodigy-copy-cmd
     :n "R" #'revert-buffer)
   (:map prodigy-view-mode-map
     :n "gf" #'find-file-at-point
     :n "q"  #'quit-window)))

(defun ckp/esh-init-keymap ()
  "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
  (map! :map eshell-mode-map
     :ni "M-j" #'eshell-previous-prompt
     :ni "M-k" #'eshell-next-prompt))
(add-hook 'eshell-first-time-mode-hook #'ckp/esh-init-keymap)

(provide '+bindings)
;;; +bindings.el ends here
