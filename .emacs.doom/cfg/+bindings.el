;;; +bindings.el Keymap -*- lexical-binding: t; -*-

(map!
 (:leader
        (:prefix "b"
          (:desc "Yank entire buffer" :n "Y" #'ckp/copy-buffer))

        (:desc "External" :prefix "a"
          (:desc "Prodigy" :n "S" #'prodigy)))

 (:after org
        :map org-mode-map
        :localleader
        :n "e" #'ckp/tangle-blocks-for-file)

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

 (:after eshell
   (:map* eshell-mode-map
     :ni "M-j" #'eshell-previous-prompt
     :ni "M-k" #'eshell-next-prompt))

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



(provide '+bindings)
;;; +bindings.el ends here
