(unless (featurep! :app irc)
  (error "config relies on circe and lui integrations"))

(use-package! alert
  :defer t
  :init (setq alert-default-style 'notifier))

(use-package! emoji-cheat-sheet-plus
    :defer-incrementally emojify
    :hook (slack-mode . emoji-cheat-sheet-plus-display-mode))

(use-package! slack
  :commands (slack-start)
  :defer t
  :init
  (setq slack-enable-emoji t
        slack-prefer-current-team t
        slack-request-timeout 100)
  (map! :leader
        (:prefix-map ("y" . "chat")
         :desc "Threads(All)" "T" #'slack-all-threads
         :desc "DMs" "d" #'slack-im-select
         :desc "Groups" "g" #'slack-group-select
         :desc "Channel" "j" #'slack-channel-select
         :desc "Close Workspace" "q" #'slack-ws-close
         :desc "Select Room" "r" #'slack-select-rooms
         :desc "Start Slack" "s" #'slack-start
         :desc "Unreads" "u" #'slack-all-unreads))
  :config
  (map! (:localleader
         (:map (slack-mode slack-message-buffer-mode slack-thread-message-buffer-mode)
         "#" #'slack-message-embed-channel
          "(" #'slack-message-remove-reaction
          ")" #'slack-message-add-reaction
          "@" #'slack-message-embed-mention
          "T" #'slack-all-threads
          "d" #'slack-im-select
          "e" #'slack-message-edit
          "g" #'slack-group-select
          "j" #'slack-channel-select
          "k" #'slack-select-rooms
          "mc" #'slack-message-embed-channel
          "mm" #'slack-message-embed-mention
          "p" #'slack-room-load-prev-messages
          "q" #'slack-ws-close
          "r" #'slack-select-rooms
          "t" #'slack-thread-show-or-create
          "u" #'slack-all-unreads))
        (:map (slack-mode slack-message-buffer-mode slack-thread-message-buffer-mode)
         :i "#" 'slack-message-embed-channel
         :i ":" 'slack-insert-emoji
         :i "@" 'slack-message-embed-mention)))
