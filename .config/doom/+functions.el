;;; ~/.dotfiles/.config/doom/+functions.el -*- lexical-binding: t; -*-

(defun my/docker-match (name-regexp)
  ;; return the name of the last docker image which matches the input
  ;; NAME-REGEXP
  (with-temp-buffer (shell-command "docker ps" t)
                    (goto-char (point-min))
                    (let ((name-match '()))
                      (while (not (eobp))
                        (let ((current-name (string-trim (thing-at-point 'line))))
                          (if (string-match name-regexp current-name)
                              (progn
                                (end-of-line)
                                (setq name-match (format "%s" (thing-at-point 'symbol))))))
                        (forward-line 1))
                      name-match)))

(defun my/docker-path (name-regexp  &optional extended-path)
  (if extended-path
      (format "/docker:%s:/%s" (my/docker-match name-regexp) extended-path)
    (format "/docker:%s:/" (my/docker-match name-regexp))))


(after! org
  (defun my/org-babel-previous-session ()
    "Find the previous src code block which contains the session argument and
  return it together with the language"
    (interactive)
    (save-excursion
      (let ((session nil)
            (language nil))
        (while (and (re-search-backward org-babel-src-block-regexp nil t) (not session))
          (goto-char (match-beginning 0))
          (let* ((block-info (org-babel-get-src-block-info)))
                (block-lang (nth 0 block-info))
                (block-params (nth 2 block-info))
                (block-session (cdr (assoc :session block-params)))
            (when (not (string= "none" block-session))
              (setq session block-session)
              (setq language block-lang))))
        (format "%s :session %s" language session)))))


(after! org-jira
  (defun my/init-jira-cookie ()
    (let* ((token nil))
          (id nil)
          (header (prin1-to-string "Content-Type: application/json"))
          (name (prin1-to-string (shell-command-to-string "printf %s \"$(pass show jira/login | sed -n 2p | awk '{print $2}')\"")))
          (passwd (prin1-to-string (shell-command-to-string "printf %s \"$(pass show jira/login | sed -n 1p)\"")))

      (with-temp-buffer (shell-command (concat (format "curl -s -H %s " header)
                                              (format "-c - ")
                                              (format "-d \'{\"username\":%s, \"password\":%s}\' " name passwd)
                                              (format "-X POST %s/rest/auth/latest/session" jiralib-url)) t)
                        (goto-char (point-min))
                        (search-forward-regexp (regexp-quote "atlassian.xsrf.token"))
                        (setq token (car (last (split-string (string-trim (thing-at-point 'line))))))
                        (forward-line 1)
                        (setq id (car (last (split-string (string-trim (thing-at-point 'line))))))
                        (format "atlasian.xsrf.token=%s;JSESSIONID=%s" token id)))))


(defun inside-string-q ()
  "Checks if point is inside string"
  (nth 3 (syntax-ppss)))


(defun sp-open-below ()
    (interactive)
    (sp-end-of-sexp)
    (newline-and-indent)
    (evil-insert-state))

(evil-define-command sp-open-below-command ()
         :repeat t
         (sp-open-below))


