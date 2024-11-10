;;; ../../repos/dotfiles/doom/.config/doom/+sql.el -*- lexical-binding: t; -*-

(setq sql-product 'postgres)

;; (add-hook! 'sql-set-sqli-hook (hack-dir-local-variables-non-file-buffer))

(add-hook! 'sql-set-sqli-hook (with-current-buffer (sql-find-sqli-buffer) (emacs-lock-mode 'kill)))
;; sql-mode pretty much requires your psql to be uncustomised from stock settings
(add-to-list 'sql-postgres-options "--no-psqlrc")

(setq-default sql-input-ring-file-name
              (locate-user-emacs-file ".sqli_history"))

(add-to-list 'auto-mode-alist '("\\.plpgsql\\'" . sql-mode))

(sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
(sql-set-product-feature 'postgres :prompt-cont-regexp
                         "^[-[:alnum:]_]*[-(][#>] ")

(add-hook 'sql-login-hook 'my-sql-login-hook)
(defun my-sql-login-hook ()
  "Custom SQL log-in behaviours. See `sql-login-hook'."
  ;; n.b. If you are looking for a response and need to parse the
  ;; response, use `sql-redirect-value' instead of `comint-send-string'.
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n")
      (comint-send-string proc "\\i ~/login.sql\n"))))

(defun upcase-sql-keywords ()
  "Set all SQL keywords to uppercase."
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

;; https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sql/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sql/font-lock-everything-in-sql-interactive-mode)


(defun sql-explain-region-as-json (beg end &optional copy)
  "Explain the SQL between BEG and END in detailed JSON format.
This is suitable for pasting into tools such as
http://tatiyants.com/pev/.
When the prefix argument COPY is non-nil, do not display the
resulting JSON, but instead copy it to the kill ring.
If the region is not active, uses the current paragraph, as per
`sql-send-paragraph'.
Connection information is taken from the special sql-* variables
set in the current buffer, so you will usually want to start a
SQLi session first, or otherwise set `sql-database' etc.
This command currently blocks the UI, sorry."
  (interactive "rP")
  (unless (eq sql-product 'postgres)
    (user-error "This command is for PostgreSQL only"))
  (unless (use-region-p)
    (setq beg (save-excursion (backward-paragraph) (point))
          end (save-excursion (forward-paragraph) (point))))
  (let ((query (buffer-substring-no-properties beg end)))
    (with-current-buffer (if (sql-buffer-live-p sql-buffer)
                             sql-buffer
                           (current-buffer))
      (let* ((process-environment
              (append (list (concat "PGDATABASE=" sql-database)
                            (concat "PGHOST=" sql-server)
                            (concat "PGUSER=" sql-user))
                      process-environment))
             (args (list "--no-psqlrc"
                         "-qAt"
                         "-w"             ; Never prompt for password
                         "-E"
                         "-c" (concat "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) " query ";")
                         ))
             (err-file (make-temp-file "sql-explain-json")))
        (with-current-buffer (get-buffer-create "*sql-explain-json*")
          (setq buffer-read-only nil)
          (delete-region (point-min) (point-max))
          (let ((retcode (apply 'call-process sql-postgres-program nil (list (current-buffer) err-file) nil args)))
            (if (zerop retcode)
                (progn
                  (json-mode)
                  (read-only-mode 1)
                  (if copy
                      (progn
                        (kill-ring-save (buffer-substring-no-properties (point-min) (point-max)))
                        (message "EXPLAIN output copied to kill-ring."))
                    (display-buffer (current-buffer))))
              (with-current-buffer (get-buffer-create "*sql-explain-errors*")
                (let ((inhibit-read-only t))
                  (insert-file-contents err-file nil nil nil t))
                (display-buffer (current-buffer))
                (user-error "EXPLAIN failed")))))))))


(defun sql/maybe-setup-pgdump-outline nil
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (when (looking-at "-- PostgreSQL database dump")
      (set (make-local-variable 'outline-regexp)
           "-- \\(Data for \\)?Name:")
      (set (make-local-variable 'outline-level)
           (lambda nil 1))
      (outline-minor-mode 1)
      (hide-sublevels 1))))

(add-hook! 'sql-mode-hook #'sql/maybe-setup-pgdump-outline)
(add-hook! 'sql-mode-hook (setq indent-tabs-mode t))

(defun sql/redirect-to-buf (beg end &optional copy)
  "Send and redirect region"
  (interactive "rP")
  (cond ((eq major-mode 'sql-interactive-mode)
         (unless (use-region-p)
           (setq beg (save-excursion (comint-bol) (point))
                 end (save-excursion (search-forward ";")))))
        ((not (use-region-p))
           (setq beg (save-excursion (backward-paragraph)
                                     (when (looking-at-p "[[:blank:]]*$|^```") (forward-line))
                                     (point))
                 end (save-excursion (forward-paragraph) (point)))))

  (let ((buf (sql-find-sqli-buffer))
        (reg (buffer-substring-no-properties beg end))
        (outbuf "sql-result")
        ;; the query we're looking to redirect may be a previous input
        ;; probably has a newline, and our end pos won't
        (last-input-pos (if (char-equal ?s (char-after comint-last-input-end))
                            (- (marker-position comint-last-input-end) 1)
                          (marker-position comint-last-input-end))))
    (when (and (eq major-mode 'sql-interactive-mode)
               (eq last-input-pos end))
      (delete-region beg end))
    (with-current-buffer (get-buffer-create outbuf)
      (sql-redirect buf query outbuf)
      (display-buffer (current-buffer)))))


(provide '+sql)
