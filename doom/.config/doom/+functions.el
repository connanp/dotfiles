;;; ~/.dotfiles/.config/doom/+functions.el -*- lexical-binding: t; -*-

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
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

(defun my/chmod-exec-buffer-file ()
  "Mark current buffer's file executable."
  (interactive)
  (executable-chmod))


(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))


(defun clear-kill-ring ()
  "empty kill ring and gc"
  (setq kill-ring nil)
  (garbage-collect))

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


(defun xah-quote-lines (@quoteL @quoteR @sep )
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

In lisp code, @quoteL @quoteR @sep are strings.

URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version 2020-06-26 2021-07-21 2021-08-15"
  (interactive
   (let (($brackets
          '(
            "\"double\""
            "'single'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            )) $bktChoice $sep $sepChoice $quoteL $quoteR)
     (setq $bktChoice (ido-completing-read "Quote to use:" $brackets ))
     (setq $sepChoice (ido-completing-read "line separator:" '(  "," ";" "none" "other")))
     (cond
      ((string-equal $bktChoice "none")
       (setq $quoteL "" $quoteR "" ))
      ((string-equal $bktChoice "other")
       (let (($x (read-string "Enter 2 chars, for begin/end quote:" )))
         (setq $quoteL (substring-no-properties $x 0 1)
               $quoteR (substring-no-properties $x 1 2))))
      (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
               $quoteR (substring-no-properties $bktChoice -1))))
     (setq $sep
           (cond
            ((string-equal $sepChoice "none") "")
            ((string-equal $sepChoice "other") (read-string "Enter separator:" ))
            (t $sepChoice)))
     (list $quoteL $quoteR $sep)))
  (let ( $p1 $p2 ($quoteL @quoteL) ($quoteR @quoteR) ($sep @sep))
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert $quoteL)
            (end-of-line )
            (insert $quoteR $sep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))
