(require 'auto-complete)

(defvar auto-complete-rst-genesource-py
  (let ((current-directory (file-name-directory load-file-name)))
    (concat current-directory "genesource.py")))

(defun auto-complete-rst-genesource-eval ()
  (with-temp-buffer
    (shell-command auto-complete-rst-genesource-py t)
    (eval-buffer))
  )

(defun auto-complete-rst-options-candidates ()
  (auto-complete-rst-get-option
   (auto-complete-rst-directive-name-at-option)))

(defvar auto-complete-rst-directive-options-map
  (make-hash-table :test 'equal)
  "A map from directive name (string) to its options (list of string)")

(defun auto-complete-rst-get-option (directive)
  "Get options (list of string) of given directive"
  (when directive
    (gethash directive auto-complete-rst-directive-options-map)))

(defun auto-complete-rst-directive-name-at-option (&optional bound)
  "Get the directive name when the cursor is at the option"
  (save-excursion
    (let* ((dir-end-pos (auto-complete-rst-goto-directive-from-option bound))
           (dir-name-end-pos (- (point) 2))
           (dir-name-beg-pos-1
            (re-search-backward " \\(\\sw\\|\\s_\\)*::\\=" bound t))
           (dir-name-beg-pos
            (if dir-name-beg-pos-1 (+ 1 dir-name-beg-pos-1))))
      (when dir-name-beg-pos
        (message "%s" dir-name-beg-pos)
        (buffer-substring dir-name-beg-pos dir-name-end-pos)))))

(defun auto-complete-rst-goto-directive-from-option (&optional bound)
  "Go to the position right after the :: of directive (#) from option (@)

.. DIRECTIVE::#
   :OPTION:
   :OPT@

"
  (setq bound (or bound (point-min)))
  (let* ((opt-pos (re-search-backward
                   "\\(\\s \\):?\\(\\sw\\|\\s_\\)*\\=" bound t))
         (opt-l0 (if opt-pos
                     (re-search-backward "^\\(\\s \\)*\\=" bound t)))
         (dir-pos nil))
    (when opt-l0
      (loop while (< bound (point))
            do (progn (forward-line -1)
                      (beginning-of-line))
            ;; check if the current line has a directive starts with ".."
            if (setq dir-pos
                     (re-search-forward
                      "\\=\\(\\s \\)*\\.\\.\\(\\s \\)+\\(\\sw\\|\\s_\\)+::"
                      opt-pos t))
            return dir-pos
            ;; if not, then it must be an option line
            if (not (re-search-forward "\\=\\(\\s \\)+:" opt-pos t))
            ;; if not, then there is no directive
            return nil))
    dir-pos))


(defun auto-complete-rst-insert-two-backquotes ()
  (insert "``")
  (backward-char)
  )

(defvar ac-source-rst-directives
  '((candidates . auto-complete-rst-directives-candidates)
    (prefix . "[[:space:]]\\.\\. \\([[:alnum:]-]*\\)")
    (symbol . "D")
    (requires . 0)
    ))

(defvar ac-source-rst-roles
  '((candidates . auto-complete-rst-roles-candidates)
    (prefix . "[[:space:]]:\\([^:]*\\)")
    (symbol . "R")
    (requires . 0)
    (action . auto-complete-rst-insert-two-backquotes)
    ))

(defvar ac-source-rst-options
  '((candidates . auto-complete-rst-options-candidates)
    (prefix . "[[:space:]]\\{4,\\}:\\([^:]*\\)")
    (symbol . "O")
    (requires . 0)
    ))

(defun auto-complete-rst-complete-space ()
  (interactive)
  (insert " ")
  (when auto-complete-mode
    (auto-complete '(ac-source-rst-directives))))

(defun auto-complete-rst-complete-colon ()
  (interactive)
  (insert ":")
  (when auto-complete-mode
    (auto-complete '(ac-source-rst-options ac-source-rst-roles))))

(defvar auto-complete-rst-other-sources nil
  "Sources to use other than the sources defined in `auto-complete-rst'

Default `ac-sources' will be used if it is `nil' (default).")

(defun auto-complete-rst-add-sources ()
  (setq ac-sources (or auto-complete-rst-other-sources ac-sources))
  (add-to-list 'ac-sources 'ac-source-rst-directives)
  (add-to-list 'ac-sources 'ac-source-rst-roles)
  (add-to-list 'ac-sources 'ac-source-rst-options)
  (local-set-key ":" 'auto-complete-rst-complete-colon)
  (local-set-key " " 'auto-complete-rst-complete-space)
  )

(defun auto-complete-rst-init ()
  (auto-complete-rst-genesource-eval)
  (add-to-list 'ac-modes 'rst-mode)
  (add-hook 'rst-mode-hook 'auto-complete-rst-add-sources)
  )

(provide 'auto-complete-rst)
