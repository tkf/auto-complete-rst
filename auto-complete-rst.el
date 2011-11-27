(require 'auto-complete)

(defvar auto-complete-rst-genesource-py
  (let ((current-directory (file-name-directory load-file-name)))
    (concat current-directory "genesource.py")))

(defun auto-complete-rst-genesource-eval ()
  (with-temp-buffer
    (shell-command auto-complete-rst-genesource-py t)
    (eval-buffer))
  )

(defun auto-complete-rst-insert-two-backquotes ()
  (insert "``")
  (backward-char)
  )

(defvar ac-source-rst-directives
  '((candidates . auto-complete-rst-directives-candidates)
    (prefix . "[[:space:]]\\.\\. \\(.*\\)")
    (symbol . "D")
    (requires . 0)
    ))

(defvar ac-source-rst-roles
  '((candidates . auto-complete-rst-roles-candidates)
    (prefix . "[[:space:]]:\\(.*\\)")
    (symbol . "R")
    (requires . 0)
    (action . auto-complete-rst-insert-two-backquotes)
    ))

(defvar ac-source-rst-options
  '((candidates . auto-complete-rst-options-candidates)
    (prefix . "[[:space:]]\\{4,\\}:\\(.*\\)")
    (symbol . "O")
    (requires . 0)
    (action . auto-complete-rst-insert-two-backquotes)
    ))

(defun auto-complete-rst-complete-space ()
  (interactive)
  (insert " ")
  (auto-complete '(ac-source-rst-directives))
  )

(defun auto-complete-rst-complete-colon ()
  (interactive)
  (insert ":")
  (auto-complete '(ac-source-rst-roles ac-source-rst-options))
  )

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
