(require 'xdg)
(require 'gptel-autoloads)
(require 'gptel-agent-autoloads)
(require 'gptel-commit-autoloads)

(defvar gptel-default-mode 'org-mode)
(defvar gptel-backend (gptel-make-gh-copilot "Copilot"))
(defvar gptel-model 'gpt-5-mini)

(defvar gptel-commit-prompt
  "You are an expert at writing Git commit messages.
Generate **only** the commit message, nothing else.

FORMAT RULES:
- OUTPUT PLAIN TEXT ONLY. Do not use markdown formatting.
  NO code blocks, NO `backticks`, NO **bold** or *italic*.
  Just raw ASCII text.
- When referring to Common Lisp code, but not Emacs Lisp, upcase
  symbol names LIKE-THIS.
- Subject line (≤50 chars, imperative mood, NO period)
- Optional body paragraph explaining why (wrap at 72 chars).")

(defvar sc-gptel-mode-chat-directory
  (file-name-concat (or (getenv "SISCOG_DIR") (xdg-data-home)) "gptel-chat")
  "Default directory in which to store gptel chats")

(defun sc--gptel-mode-assign-filename ()
  "If this is a dissociated chat buffer, save it to a predetermined location.

Intended to be added to `before-save-hook' in gptel chat buffers.  Use a
prefix-arg to save manually."
  (unless (or (buffer-file-name) current-prefix-arg)
    (make-directory sc-gptel-mode-chat-directory t)
    (setq buffer-file-name
          (file-name-concat
           sc-gptel-mode-chat-directory
           (concat
            (format-time-string "%Y%m%d%H%M%2S-")
            (file-name-sans-extension
             (replace-regexp-in-string "[<>:\"\\\\|?*/ ]" "-" (buffer-name)))
            (pcase major-mode
              ('org-mode ".org") ('markdown-mode ".md") (_ ".txt")))))
    (rename-buffer (file-name-nondirectory buffer-file-name) t)))

(defun sc--gptel-mode-auto-save-chat ()
  "Enable saving chat buffers to predetermined location."
  (add-hook 'before-save-hook #'sc--gptel-mode-assign-filename
            nil 'local))

(add-hook 'gptel-mode-hook #'sc--gptel-mode-auto-save-chat)
(add-hook 'gptel-mode-hook #'visual-line-mode)

(defun sc--gptel-mode-auto-enable ()
  "Ensure that this file opens with `gptel-mode' enabled."
  (save-excursion
    (let ((enable-local-variables t))  ; Ensure we can modify local variables
      (if (and (save-excursion
                 (goto-char (point-min))
                 (looking-at ".*-\\*-")))  ; If there's a -*- line
        ;; First remove any existing eval, then add the new one
        (modify-file-local-variable-prop-line
          'eval nil 'delete))
      ;; Always add our eval
      (add-file-local-variable-prop-line
        'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

(add-hook 'gptel-save-state-hook #'sc--gptel-mode-auto-enable)
