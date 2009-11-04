;;;; Siscog Stuff

(setenv "PATH" (format "d:\\cygwin\\bin;d:\\cygwin\\usr\\bin;%s"
                       (getenv "PATH")))

;(setenv "CREWS_VDEV_DIR" "y:/git/crews-vdev")

;;;; Restore M-> and M-<

(global-set-key (kbd "M-<") 'beginning-of-buffer-nomark)
(global-set-key (kbd "M->") 'end-of-buffer-nomark)

;;;; Windows stuff

(defun w32-maximize-frame ()
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame ()
  (interactive)
  (w32-send-sys-command 61728))

(global-set-key (kbd "C-c m") 'w32-maximize-frame)
(global-set-key (kbd "C-c M") 'w32-restore-frame)

;;;; Replacing ELI with SLIME

(load "lisp-mode")

(setq auto-mode-alist
      (append '(("\.lisp$" . lisp-mode)
		("\.lsp$" . lisp-mode)
		("\.cl$" . lisp-mode)
		("\.asd$" . lisp-mode)
		("\.system$" . lisp-mode))
              auto-mode-alist))

(add-to-list 'load-path "z:/src/slime/")
(load "my-slime-config")

;;;; Siscog Mode (WIP)

(defvar siscog-mode nil)

(defun siscog-mode ()
  "Show dots as highlighted dashes."
  (font-lock-add-keywords
   nil
   `(("\\(.\\)" ;"[a-zA-Z0-9]\\(.\\)[a-zA-Z0-9]"
      (0 (progn
           (decompose-region (match-beginning 1) (match-end 1))
           nil)))))
  (font-lock-add-keywords
   nil
   `(("\\(.\\)"
      (0 (progn
           (compose-region (match-beginning 1) (match-end 1) ?-)
           nil))))))

;;;; Org Mode

;; Add extra tweaks when org-mode is started
(defvar org-extra-installed-p nil)
(add-hook 'org-mode-hook 'org-extra-install)

(defun org-extra-install ()
  "Add customisations to Org mode. This function is designed to be
added the mode hook, `org-mode-hook'"
  ;; Extras that need installing for every org-mode buffer
  ;; ... none at the moment
  ;;
  ;; One-off extras
  (unless org-extra-installed-p
    ;; Link handlers for PMS
    ;; eg [[POA:12345]] will create a clickable link to POA 12345.0
    (setq org-link-abbrev-alist
          `(("TASK" . org-extra-link-abbrev-task)
            ("POA" . org-extra-link-abbrev-poa)
            ,@org-link-abbrev-alist))
    ;; Jump to lisp definition
    (org-add-link-type "def" 'org-extra-link-def)
    (setq org-extra-installed-p t)))

(defun org-extra-split-poa (poa)
  (multiple-value-bind (poa life)
      (split-string poa "\\.")
    (list poa (or life 0))))

(defun org-extra-link-abbrev-poa (poa)
  "Returns a link to a POA"
  (apply 'format
         "http://pms.siscog.com:8080/main_frame_link.asp?module=defects&category=poa&id=%s&life=%s"
         (org-extra-split-poa poa)))

(defun org-extra-link-abbrev-task (task)
  "Returns a link to a task"
  (format "http://pms.siscog.com:8080/main_frame_link.asp?module=tasks&category=defect&id=%s" task))

(defun org-extra-link-def (thing)
  "Look up THING using in Allegro and go to its definition.
THING can be a symbol, an fspec, or their string representation."
  (fi::lisp-find-definition-common thing :other-window))

;;;; Hide Comments

(defun hs-hide-all-comments ()
  "Adapted from `hs-hide-all'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comments..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-block-start-regexp "\\)"
                       "\\|\\(" hs-c-start-regexp "\\)")))
       (while (progn
                (unless hs-hide-comments-when-hiding-all
                  (forward-comment (point-max)))
                (re-search-forward re (point-max) t))
         (if (match-beginning 1)
             ;; we have found a block beginning, skip it
             (progn
               (goto-char (match-beginning 1))
               (forward-sexp 1))
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

;; hide top-level comment blocks by default
(add-hook 'lisp-mode-hook
          (lambda ()
            (hs-minor-mode t)
            (hs-hide-all-comments)))

(define-key lisp-mode-shared-map (kbd "<backtab>") 'hs-toggle-hiding)