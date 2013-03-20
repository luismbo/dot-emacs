;;;; -*- coding: utf-8 -*-

(defvar *use-slime* t)

;;;; Siscog Stuff

;(load (format "%s/custom/sc-before.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-before.el")

;; Tells SC-EMACS to use new SISCOG's ODBC names
(defvar *new-odbc-names* t)

;; Load SC-EMACS
(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))

(setq lisp-mode-hook (remove 'set-lisp-paragraph-regexps lisp-mode-hook))
(setq lisp-mode-hook (remove 'common-lisp-lisp-mode-hook lisp-mode-hook))

;; Customise SC-EMACS
;(load (format "%s/custom/sc-user-param.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-user-param.el")

;; Load other user specific customization.
;(load (format "%s/custom/sc-after.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-after.el")

(setenv "CYGWIN" "nodosfilewarnings")

(setenv "PATH" (format "d:\\cygwin\\bin;d:\\cygwin\\usr\\bin;%s"
                       (getenv "PATH")))

;(setenv "CREWS_VDEV_DIR" "y:/git/crews-vdev")

(defun lbo:sc (db-user data-source data-dir acl-version)
  "SC settings helper."
  (setq *old-products-configuration* (eql acl-version :v8-0))
  (sc-set-acl-version acl-version)
  (sc-set-db-user db-user data-source)
  (sc-set-data-dir data-dir))

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

;;;; Hide Comments

(defvar hs-all-comments-hidden-p nil)

(defun hs-hide-all-comments ()
  "Adapted from `hs-hide-all'."
  (interactive)
  (setq hs-all-comments-hidden-p t)
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

(add-hook 'lisp-mode-hook
          (lambda ()
            (hs-minor-mode t)
            ;; (hs-hide-all-comments)
            ))

;; Shift+TAB toggles block visibility.
(define-key lisp-mode-shared-map (kbd "<backtab>") 'hs-toggle-hiding)

(defun hs-toggle-all-comments ()
  (interactive)
  (if hs-all-comments-hidden-p
      (progn
        (hs-show-all)
        (setq hs-all-comments-hidden-p nil))
      (hs-hide-all-comments)))

(define-key lisp-mode-shared-map (kbd "<C-tab>") 'hs-toggle-all-comments)

;; disable hs-minor-mode for ediff
(add-hook 'ediff-prepare-buffer-hook
          (lambda ()
            (when hs-minor-mode
              (hs-minor-mode nil))))

;;;; Pretty Inner Dots

(defun pretty-inner-dots ()
  (font-lock-add-keywords
   nil
   '(("\\(\\.\\)[ \n\r\t]"
      (0 (progn
           (decompose-region (match-beginning 1) (match-end 1))
           nil)))))
  (font-lock-add-keywords
   nil
   '(("\\(\\.\\)[^ \n\r\t]"
      (0 (progn
           (compose-region (match-beginning 1) (match-end 1) ?·)
           nil))))))

(add-hook 'lisp-mode-hook 'pretty-inner-dots)

;; enable tabs for Lisp code, for HR
(add-hook 'lisp-mode-hook
          (lambda ()
	    (set-face-foreground 'paren-face "gray45")
            (setq indent-tabs-mode t)
            (setq fill-column 80)))

(set-face-background 'trailing-whitespace "gray20")

;;;; Hyperspec

(setq common-lisp-hyperspec-root "http://intranet/TechDocs/Lisp/HyperSpec/")

;; override global hyperspec.el
(load "~/src/slime/hyperspec.el")

;;;; update slime-banner with MAPS::BACKGROUND.TITLE

(defun smeliscog-update-repl-header ()
  (interactive)
  (setq header-line-format
        (format "%s  Port: %s  Pid: %s"
                "CREWS - Short Term - [2009-01-01 2009-01-14] for Drivers at Ant Fom Sun"
                ;(slime-eval "(maps::background.title)")
                (slime-connection-port (slime-connection))
                (slime-pid))))

;;;; Disable the annoying/slow tramp load on, e.g., ido-mode's C-x C-f.

(setq ido-enable-tramp-completion nil)

;;;; gitk integration

;; (server-start)

;; ;; by TW, the following automagically starts ediff on the open files
(defadvice server-visit-files (after server-visit-files-gitk-ediff
                                     first
                                     (files client &optional nowait)
                                     activate)
  (let ((filenames (mapcar 'car files)))
    (when (and (= (length filenames) 2)
               (some (lambda (filename)
                       (string-match "\\.gitk-tmp\\.[0-9]+" filename))
                     filenames))
      (apply 'ediff-buffers (mapcar 'get-file-buffer filenames)))))

;;;; magit

(setq magit-git-executable "d:/git/cmd/git.cmd")

;;;; modif-mode

(add-to-list 'load-path "z:/siscog/misc/modif-request-mode/")
(require 'modif-request-mode)

;;;; lol

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
   decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;;;; printing

(setenv "PRINTER" "PDFCreator")
(setq ps-printer-name "PDFCreator")
(setq ps-printer-name-option "-d")
(setq ps-lpr-command "d:/cygwin/bin/lpr.exe")

;;;; SLIME frame title

;;; Show the current lisp's name, if available

;(setq frame-title-format
;      '(multiple-frames
;        ("%b" (:eval (if (slime-current-connection)
;                         (format " - %s" (slime-connection-name))
;                         "")))
;        ("" invocation-name "@" system-name
;         (:eval (if (slime-current-connection)
;                    (format " - %s" (slime-connection-name))
;                    "")))))

;(setq frame-title-format
;      '("" (:eval (if (slime-current-connection)
;                      (slime-eval '(cl:ignore-errors (maps::background.title)))
;                      (format "%s@%s" invocation-name system-name)))))

;;;; Shortcuts

(global-set-key [(control f11)]
                (lambda ()
                  (interactive)
                  (let ((*current-x-arg* t))
                    (edit-mod-source-file))))

(global-set-key [(control f12)]
                (lambda ()
                  (interactive)
                  (let ((*current-x-arg* t))
                    (ediff-original-source-files))))

(defun alisp ()
  (interactive)
  (let ((allegro-common-lisp-image-file
          (replace-regexp-in-string "allegro-ansi\\.dxl"
                                    "alisp.dxl"
                                    allegro-common-lisp-image-file))
        (allegro-common-lisp-image-name
          (replace-regexp-in-string "allegro-ansi\\.exe"
                                    "alisp.exe"
                                    allegro-common-lisp-image-name)))
    (allegro)))
