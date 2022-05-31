;;;; -*- coding: utf-8 -*-

(add-to-list 'load-path "z:/siscog/slime")
(require 'slime)
(slime-setup '(slime-fancy slime-banner slime-indentation slime-mdot-fu slime-trace))

;; (add-to-list 'load-path "z:/siscog/org-pms")
;; (require 'org-pms)

(setq pms-effort-file "w:/org/EFFORT.org")
(setq pms-username "luismbo")
(setq pms-visible-name "Luís B. Oliveira")

(defvar *use-slime* t)

;;;; Siscog Stuff

(setq sc-legacy-mode nil)
;(load (format "%s/custom/sc-before.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-before.el")

;; Tells SC-EMACS to use new SISCOG's ODBC names
(defvar *new-odbc-names* t)

;; Load SC-EMACS
(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))

(setq *mod-mail-method* :ascii)

(setq lisp-mode-hook (remove 'set-lisp-paragraph-regexps lisp-mode-hook))
(setq lisp-mode-hook (remove 'common-lisp-lisp-mode-hook lisp-mode-hook))

;; Customise SC-EMACS
;(load (format "%s/custom/sc-user-param.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-user-param.el")

;; Load other user specific customization.
;(load (format "%s/custom/sc-after.el" (getenv "SISCOG_EMACS_DIR_LOCAL")))
(load "~/.emacs.d/custom/sc-after.el")

;;;

(define-key global-map (kbd "C-c a") 'scg-staging-area)

;;;

(setenv "CYGWIN" "nodosfilewarning")

(setenv "PATH" (format "d:\\opt\\global\\bin;d:\\cygwin64\\bin;d:\\cygwin64\\usr\\bin;%s"
                       (getenv "PATH")))

(add-to-list 'exec-path "d:\\opt\\global\\bin")

;; for mingw pkgconfig
(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig")

(setq lbo:git-root
      (cl-find-if #'file-exists-p '("d:/opt/PortableGit-2.28" "d:/opt/PortableGit" "c:/siscog-dev-tools/Git")))

(setq ediff-diff-program (cl-find-if #'file-exists-p
                                     (list (concat lbo:git-root "/usr/bin/diff.exe")
                                           (concat lbo:git-root "/bin/diff.exe"))))
(setq diff-command ediff-diff-program)

;(setq ediff-diff3-program (concat lbo:git-root "/usr/bin/diff3.exe"))
;(setq ediff-diff3-program "d:/opt/PortableGit-2.7.0/usr/bin/diff3.exe")
(setq ediff-diff3-program "d:/cygwin64/bin/diff3.exe")

;;;; magit

;; pointing magit and vc to git.exe rather than git.cmd is
;; significantly faster for magit-status.
(setq magit-git-executable (concat lbo:git-root "/bin/git.exe"))

(setq vc-git-program magit-git-executable)

;(setenv "CREWS_VDEV_DIR" "y:/git/crews-vdev")

(defun lbo:sc (db-user data-source data-dir acl-version)
  "SC settings helper."
  (setq *old-products-configuration* (eql acl-version :v8-0))
  (if (eq acl-version :sbcl)
      (setq sc-current-lisp :sbcl)
    (sc-set-acl-version acl-version t))
  (sc-set-db-user db-user data-source)
  (sc-set-data-dir data-dir))

(add-hook 'sc-startup-hook
	  (lambda ()
	    (lbo:sc "siscog009database" "crw_local"
		    "d:/users/lbo/siscog/data/crw_local/siscog009database"
		    :sbcl)
	    (setenv "SCS_NT_SERVICES_HOST" "localhost:20000"))
	  t)

;; (lbo:sc (rot13 "fvfpbt009qngnonfr")
;; 	(rot13 "qo0511TBEN3")
;; 	(rot13 "h:/hfref/perjf-nz/perjf-fvfpbt/qo")
;; 	:v9-0-64)

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
            (diminish 'hs-minor-mode)))

(add-hook 'fi:common-lisp-mode-hook
          (lambda ()
            (hs-minor-mode t)
            ;; (hs-hide-all-comments)
            (local-set-key (kbd "<backtab>") 'hs-toggle-hiding)
            (local-set-key (kbd "<C-tab>") 'hs-toggle-all-comments)))


;; Shift+TAB toggles block visibility.
(define-key lisp-mode-shared-map (kbd "C-=") 'hs-toggle-hiding)

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
            (when (and (boundp 'hs-minor-mode) hs-minor-mode)
              (hs-minor-mode nil))))

;;;; Pretty Inner Dots

(defun pretty-inner-dots ()
  (font-lock-add-keywords
   nil
   '(("\\(\\.\\)[ .\n\r\t]"
      (0 (progn
           (decompose-region (match-beginning 1) (match-end 1))
           nil)))))
  (font-lock-add-keywords
   nil
   '(("\\(\\.\\)[^ .\n\r\t]"
      (0 (progn
           (compose-region (match-beginning 1) (match-end 1) ?·)
           nil))))))

(add-hook 'lisp-mode-hook 'pretty-inner-dots)
;; (remove-hook 'lisp-mode-hook 'pretty-inner-dots)

;; enable tabs for code, for HR
(add-hook 'prog-mode-hook
          (lambda ()
            ;; (set-face-foreground 'parenface-paren-face "gray45")
            (setq indent-tabs-mode t)
            (setq fill-column 80)))

(set-face-background 'trailing-whitespace "gray20")

;;;; Hyperspec

;; (setq common-lisp-hyperspec-root "http://intranet/TechDocs/Lisp/HyperSpec/")

;; override global hyperspec.el
;; (load "~/src/slime/hyperspec.el")

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

;;;; lol

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
   decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((step 5)
         (alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha step) (+ oldalpha step))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") (lambda () (interactive) (djcb-opacity-modify)))
(global-set-key (kbd "C-9") (lambda () (interactive) (djcb-opacity-modify t)))
(global-set-key (kbd "C-0") (lambda ()
                              (interactive)
                              (modify-frame-parameters nil '((alpha . 100)))))

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

(define-key global-map [f11] 'ediff-file-with-original)
(global-set-key [(control f11)] 'edit-mod-source-file)
(global-set-key [(control f12)] 'ediff-original-source-files)
(global-set-key (kbd "C-c m .") 'search-definition-in-source-file)

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

;; (add-to-list 'load-path "~/siscog/sc-extra")
;; (require 'sc-extra-autoloads)

;;;; CRM Keybindings

(global-set-key (kbd "C-c m c") 'set-definition-changed)
(global-set-key (kbd "C-c m a") 'set-definition-added)
(global-set-key (kbd "C-c m n") 'set-definition-new)
(global-set-key (kbd "C-c m C") 'set-definition-copy)
(global-set-key (kbd "C-c m d") 'set-definition-deleted)
(global-set-key (kbd "C-c m m") 'set-definition-moved)
(global-set-key (kbd "C-c m u") 'set-definition-update)
(global-set-key (kbd "C-c m M") 'set-definition-comment)
(global-set-key (kbd "C-c m D") 'set-definition-doc-update)
(global-set-key (kbd "C-c m h") 'set-definition-header)
(global-set-key (kbd "C-c m p") 'set-definition-to-patch)

(defun lbo-crm:move-definition-to-other-buffer ()
  (interactive)
  (mark-sexp)
  (kill-ring-save (region-beginning) (region-end))
  (other-window 1)
  (lbo:end-of-buffer-nomark)
  (newline 3)
  (yank)
  (set-definition-added)
  (other-window 1)
  (paredit-forward)
  (set-definition-moved))

;; (eval-after-load 'grep
;;   '(let ((cmd "find . -name .git -prune -o ! -name \"*~\" ! -name \"#*#\" -type f -print0 | xargs -0 -e grep --color=always -nHi -e \"^[^;]*\\(\\)\""))
;;     (grep-apply-setting 'grep-find-command
;;      (cons cmd (length cmd)))))

(eval-after-load 'grep
  '(grep-apply-setting 'grep-find-command
    "find . -name .git -prune -o ! -name \"*~\" ! -name \"#*#\" -type f -print0 | xargs -0 -e grep --color=always -nHi -e "))

;;; AucTeX

(setq LaTeX-command "d:/opt/miktex/miktex/bin/latex.exe")
(setq LaTeX-command-style '(("" "%(latex) %S%(PDFout)"))) ; era: '(("" "%(PDF)%(latex) %S%(PDFout)"))
(eval-after-load 'tex '(TeX-global-PDF-mode t))

(server-start)

(require 'grep)

(defun grep-process-setup ()
  "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (when (eq grep-highlight-matches 'auto-detect)
    (grep-compute-defaults))
  (unless (or (eq grep-highlight-matches 'auto-detect)
	      (null grep-highlight-matches)
	      ;; Don't output color escapes if they can't be
	      ;; highlighted with `font-lock-face' by `grep-filter'.
	      (null font-lock-mode))
    ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
    ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'
    (setenv "TERM" "emacs-grep")
    ;; --LBO 11-09-2015
    ;; (setenv "GREP_OPTIONS"
    ;;         (concat (getenv "GREP_OPTIONS")
    ;;     	    " --color=" (if (eq grep-highlight-matches 'always)
    ;;     			    "always" "auto")))
    ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions
    (setenv "GREP_COLOR" "01;31")
    ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions
    (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"))
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     ;; This relies on the fact that `compilation-start'
	     ;; sets buffer-modified to nil before running the command,
	     ;; so the buffer is still unmodified if there is no output.
	     (cond ((and (zerop code) (buffer-modified-p))
		    '("finished (matches found)\n" . "matched"))
		   ((not (buffer-modified-p))
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code))))
  (run-hooks 'grep-setup-hook))

(defvar lbo:fonts '("-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1"
		    "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
		    "-outline-Consolas-normal-normal-normal-mono-26-*-*-*-c-*-iso8859-1"))

(defun lbo:cycle-font ()
  (interactive)
  (setq lbo:fonts (append (rest lbo:fonts) (list (first lbo:fonts))))
  (set-frame-font (first lbo:fonts)))
