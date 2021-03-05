;;;; -*- coding: utf-8; lexical-binding: t -*-

;;; Figuring out which system we're in.
;;; We might be running on MacOSX but using X11.
(defvar darwin-p (string-match "darwin" (symbol-name system-type)))
(defvar x11-p (eq window-system 'x))
(defvar mac-p (or (eq window-system 'ns) (eq window-system 'mac)))
(defvar olpc-p (string-match "olpc" (user-login-name)))
(defvar siscog-p (string-match "luismbo" (user-login-name)))
(defvar win-p (eq window-system 'w32))

(setq user-mail-address (rot13 "ybyvirven@pbzzba-yvfc.arg"))

;; (defun lbo:imenu-elisp-sections ()
;;   (setq imenu-prev-index-position-function nil)
;;   (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

;; (add-hook 'emacs-lisp-mode-hook 'lbo:imenu-elisp-sections)

;;; emacs ... -T ORG
(defvar org-only-mode-p
  (string= "ORG" (frame-parameter (selected-frame) 'title)))

;;; emacs ... -T JABBER
(defvar roster-only-mode-p
  (string= "JABBER" (frame-parameter (selected-frame) 'title)))

(defvar gnus-only-mode-p
  (string= "GNUS" (frame-parameter (selected-frame) 'title)))

;;;; Exec Path

(when mac-p
  (add-to-list 'exec-path "/usr/local/bin"))

;;;; Load Path

(add-to-list 'load-path "~/.emacs.d/lib")

;;;; package.el

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar lbo:*auto-refreshed-packages* nil
  "True if `lbo:ensure-package' has already refreshed the package
  list in the current session")

(defun lbo:ensure-package (name)
  (unless (package-installed-p name)
    (unless lbo:*auto-refreshed-packages*
      (package-refresh-contents)
      (setq lbo:*auto-refreshed-packages* t))
    (package-install name)))

;;;; use-package

(lbo:ensure-package 'use-package)
(lbo:ensure-package 'diminish)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;;; SISCOG

(when siscog-p
  (load "~/.emacs.d/siscog/site.el")
  (unless (or roster-only-mode-p org-only-mode-p gnus-only-mode-p)
    (load "~/.emacs.d/my-siscog-config.el"))
  (unless (or gnus-only-mode-p roster-only-mode-p)
    (load "~/.emacs.d/my-siscog-org-config.el")
    (setq display-time-format "-%H:%M-")
    ;; (display-time)
    ))

;;;; Editing Stuff

(setq-default indent-tabs-mode (and siscog-p t))
(set-language-environment "UTF-8")

(global-font-lock-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(when mac-p
  (setq browse-url-browser-function
        (lambda (url &optional new-window)
          (message url)
          (do-applescript (concat "open location \"" url "\"")))))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

;;;; Highlight FIXMEs et cetera

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|CAUTION\\|XXX\\)"
                                           1 font-lock-warning-face prepend)))))

;;; ethan-wspace

(use-package ethan-wspace
  :config
  (setq mode-require-final-newline nil)
  (add-hook 'ethan-wspace-mode-hook
            (lambda ()
              (when indent-tabs-mode
                (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))))
  (add-hook 'prog-mode-hook 'ethan-wspace-mode))

(defun lbo:scrub-whitespace ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;;;; Goto Last Change

(use-package goto-last-change
  :bind (("C-c \\" . goto-last-change)))

;;;; Non-marking M-> and M-<

(defun lbo:end-of-buffer-nomark () (interactive) (goto-char (point-max)))
(defun lbo:beginning-of-buffer-nomark () (interactive) (goto-char (point-min)))
(global-set-key (kbd "M-<") 'lbo:beginning-of-buffer-nomark)
(global-set-key (kbd "M->") 'lbo:end-of-buffer-nomark)

;;;; Fonts

(when mac-p
  ;(setq mac-allow-anti-aliasing nil)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(setq *default-font*
      (cond
        (mac-p "Menlo-11")
        (olpc-p "Monospace-7")
        ;; (win-p "Consolas-10")
        (win-p "Cascadia Code PL-10")
        ((> emacs-major-version 22) "Ubuntu Mono 12")
        (t "-*-*-*-*-*-*-13-*-*-*-*-*-*-*")))

(set-frame-font *default-font* t t)

(global-set-key [C-M-wheel-up] 'text-scale-increase)
(global-set-key [C-M-wheel-down] 'text-scale-decrease)

;;;; C

(defun my-c-mode ()
  (c-set-style "K&R")
  (setq c-basic-offset 4
        indent-tabs-mode nil)
  (helm-gtags-mode))

(add-hook 'c-mode-hook 'my-c-mode)
(add-hook 'c++-mode-hook 'my-c-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))

(when siscog-p
  (defun lbo:inject-mingw64-path (wrapped-function &rest args)
    (let ((exec-path (cl-list* "d:/msys64/mingw64/bin/"
                               "d:/msys64/usr/bin/"
                               exec-path))
          (restore (getenv "PATH")))
      (unwind-protect
           (progn
             (setenv "PATH" (mapconcat #'identity exec-path ";"))
             (apply wrapped-function args))
        (setenv "PATH" restore))))

  (advice-add 'gdb :around 'lbo:inject-mingw64-path)

  (advice-add 'compile :around 'lbo:inject-mingw64-path)
  (advice-add 'recompile :around 'lbo:inject-mingw64-path)

  (advice-add 'compile :around
              (lambda (wrapped &rest args)
                "Use a sane value for `default-directory'."
                (let* ((default-directory (locate-dominating-file default-directory ".git")))
                  (apply wrapped args)))
              '((name . lbo:default-directory-from-first-dot-git)
                (depth . 0)))

  (defun lbo:dont-ask-about-saving-outside-project-dir (wrapped &rest args)
    "Use a sane value for `default-directory'."
    (let* ((my-default-directory default-directory)
           (compilation-save-buffers-predicate
             (lambda () (string-prefix-p
                         my-default-directory
                         (file-truename (buffer-file-name))))))
      (apply wrapped args)))

  (advice-add 'compile :around
              'lbo:dont-ask-about-saving-outside-project-dir
              '((depth . 50)))

  (advice-add 'recompile :around 'lbo:dont-ask-about-saving-outside-project-dir))

(defun lbo:remove-all-advice (symbol)
  (advice-mapc (lambda (ad props)
                 (advice-remove symbol ad))
               symbol))

;;;; Java

(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))

;;;; Python

;(add-to-list 'load-path "~/.emacs.d/python-mode/")
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode)
;                             interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;;; Factor

(when mac-p
  (load-file "~/Software/factor/misc/fuel/fu.el"))

;;;; Common Lisp

(load "~/.emacs.d/my-slime-config.el")

(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)

;;;; Paredit

(lbo:ensure-package 'paredit)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

;; have RET automatically do indentation
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "RET") nil)
     (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)
     (diminish 'paredit-mode)))

;;; Elisp

(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))

(diminish 'eldoc-mode)

(use-package find-func
  :ensure nil
  :bind (:map emacs-lisp-mode-map ("M-." . find-function-at-point)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'compile-defun)))

;;;; Light-grey Parentheses

(use-package paren-face
  :if window-system
  :config (global-paren-face-mode))

;;;; Pretty Greek Alphabet

(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                 "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron"
                 "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi"
                 "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords
                 nil
                 `((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                     (0 (progn
                          (decompose-region (match-beginning 2) (match-end 2))
                          nil)))))
                (font-lock-add-keywords
                 nil
                 `((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                     (0 (progn
                          (compose-region (match-beginning 2) (match-end 2)
                                          ,greek-char)
                          nil)))))))))

;; (add-hook 'lisp-mode-hook 'pretty-greek)

;;;; Window number mode

(use-package wn-mode
  :diminish wn-mode
  :config (wn-mode))

;;;; Utilities

(defun max-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (cond (mac-p (set-frame-size (selected-frame) 210 58))
        ((and darwin-p x11-p) (set-frame-size (selected-frame) 199 69))))

;; (global-set-key (kbd "C-c m") 'max-frame)

(defun split-frame-in-3 ()
  (interactive)
  (split-window-horizontally)
  (wn-select-nth 1)
  (split-window-vertically)
  (enlarge-window 15)
  (wn-select-nth 2))

(global-set-key (kbd "C-c f") 'split-frame-in-3)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;; http://www.emacswiki.org/emacs/RevertBuffer#toc3
(defun revert-all-buffers ()
   "Refreshes all open buffers from their respective files"
   (interactive)
   (let* ((list (buffer-list))
          (buffer (car list)))
     (while buffer
       (when (buffer-file-name buffer)
         (progn
           (set-buffer buffer)
           (revert-buffer t t t)))
       (setq list (cdr list))
       (setq buffer (car list))))
  (message "Refreshing open files"))

;;;; Look and Feel

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(column-number-mode 1)
(when window-system
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(unless window-system
  (xterm-mouse-mode))

(unless mac-p
  (menu-bar-mode 0))

(setq default-indicate-empty-lines t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;(invert-face 'default)
(setq ring-bell-function 'ignore)

(use-package doom-themes
  :config
  ;; (load-theme 'doom-dark+ t)
  (load-theme 'doom-gruvbox t))

;; (use-package zenburn-theme
;;   :config (load-theme 'zenburn t))

;;;; Enable disabled functions

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;; Editing Keybindings

(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-DEL") 'join-line)
(global-set-key (kbd "C-<backspace>") 'join-line)

(global-set-key "\M-`" 'other-frame)
(global-unset-key (kbd "C-z"))

;;;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq haskell-program-name "/usr/local/bin/ghci -fobject-code")

;;;; Org Mode

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(lbo:ensure-package 'org)
(require 'org-install)

(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
;(setq org-startup-indented t) ; interesting alternative
(setq org-src-fontify-natively t)

(when siscog-p
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode t))

(setq org-agenda-custom-commands
      '(("n" "Agenda and NEXT tasks"
         ((tags-todo "next")
          (agenda "")))))

;(add-hook 'org-agenda-mode-hook (lambda () (org-agenda-day-view)))

(setq org-directory
      (if siscog-p
          "z:/org"
          "~/Dropbox/Documents/org"))

(setq org-agenda-files (list org-directory))

(setq org-default-notes-file (concat org-directory "/WORK.org"))
(define-key global-map (kbd "C-c o c") 'org-capture)

(setq org-capture-templates
      `(("t" "Todo" entry (file+olp ,(concat org-directory "/WORK.org") "CAPTURE")
             "* MAYBE %?\n  %i\n  %a")))

;; Add new TODO states
;;
;; C-c C-t followed by the key in parenthesis picks the respective
;; state.  S-<Right> and S-<Left> cycle through all of these states.
(setq org-todo-keywords
      '(;(sequence "TODO(t)" "|" "DONE(d)")
        (sequence "TODO(t)" "MAYBE(m)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELLED(c)")
        (sequence "OPEN(o)" "STARTED(s)" "VERIFICATION(v)" "|" "DELEGATED(l)" "RESOLVED(r)")
        (sequence "REVIEW(R)" "|" "REVIEWED(D)")))

(setq org-todo-keyword-faces
      '(("CANCELLED" . shadow)
        ("WAITING" . (:foreground "orange"))
        ("MAYBE" . (:foreground "orange"))
        ("WIP" . (:foreground "orange"))
        ("STARTED" . (:foreground "orange"))
        ("SEP" . (:foreground "orange"))))

;; The default was '(closed clock), show state changes as well.
(setq org-agenda-log-mode-items '(closed clock state))

(global-set-key (kbd "C-c o a")
                (lambda () (interactive) (org-agenda nil "n")))
(global-set-key (kbd "C-c o t") 'org-todo-list)

(defun my-open-main-org-file ()
  (interactive)
  (find-file (concat org-directory (if siscog-p "/WORK.org" "/LIFE.org")))
  (set-face-foreground 'org-hide (face-background 'default)))

(global-set-key (kbd "C-c o o") 'my-open-main-org-file)

(when siscog-p
  (global-set-key (kbd "C-c o e")
                  '(lambda () (interactive) (find-file (concat org-directory "/EFFORT.org")))))

(setq org-agenda-span 'day)

;;;; ELPA FTW

;; (lbo:package-require 'scala-mode2)

;; (let ((ensime-dir "~/src/scala/ensime/src/main/elisp"))
;;   (when (file-exists-p ensime-dir)
;;     (add-to-list 'load-path ensime-dir)
;;     (require 'ensime)
;;     (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;;;; Magit

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-file-dispatch)
         ("C-c d"   . magit-dispatch))
  :diminish auto-revert-mode
  :init (require 'vc-git)) ; for magit-grep

(add-to-list 'load-path "~/src/multi-magit")

(use-package multi-magit
  :bind ("C-x G" . multi-magit-status)
  :ensure nil
  :config (progn
            (unless magit-repository-directories
              (setq magit-repository-directories '(("~/src/" . 2))))
            (magit-add-section-hook 'magit-status-sections-hook
                                    'multi-magit-insert-repos-overview
                                    nil t)))

;; git >= 2.25 follows network/subst mapped drives which is annoying.
;; Thankfully `magit-toplevel' mostly handles this when
;; `find-file-visit-truename' is false.
(setq find-file-visit-truename nil)

;;;; Git grep

;; from https://gist.github.com/scottjacobsen/4693356
(defun lbo:git-root ()
  "Return GIT_ROOT if this file is a part of a git repo,
else return nil."
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat curdir ".git"))
          (setq found t)
        (setq curdir (concat curdir "../"))
        (setq max (- max 1))))
    (if found (expand-file-name curdir))))

;; like vc-git-grep but case insensitive
(defun lbo:vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
                                   nil nil 'grep-history)
             nil
             default-directory))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "In directory: "
                                          nil default-directory t)))
           (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
          (if (string= command "git grep")
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command
              ;; -I means "ignore binaries"
              (grep-expand-template "git --no-pager grep -I --no-color --ignore-case -n -e <R> -- <F>"
                                    regexp files))
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir)
              (compilation-environment (cons "PAGER=" compilation-environment)))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

(defun lbo:git-grep (regexp)
  (interactive (progn
                 (grep-compute-defaults)
                 (list (grep-read-regexp))))
  (lbo:vc-git-grep regexp "*" (lbo:git-root)))

(global-set-key (kbd "C-c g") 'lbo:git-grep)

;; C-c i toggle ignore case
;; C-! suspends auto update
;; C-x C-s moves results to a grep-mode buffer
;; (use-package helm-git-grep
;;   :bind ("C-c g" . lbo:helm-git-grep-at-point)
;;   :config (defun lbo:helm-git-grep-at-point ()
;;          (interactive)
;;          (helm-git-grep-at-point)))

;;;; Input Methods

;; Default setting for C-\
(setq default-input-method 'portuguese-prefix)

(global-set-key (kbd "C-c k p")
                (defun lbo:portuguese-input ()
                  (interactive)
                  (set-input-method 'portuguese-prefix)))

(global-set-key (kbd "C-c k e")
                (defun lbo:esperanto-input ()
                  (interactive)
                  (set-input-method 'esperanto-postfix)))

(global-set-key (kbd "C-c k l")
                (defun lbo:tex-input ()
                  (interactive)
                  (set-input-method 'TeX)))

;;;; bm

(use-package bm
  :bind (("<left-fringe> <wheel-up>" . bm-next-mouse)
         ("<left-fringe> <wheel-down>" . bm-previous-mouse)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
         ("<left-fringe> <mouse-3>" . bm-show-all))
  :config (setq bm-cycle-all-buffers t))

;;;; expand-region

(use-package expand-region
  :bind ("C-@" . er/expand-region))

;;;; multiple-cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;;; ace-jump-mode

(use-package avy
  :bind ("C-:" . avy-goto-char))

;;;; AMPL

(use-package ampl-mode
  :ensure nil
  :mode ("\\.mod$" "\\.ampl$"))

;;;; htmlize

(use-package htmlize
  :commands (lbo:export-buffer-to-html
             lbo:export-region-to-html)
  :config
  (defmacro lbo:with-disabled-themes (&rest body)
    (let ((themes (gensym)))
      `(let ((,themes custom-enabled-themes))
         (mapc #'disable-theme ,themes)
         (unwind-protect (progn ,@body)
           (mapc #'enable-theme ,themes)))))

  (defun lbo:browse-htmlized-buffer (prefix buffer)
    (with-current-buffer buffer
      (let ((file (make-temp-file prefix nil ".html")))
        (write-file file)
        (browse-url (concat "file://" file)))
      (kill-buffer)))

  (defun lbo:export-buffer-to-html ()
    (interactive)
    (lbo:with-disabled-themes
      (lbo:browse-htmlized-buffer "htmlized-buffer-" (htmlize-buffer))))

  (defun lbo:export-region-to-html ()
    (interactive)
    (let (transient-mark-mode-enabled transient-mark-mode)
      (lbo:with-disabled-themes
        (transient-mark-mode -1)
        (redisplay)
        (lbo:browse-htmlized-buffer "htmlized-region-"
                                    (htmlize-region (region-beginning)
                                                    (region-end)))
        (transient-mark-mode (if transient-mark-mode-enabled 1 -1))))))

;;;; The End

(setq auto-save-list-file-prefix "~/.asl-emacs/saves-")
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq confirm-kill-emacs 'yes-or-no-p)

(unless org-only-mode-p
  (ido-mode)
  ;; don't search for files outside the current directory in
  ;; `ido-find-file'.
  (setq ido-auto-merge-work-directories-length -1))

;; (autoload 'idomenu "idomenu" nil t)
;; (global-set-key (kbd "M-i") 'idomenu)

(random t)

(when org-only-mode-p
  (setq inhibit-startup-message t)
  (my-open-first-agenda-file))

;; (load "~/.emacs.d/my-jabber-config.el")

(when siscog-p
  (load "~/.emacs.d/siscog/gnus-config.el")
  (when gnus-only-mode-p
    (setq inhibit-startup-message t)
    (gnus)))

(when roster-only-mode-p
  (setq inhibit-startup-message t)
  (gtalk))

(eval-after-load 'sml-mode
  '(setq sml-program-name "/usr/local/Cellar/smlnj/110.75/libexec/bin/sml"))

(lbo:ensure-package 'circe)

(eval-after-load 'circe
  '(progn
    (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
    (circe-set-display-handler "PART" (lambda (&rest ignored) nil))
    (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))
    ;; see <https://github.com/jorgenschaefer/circe/issues/340>
    (setq tls-end-of-info
          (concat
           "\\("
           ;; `openssl s_client' regexp.  See ssl/ssl_txt.c lines 219-220.
           ;; According to apps/s_client.c line 1515 `---' is always the last
           ;; line that is printed by s_client before the real data.
           "^    Verify return code: .+\n\\(\\|^    Extended master secret: .+\n\\)---\n\\|"
           ;; `gnutls' regexp. See src/cli.c lines 721-.
           "^- Simple Client Mode:\n"
           "\\(\n\\|"                   ; ignore blank lines
           ;; According to GnuTLS v2.1.5 src/cli.c lines 640-650 and 705-715
           ;; in `main' the handshake will start after this message.  If the
           ;; handshake fails, the programs will abort.
           "^\\*\\*\\* Starting TLS handshake\n\\)*"
           "\\)"))
    (setq circe-network-options '(("kerno"
                                   :tls t
                                   :nick "luis"
                                   :host "kerno.org"
                                   :port 6697)))))

;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t)
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "siscog"
;;    :default t
;;    :client-id ""
;;    :client-secret ""
;;    :token ""
;;    :subscribed-channels '()))

(lbo:ensure-package 'helm-gtags)

(eval-after-load 'helm-gtags
  '(progn
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;;; Helm

;; (use-package helm-config
;;   :ensure helm
;;   :demand t
;;   :bind (("C-c C-/" . helm-occur)
;;          ("C-c i"   . helm-imenu)
;;          ("C-x f"   . helm-multi-files)
;;          ("C-c C-f" . helm-find-files)
;;          ("M-H"     . helm-resume)
;;          ("M-x"     . helm-M-x))
;;   :config
;;   (require 'helm-files)
;;   (require 'helm-buffers)
;;   (use-package helm-mode
;;     :ensure nil
;;     :diminish helm-mode
;;     :init
;;     (helm-mode 1))

;;   (require 'helm-multi-match)

;;   (helm-autoresize-mode 1)

;;   (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
;;   (bind-key "C-i" #'helm-execute-persistent-action helm-map)
;;   (bind-key "C-z" #'helm-select-action helm-map))

;;;; Ivy / Counsel / Swiper

;; (use-package ivy
;;   :config (ivy-mode 1))

;; (use-package counsel
;;   :config (counsel-mode 1))

;; (use-package swiper
;;   :bind ("C-s" . swiper))

;;;; Selectrum

(use-package selectrum
  :config (selectrum-mode 1))

(use-package selectrum-prescient
  :config (progn
            (selectrum-prescient-mode 1)
            (prescient-persist-mode 1)))

;;;; Marginalia

(use-package marginalia
  :config (progn
            (marginalia-mode)
            (marginalia-cycle)))

(use-package embark
  :bind
  ("C-S-a" . embark-act))              ; pick some comfortable binding

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package ctrlf
  :config (progn
            (setq ctrlf-mode-bindings
                  '(("C-S-s" . ctrlf-forward-literal)
                    ("C-S-r" . ctrlf-backward-literal)
                    ("C-M-S-s" . ctrlf-forward-regexp)
                    ("C-M-S-r" . ctrlf-backward-regexp)
                    ("C-c ."   . ctrlf-forward-symbol-at-point)
                    ("C-c _"   . ctrlf-forward-symbol)))
            (setq ctrlf-minibuffer-bindings
                  `(("<down>" . ctrlf-next-match)
                    ("<up>"   . ctrlf-previous-match)
                    ("C-w"    . next-history-element)
                    ,@ctrlf-minibuffer-bindings))
            (ctrlf-mode)))

(define-prefix-command 'lbo:consult-map)
(global-set-key (kbd "M-c") 'lbo:consult-map)

(use-package consult
  :bind (;; M-g bindings (goto-map)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g e" . consult-error)
         ;; M-c
         ("M-c 4 b" . consult-buffer-other-window)
         ("M-c 5 b" . consult-buffer-other-frame)
         ("M-c b"   . consult-buffer)
         ("M-c B"   . consult-bookmark)
         ("M-c f" . consult-find)
         ("M-c h" . consult-history)
         ("M-c i" . consult-imenu)
         ("M-c o" . consult-outline)
         ("M-c p" . consult-project-imenu)
         ("M-c r" . consult-register)
         ("M-c s" . consult-line)
         ("M-c G" . consult-grep)
         ("M-c g" . consult-git-grep)
         ("M-c k" . consult-keep-lines)
         ("M-c l" . consult-line)
         ("M-c m" . consult-multi-occur)
         ("M-c u" . consult-focus-lines)
         ("M-c x" . consult-mode-command)
         ;; Other bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :config (setq consult-narrow-key "<"))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;;;; smartscan

(use-package smartscan
  :bind (("C-c n" . smartscan-symbol-go-forward)
         ("C-c p" . smartscan-symbol-go-backward)))

;;;; spaceline

;; (use-package spaceline
;;   :demand t
;;   :init (setq powerline-default-separator 'arrow-fade
;;            powerline-height 25)
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

(use-package powerline
  :demand t
  :init (setq powerline-default-separator 'slant
              powerline-height 25)
  :config
  (powerline-default-theme)
  (defun lbo:powerline-reset-after (&rest _args)
    (powerline-reset))
  (advice-add #'enable-theme :after 'lbo:powerline-reset-after)
  (advice-add #'disable-theme :after 'lbo:powerline-reset-after)

  ;; adapted from <https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line>
  (when (fboundp 'all-the-icons-octicon)
    (defpowerline powerline-vc
      (when (and (buffer-file-name (current-buffer)) vc-mode)
        (if (string-match "Git[:-]" vc-mode)
            (let ((branch (subseq vc-mode (match-end 0))))
              (concat
               (propertize (format " %s " (all-the-icons-octicon "git-branch"))
                           'face `(:height 1.3 :family ,(all-the-icons-octicon-family) :inherit ,face)
                           'display '(raise -0.15))
               branch))
          (format-mode-line '(vc-mode vc-mode)))))))

;;;; dired

(setq-default dired-listing-switches "-alh")
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x)
(setq dired-omit-files "^\\.?#")
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode)
            (dired-hide-details-mode)))

;; (setq ls-lisp-use-insert-directory-program t)
;; (setq ls-list-verbosity nil)

;; (defadvice ls-lisp-format (around my-ls-lisp-format
;;   (file-name file-attr file-size switches time-index))
;;   "Advice definition which removes unnecessary information
;; during file listing in dired. For such purposes
;; `ls-lisp-verbosity' customized variable can be used, but
;; even if it is equal to nil dired will display file
;; permissions field like \"drwxrwxrwx\".\. So here we just
;; get full control to what dired shows and leave only those
;; fields which we need."
;;   (progn
;;     ad-do-it
;;     (setq ad-return-value
;;           (concat
;;            ;; (substring ad-return-value 0 1)
;;            (substring ad-return-value 10))))) ; 13
;; (ad-activate 'ls-lisp-format t)

;; (use-package dired-narrow
;;   :bind (:map dired-mode-map ("/" . dired-narrow)))

;; (use-package peep-dired
;;   :bind (:map dired-mode-map ("P" . peep-dired)))

;; (setq package-check-signature nil)

;; (use-package dired
;;   :hook (dired-mode . dired-hide-details-mode)
;;   :ensure nil
;;   :config
;;   ;; Colourful columns.
;;   (use-package diredfl
;;     :config
;;     (diredfl-global-mode 1)))

;; (use-package dired-git-info
;;   :bind (:map dired-mode-map
;;               (")" . dired-git-info-mode)))

;;;; hunspell

;; http://sourceforge.net/projects/ezwinports/files/?source=navbar
;; http://natura.di.uminho.pt/download/sources/Dictionaries/hunspell/LATEST/hunspell-pt_PT-latest.tar.gz
;;
;; M-x ispell-change-dictionary
;; M-x ispell-region
;; M-x ispell-buffer
(when siscog-p
  (eval-after-load 'ispell
    `(progn
       (add-to-list 'exec-path "d:/opt/hunspell/bin")
       (setq ispell-program-name "hunspell")
       (setq ispell-library-directory "d:/opt/hunspell/share/hunspell")
       (setq ispell-local-dictionary-alist
             '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil iso-8859-1)
               ("portugues" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "pt_PT") nil iso-8859-1))))))

;;;; grammar checker

(when siscog-p
  (eval-after-load 'langtool
                   `(progn
                      (setq langtool-language-tool-jar "d:/opt/LanguageTool/languagetool-commandline.jar"))))

;;;; transparency

(defun lbo:transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive (let ((current-alpha (frame-parameter (selected-frame) 'alpha)))
                 (list (read-number (format "Set frame alpha: " current-alpha)
                                     current-alpha))))
  (set-frame-parameter (selected-frame) 'alpha value))

;;;; Ligatures

(use-package ligature
  :load-path "~/src/elisp/ligature.el/"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures t ;; '(prog-mode slime-editing-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ;; ">="
                            ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$"
                            ;; "<="
                            "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??"
                            ;; ";;"
                            "/*" "/=" "/>" "//" "__" "~~"
                            ;; "(*" "*)"
                            "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it per
  ;; mode with `ligature-mode'.
  (global-ligature-mode -1))

;;;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "dbb643699e18b5691a8baff34c29d709a3ff9787f09cdae58d3c1bc085b63c25" "75cd4234cc08d4ccf3ddef8fb763b9e145d4e68d3c938a3502d892c72f71e007" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(default-input-method "portuguese-prefix")
 '(package-selected-packages
   '(doom-themes color-theme-sanityinc-tomorrow monokai-theme solarized-theme mini-frame which-key marginalia selectrum alert circe dired dired-git-info markdown-mode dockerfile-mode groovy-mode yaml-mode magit macrostep package-lint rainbow-delimiters doom-modeline slime auto-compile magit-gerrit spaceline spacemacs-theme typescript-mode mo-git-blame parinfer dracula-theme wgrep zone-select zone-nyan zones wttrin wn-mode white-sand-theme tangotango-theme shorten request-deferred redshank parenface org-bullets moe-theme minesweeper meacupla-theme lui lcs langtool htmlize goto-last-change gitignore-mode folding ethan-wspace darkroom crux color-theme-sanityinc-solarized aggressive-indent 2048-game))
 '(safe-local-variable-values
   '((indent-tabs)
     (org-src-fontify-natively . t)
     (c-style . "stroustrup")
     (org-clock-continuously . t)
     (eval font-lock-add-keywords nil
           '(("(\\(dvar\\|maximize\\|minimize\\|s.t.\\|for\\|where\\|sum\\) " 1 font-lock-keyword-face)))
     (c-style . "K&R")
     (intent-tabs-mode)
     (c-style . K&R)
     (encoding . latin-1-dos)
     (org-clock-into-drawer))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(parenthesis ((t (:foreground "DimGray")))))
