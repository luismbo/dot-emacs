;;;; -*- coding: utf-8 -*-

;;; Figuring out which system we're in.
;;; We might be running on MacOSX but using X11.
(defvar darwin-p (string-match "darwin" (symbol-name system-type)))
(defvar x11-p (eq window-system 'x))
(defvar mac-p (or (eq window-system 'ns) (eq window-system 'mac)))
(defvar olpc-p (string-match "olpc" (user-login-name)))
(defvar siscog-p (string-match "luismbo" (user-login-name)))

(defun lbo:imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'lbo:imenu-elisp-sections)

;;; emacs ... -T ORG
(defvar org-only-mode-p
  (string= "ORG" (frame-parameter (selected-frame) 'title)))

;;; emacs ... -T JABBER
(defvar roster-only-mode-p
  (string= "JABBER" (frame-parameter (selected-frame) 'title)))

(defvar gnus-only-mode-p
  (string= "GNUS" (frame-parameter (selected-frame) 'title)))

(require 'cl)

;;;; Exec Path

(when mac-p
  (add-to-list 'exec-path "/usr/local/bin"))

;;;; Load Path

(add-to-list 'load-path "~/.emacs.d/lib")

;;;; package.el

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

;;;; SISCOG

(when siscog-p
  (load "~/.emacs.d/siscog/site.el")
  (unless (or roster-only-mode-p org-only-mode-p gnus-only-mode-p)
    (load "~/.emacs.d/my-siscog-config.el"))
  (unless (or gnus-only-mode-p roster-only-mode-p)
    (load "~/.emacs.d/my-siscog-org-config.el")
    (setq display-time-format "-%H:%M-")
    (display-time)))

;;;; Editing Stuff

(setq-default indent-tabs-mode nil) ; DIE TABS!!
(set-language-environment "UTF-8")

(global-font-lock-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(when mac-p
  (setq browse-url-browser-function
        (lambda (url &optional new-window)
          (message url)
          (do-applescript (concat "open location \"" url "\"")))))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))

;;;; Goto Last Change

(lbo:ensure-package 'goto-last-change)
(global-set-key (kbd "C-c \\") 'goto-last-change)

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
        (siscog-p "Consolas-10")
        ((> emacs-major-version 22) "Monospace-10")
        (t "-*-*-*-*-*-*-13-*-*-*-*-*-*-*")))

(set-frame-font *default-font* t t)

(global-set-key [C-M-wheel-up] 'text-scale-increase)
(global-set-key [C-M-wheel-down] 'text-scale-decrease)

;;;; C

(defun my-c-mode ()
  (c-set-style "K&R")
  (setq c-basic-offset 4
        indent-tabs-mode nil))

(add-hook 'c-mode-hook 'my-c-mode)
(add-hook 'c++-mode-hook 'my-c-mode)

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

(add-hook 'lisp-mode-hook (lambda () (setq show-trailing-whitespace t)))

;;;; Paredit

(lbo:ensure-package 'paredit)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

;; have RET automatically do indentation
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "RET") nil)
     (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)))

;;; Elisp

(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))

;;;; Light-grey Parentheses

(when window-system
  (lbo:ensure-package 'paren-face)
  (global-paren-face-mode))

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

(lbo:ensure-package 'wn-mode)
(wn-mode)

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
(scroll-bar-mode -1)
(tool-bar-mode -1)

(unless mac-p
  (menu-bar-mode 0))

(setq default-indicate-empty-lines t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;(invert-face 'default)
(setq ring-bell-function 'ignore)

(when window-system
  (cond (roster-only-mode-p
         (color-theme-dark-laptop))
        (org-only-mode-p
         (lbo:ensure-package 'solarized-theme)
         (load-theme 'solarized-light t))
        (t
         (lbo:ensure-package 'zenburn-theme)
         (load-theme 'zenburn t))))

;;;; Enable disabled functions

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; Editing Keybindings

(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-DEL") 'join-line)
(global-set-key (kbd "C-<backspace>") 'join-line)

(global-set-key "\M-`" 'other-frame)
(global-unset-key (kbd "C-z"))

;;;; Haskell

(load "~/.emacs.d/haskell-mode/haskell-site-file.el")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq haskell-program-name "/usr/local/bin/ghci -fobject-code")

;;;; Clojure

;(add-to-list 'load-path "~/source/clojure-mode")
;(require 'clojure-auto)
;(require 'clojure-paredit)

;(add-to-list 'load-path "~/source/clojure-swank")
;(setq swank-clojure-binary "~/source/clojure-extra/sh-script/clojure")
;(require 'swank-clojure-autoload)

;; (defun clojure ()
;;   (interactive)
;;   (require 'swank-clojure)
;;   (add-to-list 'slime-lisp-implementations
;;                `(clojure ,(swank-clojure-cmd) :init swank-clojure-init) t)
;;   (add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
;;   (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
;;   (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)
;;   (slime 'clojure))

;;;; Ruby (on Rails)

;(add-to-list 'load-path "~/.emacs.d/rinari/")
;(require 'rinari)

(add-hook 'ruby-mode-hook 'robe-mode)

;;;; NXHTML

;; (unless siscog-p
;;   (load "~/.emacs.d/nxhtml/autostart.el")

;;   (setq nxhtml-global-minor-mode t
;;         mumamo-chunk-coloring 'submode-colored
;;         nxhtml-skip-welcome t
;;         indent-region-mode t
;;         rng-nxml-auto-validate-flag nil
;;         nxml-degraded t)

;;   (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo)))

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
          "w:/org"
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

(lbo:ensure-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;; Input Methods

;; Default setting for C-\
(setq default-input-method 'portuguese-prefix)

(global-set-key (kbd "C-c k p")
                (lambda () (interactive) (set-input-method 'portuguese-prefix)))

(global-set-key (kbd "C-c k e")
                (lambda () (interactive) (set-input-method 'esperanto-postfix)))

(global-set-key (kbd "C-c k l")
                (lambda () (interactive) (set-input-method 'TeX)))

;;;; expand-region

(lbo:ensure-package 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;;; multiple-cursors

(lbo:ensure-package 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;; ace-jump-mode

(lbo:ensure-package 'avy)
(global-set-key (kbd "C-S-j") 'avy-goto-char)

;;;; AMPL

(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)

(add-to-list 'auto-mode-alist '("\\.mod$" . ampl-mode))
(add-to-list 'auto-mode-alist '("\\.ampl$" . ampl-mode))

;;;; htmlize

(defun lbo:export-buffer-to-html ()
  (interactive)
  (let ((themes custom-enabled-themes))
    (mapc #'disable-theme themes)
    (unwind-protect
         (with-current-buffer (htmlize-buffer)
           (let ((file (make-temp-file "htmlized-buffer-" nil ".html")))
             (write-file file)
             (browse-url file))
           (kill-buffer))
      (mapc #'enable-theme themes))))

;;;; The End

(setq auto-save-list-file-prefix "~/.asl-emacs/saves-")
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq confirm-kill-emacs 'yes-or-no-p)

(unless org-only-mode-p
  (ido-mode))

(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "M-i") 'idomenu)

(random t)

(when org-only-mode-p   
  (setq inhibit-startup-message t)
  (my-open-first-agenda-file))

(load "~/.emacs.d/my-jabber-config.el")

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
    (circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))))

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

;;;; dired

(setq dired-listing-switches "-alh")
