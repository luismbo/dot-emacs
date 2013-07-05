;;;; -*- coding: utf-8 -*-

;;; Figuring out which system we're in.
;;; We might be running on MacOSX but using X11.
(defvar darwin-p (string-match "darwin" (symbol-name system-type)))
(defvar x11-p (eq window-system 'x))
(defvar mac-p (or (eq window-system 'ns) (eq window-system 'mac)))
(defvar olpc-p (string-match "olpc" (user-login-name)))
(defvar siscog-p (string-match "luismbo" (user-login-name)))

;;; emacs ... -T ORG
(defvar org-only-mode-p
  (string= "ORG" (frame-parameter (selected-frame) 'title)))

;;; emacs ... -T JABBER
(defvar roster-only-mode-p
  (string= "JABBER" (frame-parameter (selected-frame) 'title)))

(require 'cl)

;;;; HACKS

;(when mac-p
;  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))

;;;; Load Path

(add-to-list 'load-path "~/.emacs.d/")

;;;; Siscog

(when siscog-p
  (require 'tramp)
  (setq grep-find-use-xargs 'gnu)
  (unless (or roster-only-mode-p org-only-mode-p)
    (load "~/.emacs.d/my-siscog-config.el"))
  (unless roster-only-mode-p
    (load "~/.emacs.d/my-siscog-org-config.el")
    (setq display-time-format "-%H:%M-")
    (display-time)))

;;;; Editing Stuff

(setq-default indent-tabs-mode nil) ; DIE TABS!!
(unless (and siscog-p (not org-only-mode-p) (not roster-only-mode-p))
  (set-language-environment "UTF-8"))
(global-font-lock-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(when mac-p
  (setq browse-url-browser-function
        (lambda (url &optional new-window)
          (message url)
          (do-applescript (concat "open location \"" url "\"")))))

;;;; Browsers

(when siscog-p
  (defvar browse-url-chm-program "d:/cygwin/opt/bin/KeyHH.exe")

  (defvar browse-url-chm-program-args '("-emacs"))

  (defun browse-url-chm (url &rest args)
    (with-temp-buffer
      (let ((process (apply 'start-process
                            "CHMBrowser"
                            nil
                            browse-url-chm-program
                            (append browse-url-chm-program-args (list url)))))
        (process-kill-without-query process))))

  (defun lbo:browse-url-internet-explorer (url &rest ignore)
    (interactive "sURL: ")
    (shell-command (concat "explorer " (shell-quote-argument url))))

  (setq browse-url-browser-function
        '(("\\.chm" . browse-url-chm)
          ("pms.siscog.com" . lbo:browse-url-internet-explorer)
          ("." . browse-url-default-windows-browser))))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))

;;;; Goto Last Change

(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
 
(global-set-key (kbd "C-c \\") 'goto-last-change)

;;;; Fonts

(when mac-p
  ;(setq mac-allow-anti-aliasing nil)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(let ((font (cond
              (mac-p "Menlo-11")
              (olpc-p "Monospace-7")
              (siscog-p "Consolas-10")
              ((> emacs-major-version 22) "Monospace-10")
              (t "-*-*-*-*-*-*-13-*-*-*-*-*-*-*"))))
  (set-default-font font)
  (add-to-list 'default-frame-alist (cons 'font font)))

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

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))

(add-hook 'emacs-lisp-mode (lambda () (paredit-mode 1)))

;; have RET automatically do indentation
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "RET") nil)
     (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)))

;;;; Light-grey Parentheses

(when window-system
  (require 'parenface))

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

(load "~/.emacs.d/wn-mode.el")
(wn-mode)

;;;; ERC

;;(add-to-list 'load-path "~/.emacs.d/erc-5.0.4/")
(autoload 'erc-select "erc" "IRC client." t)

(eval-after-load "erc"
  '(progn
    ;; (require 'erc-services)
    ;; (erc-services-mode 1)
    ;; (setq erc-prompt-for-nickserv-password nil)
    ;; ~/.emacs.d/ercpass.el contains something like:
    ;;
    ;;    (setq erc-nickserv-passwords
    ;;          '((freenode (("luis" . "pass1")
    ;;                       ("luis`" . "pass2")))))
    (unless siscog-p
      (load "~/.emacs.d/ercpass.el"))
    (require 'erc-truncate)
    (erc-scrolltobottom-enable)
    (setq erc-input-line-position -1)
    ;; (setq erc-autojoin-channels-alist '(("freenode.net" "#lisp" "#lisp-pt")))
    (when siscog-p
      (setq erc-hide-list '("JOIN" "PART" "QUIT")))))

(defun erc-connect-freenode ()
  (interactive)
  (erc :server "nhop.r42.eu"
       :port 6667
       :password (if siscog-p
                     (read-passwd "ERC password: ")
                     lbo:nhop-bouncer-password)
       :nick "luis"))

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

;;;; Cycle Buffer

(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(global-set-key (kbd "C-c b") 'cycle-buffer)

;;;; Look and Feel

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(column-number-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(unless mac-p
  (menu-bar-mode 0))

(setq default-indicate-empty-lines t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;(invert-face 'default)
(setq ring-bell-function 'ignore)

(add-to-list 'custom-theme-load-path
             "~/.emacs.d/lib/emacs-color-theme-solarized/")

(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs/")

(when window-system
  (cond (roster-only-mode-p
         (color-theme-dark-laptop))
        (org-only-mode-p
         ;; (set-face-background 'default "grey90")
         (load-theme 'solarized-light t))
        (mac-p
         (load-theme 'solarized-dark t)
         ;; (load-theme 'solarized-light t)
         ;; (load "~/.emacs.d/zenburn-emacs/zenburn-theme.el")
         ;; (color-theme-dark-laptop)
         ;; (set-face-background 'default "grey12")
         ;; (set-face-background 'tooltip "white")
         )
        (siscog-p
         (load-theme 'zenburn t))
        (t
         (color-theme-robin-hood))))

;;;; Enable disabled functions

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; Editing Keybindings

(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-DEL") 'join-line)
(global-set-key (kbd "C-<backspace>") 'join-line)

(when siscog-p
  (global-set-key (kbd "M-DEL") 'backward-kill-word))

(when mac-p
  (global-set-key "\M-`" 'other-frame))

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

(unless siscog-p
  (load "~/.emacs.d/nxhtml/autostart.el")

  (setq nxhtml-global-minor-mode t
        mumamo-chunk-coloring 'submode-colored
        nxhtml-skip-welcome t
        indent-region-mode t
        rng-nxml-auto-validate-flag nil
        nxml-degraded t)

  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo)))

;;;; Org Mode

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp/")
(require 'org-install)

(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
;(setq org-startup-indented t) ; interesting alternative

(when siscog-p
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-with-clockreport-mode t))

;(add-hook 'org-agenda-mode-hook (lambda () (org-agenda-day-view)))

(setq org-agenda-files
      (if siscog-p
          '("w:/org/WORK.org" "w:/org/EFFORT.org")
          '("~/Dropbox/Documents/org/LIFE.org")))

;; Add new TODO states: WAITING and CANCELLED.
;;
;; C-c C-t followed by the key in parenthesis picks the respective
;; state.  S-<Right> and S-<Left> cycle through all of these states.
(setq org-todo-keywords
      '(;(sequence "TODO(t)" "|" "DONE(d)")
        (sequence "TODO(t)" "MAYBE(m)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELLED(c)")
        (sequence "OPEN(o)" "WIP(i)" "REVIEW(v)" "|" "SEP(s)" "RESOLVED(r)")))

;; Give WAITING and CANCELLED some color.
(setq org-todo-keyword-faces
      '(("CANCELLED" . shadow)
        ("WAITING" . (:foreground "orange"))
        ("MAYBE" . (:foreground "orange"))
        ("WIP" . (:foreground "orange"))
        ("SEP" . (:foreground "orange"))))

;; The default was '(closed clock), show state changes as well.
(setq org-agenda-log-mode-items '(closed clock state))

(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)

(defun my-open-first-agenda-file ()
  (interactive)
  (find-file (first org-agenda-files))
  (set-face-foreground 'org-hide (face-background 'default)))

(global-set-key (kbd "C-c o o") 'my-open-first-agenda-file)

;;;; Magit

(add-to-list 'load-path "~/.emacs.d/magit/")
(load "~/.emacs.d/magit/magit.el")

(when mac-p
  (eval-after-load 'magit
    (setq magit-git-executable "/usr/local/bin/git")))

;;;; ELPA FTW

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;; Input Methods

;; Default setting for C-\
(setq default-input-method 'portuguese-prefix)

(global-set-key (kbd "C-c k p")
                (lambda () (interactive) (set-input-method 'portuguese-prefix)))

(global-set-key (kbd "C-c k e")
                (lambda () (interactive) (set-input-method 'esperanto-postfix)))

(global-set-key (kbd "C-c k l")
                (lambda () (interactive) (set-input-method 'TeX)))

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
(when roster-only-mode-p
  (setq inhibit-startup-message t)
  (gtalk))

(eval-after-load 'sml-mode
  '(setq sml-program-name "/usr/local/Cellar/smlnj/110.75/libexec/bin/sml"))

(eldoc-mode 1)
