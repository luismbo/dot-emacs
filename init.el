;;;; Dot Emacs, Luís Oliveira <luis@r42.eu>                                    *
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
              ((> emacs-major-version 22) "Monospace-9")
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

(unless (or olpc-p siscog-p)
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

(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))

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
    ;(load "~/.emacs.d/ercpass.el")
    (erc-scrolltobottom-enable)
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

(global-set-key (kbd "C-c m") 'max-frame)

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

(column-number-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(unless mac-p
  (menu-bar-mode nil))

(setq default-indicate-empty-lines t)

;(invert-face 'default)
(setq ring-bell-function 'ignore)

(cond
  ((or darwin-p siscog-p) (load "~/.emacs.d/color-theme.el"))
  ((not olpc-p) (require 'color-theme)))

;;(funcall (car (nth 5 color-themes)))
;;(funcall (car (nth 86 color-themes)))
;;(funcall (car (nth 4 color-themes)))

(when (and (not olpc-p) window-system)
  (cond (roster-only-mode-p
         (color-theme-dark-laptop))
        (org-only-mode-p
         (set-face-background 'default "grey90"))
        ((or mac-p siscog-p)
         (color-theme-dark-laptop)
         (set-face-background 'default "grey10")
         (set-face-background 'tooltip "white"))
        (t
         (color-theme-robin-hood))))

;;;; Enable disabled functions

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; Keybindings

(defun lisp-keys ()
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

(defun normal-keys ()
  (interactive)
  (keyboard-translate ?\( ?\()
  (keyboard-translate ?\[ ?\[)
  (keyboard-translate ?\) ?\))
  (keyboard-translate ?\] ?\]))

(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-DEL") 'join-line)
(global-set-key (kbd "C-<backspace>") 'join-line)

(when siscog-p
  (global-set-key (kbd "M-DEL") 'backward-kill-word))

(when mac-p
  (global-set-key "\M-`" 'other-frame))

;;;; Haskell

(when mac-p
  (add-to-list 'load-path "~/.emacs.d/haskell-mode/"))

(add-hook 'haskell-mode-hook
          (lambda ()
            (require 'inf-haskell)
            (setq haskell-program-name "/opt/local/bin/ghci")))

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

;;;; Org Mode

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp/")
(require 'org-install)

(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
;(setq org-startup-indented t) ; interesting alternative

(when siscog-p
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-with-clockreport-mode t))

(when siscog-p
  (add-hook 'org-mode-hook
            (lambda() (add-hook 'before-save-hook
                                'org-agenda-to-appt t t))))

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
        (sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELLED(c)")
        (sequence "OPEN(o)" "WIP(i)" "REVIEW(v)" "|" "SEP(s)" "RESOLVED(r)")))

;; Give WAITING and CANCELLED some color.
(setq org-todo-keyword-faces
      '(("CANCELLED" . shadow)
        ("WAITING" . (:foreground "orange"))
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
(load "~/.emacs.d/magit/50magit.el")

(when mac-p
  (eval-after-load 'magit
    (setq magit-git-executable "/usr/local/bin/git")))

;;;; w3m

(add-to-list 'load-path "~/.emacs.d/emacs-w3m/")
(require 'w3m-load)

(when siscog-p
  (add-to-list 'exec-path "d:/cygwin/bin/")
  (setq w3m-home-page "http://intranet"))

;(setq w3m-use-cookies t)

(global-set-key (kbd "C-c g") 'w3m-search) ; google search

(defun my-dictionary-lookup (word)
  (interactive "sProcurar palavra no dicionário: ")
  (w3m-browse-url
   (concat "http://www.priberam.pt/DLPO/default.aspx?pal="
           (url-hexify-string word))
   t))

(global-set-key (kbd "C-c d") 'my-dictionary-lookup)

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
(setq confirm-kill-emacs 'yes-or-no-p)

(unless org-only-mode-p
  (ido-mode))

(random t)

(when org-only-mode-p
  (setq inhibit-startup-message t)
  (my-open-first-agenda-file))

(when roster-only-mode-p
  (load "~/.emacs.d/my-jabber-config.el")
  (setq inhibit-startup-message t)
  (gtalk))
