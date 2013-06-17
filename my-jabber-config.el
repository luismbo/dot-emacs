;;;; jabber.el -*- coding: utf-8 -*-

(when siscog-p
  (setenv "PATH" (format "d:\\cygwin\\bin;d:\\cygwin\\usr\\bin;%s"
                         (getenv "PATH"))))

(add-to-list 'load-path "~/.emacs.d/emacs-jabber/")

(require 'jabber-autoloads)

(eval-after-load 'jabber
  `(progn
     ;; Faces
     (set-face-foreground 'jabber-chat-prompt-local "OrangeRed4")
     (cond (siscog-p
            (set-face-foreground 'jabber-chat-text-local "OrangeRed4")
            (set-face-foreground 'jabber-chat-prompt-foreign "OrangeRed3")
            (set-face-foreground 'jabber-chat-text-foreign "OrangeRed3"))
           (t
            (set-face-foreground 'jabber-chat-text-local "OrangeRed1")
            (set-face-foreground 'jabber-chat-prompt-foreign "orange1")))
     (set-face-foreground 'jabber-roster-user-online "LimeGreen")
     (set-face-foreground 'jabber-roster-user-away "YellowGreen")
     (set-face-foreground 'jabber-roster-user-dnd "IndianRed")
     (set-face-foreground 'jabber-activity-face "yellow4")
     (set-face-foreground 'jabber-activity-personal-face "yellow4")
     (set-face-attribute 'jabber-title-medium nil
                         :width 'unspecified :height 'unspecified)
     ;; Roster Options
     (setq jabber-vcard-avatars-retrieve nil)
     (setq jabber-roster-show-title nil)
     (setq jabber-roster-show-bindings nil)
     (setq jabber-show-offline-contacts nil)
     (setq jabber-show-resources nil)
     (setq jabber-sort-order nil)
     ;; Chat Options
     (add-hook 'jabber-roster-mode-hook (lambda () (setq truncate-lines t)))
     (setq jabber-chat-local-prompt-format "[%t] Luís> ")
     (add-hook 'jabber-chat-mode-hook
               (lambda ()
                 (visual-line-mode t)
                 (set-input-method 'portuguese-prefix)))
     ;; Misc Options
     (setq jabber-default-status "SISCOG")
     (setq jabber-default-show "dnd")
     (setq jabber-alert-presence-hooks nil)
     (setq jabber-alert-message-hooks '(jabber-message-scroll))

     (setq jabber-activity-count-in-title t)))

(defun lbo:read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun gtalk ()
  (interactive)
  (let ((jabber-account-list
         `(("luismbo@gmail.com"
            (:password . ,(when siscog-p (lbo:read-file "w:/.jabber-password")))
            (:network-server . ,(if siscog-p
                                    "localhost"
                                    "talk.google.com"))
            (:port . 443)
            (:connection-type . ssl)))))
    (jabber-connect-all)))


(when siscog-p
  (global-set-key (kbd "C-z")
                  (defun my-minimize-window ()
                    (interactive)
                    (switch-to-buffer "*scratch*")
                    (suspend-frame))))

(defun gtalk-groupchat ()
  (interactive)
  (let ((group (apply 'format "private-chat-%x%x%x%x%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x%x%x%x%x%x%x%x%x@groupchat.google.com"
                      (mapcar (lambda (x) (random x)) (make-list 32 15))))
        (account (jabber-read-account)))
    (jabber-groupchat-join account group (jabber-muc-read-my-nickname account group) t)))
