;;;; -*- coding: utf-8 -*-

(setq user-mail-address "luismbo@siscog.pt")
(setq user-full-name "Luís Borges de Oliveira")

(setq gnus-select-method
      '(nnimap "siscog"
        (nnimap-address "imaps.siscog.com")
        (nnimap-stream ssl)
        (nnimap-server-port 993)
        (nnimap-authinfo-file "~/.authinfo")))

;(setq gnus-posting-styles
;      '(;;("gmane.*" ("From" "Luis Oliveira <luismbo@gmail.com>"))
;        ;;("trolltech.*" ("From" "Luis Oliveira <luismbo@gmail.com>"))
;        ;;("gmail*" ("From" "Luis Oliveira <luismbo@gmail.com>"))))

;(setq gnus-select-method '(nntp "news.sunsite.dk"))

;(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)%l %O\n")

;;(setq message-send-mail-function 'smtpmail-send-it
;      smtpmail-starttls-credentials '(("smtp.gmail.com" 465 nil nil))
;      smtpmail-auth-credentials '(("smtp.gmail.com" 465 "luismbo@gmail.com" nil))
;      smtpmail-default-smtp-server "smtp.gmail.com"
;      smtpmail-smtp-server "smtp.gmail.com"
;      smtpmail-smtp-service 465)
;;      smtpmail-local-domain "yourcompany.com")

(setq gnus-topic-display-empty-topics nil)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-treat-display-smileys nil)
(setq gnus-treat-display-face nil)

(add-hook 'message-mode-hook
          (lambda () 
            (auto-fill-mode 1)
            (setq fill-column 72)))
