;;;; -*- coding: utf-8 -*-

(setq user-mail-address "lbo@siscog.pt")
(setq user-full-name "Luís Borges de Oliveira")

;; for fast mail searching. Use 'G G' over a group on the Group List.
(require 'nnir)

;; safer place to store passwords.
(add-to-list 'auth-sources "w:/.authinfo")

;; main SISCOG account.
(setq gnus-select-method
      '(nnimap "SISCOG"
        (nnimap-address "imaps.lisboa.siscog")
        (nnimap-list-pattern "INBOX" "imap-mail/*")
        (nnimap-expunge-on-close never) ; not sure what this does
        ;; this means we need to load everything using (kbd "A C")
        ;; before being able to save the attachment.
        (nnimap-fetch-partial-articles "text/")
        (nnir-search-engine imap)
        (nnimap-server-port 993)
        (nnimap-stream ssl)))

;; Always show some groups, whether they have new messages or not.
;;
;; My Emacs-regex-fu is not sufficient to set this variable to the
;; value I want so Instead I'm using "G c" to toggle visibility of a
;; bunch of groups. This info gets stored in ~/.newsrc.eld
;;
;;(setq gnus-permanently-visible-groups "INBOX")

;; save sent mails in the IMAP "Sent" folder.
(setq gnus-message-archive-method '(nnimap "SISCOG"))
(setq gnus-message-archive-group "imap-mail/Sent")
(setq gnus-gcc-mark-as-read t) ; mark them as read

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))

(add-to-list 'gnus-secondary-select-methods
             ;; the nntp backend doesn't seem to look at auth-sources
             '(nntp "news.sunsite.dk" (nntp-authinfo-file "w:/.authinfo")))

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnimap "Gmail"
;;                (nnimap-address "imap.gmail.com")
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)))

;; Make Gnus NOT ignore [Gmail] mailboxes
;; (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-posting-styles
      '(("gmane.*" ("From" "Luís Oliveira <luismbo@gmail.com>"))
        ("comp.*" ("From" "Luís Oliveira <luis.oliveira@deadspam.com>"))))

;; (setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
;; (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)%l %O\n")

;; for collecting folders/groups per account. (Not automatic, have to
;; create topics and move groups there.)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-topic-display-empty-topics nil)

;; hide fugly pics.
(setq gnus-treat-display-smileys nil)
(setq gnus-treat-display-face nil)

(add-hook 'message-mode-hook
          (lambda ()
            (set-input-method 'portuguese-prefix)
            (auto-fill-mode 1)
            (setq fill-column 72)))

;; for when I can't get Emacs to load libxml2.dll
;; also, it displays less colours, which is actually nice.
;; It is one of defined renderer types, or a rendering function.
(setq mm-text-html-renderer 'shr)
;; (setq mm-text-html-renderer 'gnus-w3m)
(setq mm-inline-large-images t)
(setq mm-inline-text-html-with-images t)
;; (setq mm-inline-text-html-with-w3m-keymap nil)

;;;; Keybindings

(define-key gnus-summary-mode-map (kbd "<delete>")
  'lbo:gnus-move-to-trash)

(defun lbo:gnus-move-to-trash ()
  (interactive) (gnus-summary-move-article nil "imap-mail/Trash" nil 'move))

(define-key gnus-summary-mode-map (kbd "a")
  'lbo:gnus-archive)

(defun lbo:gnus-archive ()
  (interactive) (gnus-summary-move-article nil "imap-mail/Archives" nil 'move))

;;;; Sending Mail

(setq smtpmail-default-smtp-server "modifs.siscog")
(setq smtpmail-local-domain nil)
(setq message-send-mail-function 'smtpmail-send-it)
