;;; -*- lexical-binding: t -*-

;; (sc-add-system-version "CREWS-NSB" '("v4-0-0-tk" :patch "scs-nsb" :product-name "scs"))

(sc-add-application-mode "roster" "su")

;;(sc-add-system-application "CREWS-STOG" "dispatcher")
;;(sc-add-system-db-user "CREWS-NSB" '("nsb008database" "testensb"))

;; (sc-define-system "CREWS-WM"
;;                   "CREWS"
;;                   "wm"
;;                   '()
;;                   '(("sm011database" "DB0511GORA3"))
;;                   '("data-manager" "scheduler" "roster"
;;                     "data-manager-st" "allocator" "scheduler-st" "dispatcher"
;;                     "application-controller-db" "dispatcher-server")
;;                   '((getenv "CREWS_WM_VERSION_DIR"))
;;                   (format "wm@%s" smtpmail-default-smtp-server))

;; (sc-add-system-version "CREWS-SISCOG" "viarail")

;; (sc-add-system-version "CREWS-SISCOG" "vwm")

;; (sc-add-system-version "SCS-LUL" '("poc" :product-name "scs" :patch "scs-lul"))
(sc-add-system-version "SCS-LUL" "poc")

(sc-add-system-version "SCS-SISCOG" "stw")


(setq *sc-local-repository-dir* (expand-file-name "~/siscog"))
(setq *sc-binary-repository-dir* (expand-file-name "~/siscog"))

; (setq *sc-central-repository-dir* "x:/siscog")

;;;-----------------------------------------------------------------------------
;;; Modifications management (ver sc-mod.el)
;;;-----------------------------------------------------------------------------
(setq *default-mod-date*   "42/42/42")
(setq *default-author*     "L. B. Oliveira")
(setq *default-org-dir*    *sc-central-repository-dir*)
(setq *default-src-dir*    *sc-local-repository-dir*)
(setq *default-mod-dir*    "z:/siscog/modifs")
(setq *modif-request-file* (format "%s/modif-requests" *default-mod-dir*))
(setq *actions-log-file*   (format "%s/.big-brother-file" *default-mod-dir*)) ;; NIL para nao informar o BIG-BROTHER...
(setq *modifs-mail-dir*    "y:/modif-requests")
(setq *modif-mail-from*    (rot13 "yob@fvfpbt.cg"))
(setq *modif-mail-name*    "modif-request-mail")
(setf *digest-mail-name*   "y:/modif-requests/modifs")

(setq *emacs-init-file*    (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))
(setq *emacs-program*      "emacs")

;;;-----------------------------------------------------------------------------
;;; Modifications mail (ver sc-mail.el)
;;;-----------------------------------------------------------------------------
(setq user-full-name (rot13 "Yhíf Obetrf qr Byvirven"))
(setq user-mail-address (rot13 "yob@fvfpbt.cg"))

;;;-----------------------------------------------------------------------------
;;; Lisp Documentation (ver cl-hyperspec/online-doc.el)
;;;-----------------------------------------------------------------------------
;; (set-lisp-doc-home "file://public/lisp")

;;;-----------------------------------------------------------------------------
;;; Grepfc (ver sc-grepfc.el)
;;;-----------------------------------------------------------------------------
;; (setq *grepfc-top-dir* "z:/siscog")
;; (setq *grepfc-names* '("crews" "cp" "ns" "dsb" "nsb" "wagn" "vr" "ml" "stog"))

;; (global-set-key [f2] 'grepfc-goto-line)
;; (global-set-key [C-f2] 'grepfc-goto-patch-log)
;; (global-set-key [M-f2] 'grepfc-goto-patch-data)
