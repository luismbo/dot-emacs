;; -*- lexical-binding: t; -*-

;;; Personnal features

;; (set-face-background 'region "darkseagreen")

;; (put 'eval-expression 'disabled nil)

;; (put 'upcase-region 'disabled nil)

;; (put 'downcase-region 'disabled nil)

;; (put 'erase-buffer 'disabled nil)
 
;; (custom-set-variables
;;  '(pc-select-meta-moves-sexps t)
;;  '(pc-selection-mode t nil (pc-select)))
;; (put 'narrow-to-region 'disabled nil)
 
;; (custom-set-faces)

;; (setq ps-landscape-mode t)

;; (setq ps-number-of-columns 2)

;; Own Keyboard Combinations

;; (define-key global-map [f1]      'insert-tab)
(define-key global-map [f2]      'insert-comment-tabs)
;; (define-key global-map [f3]      'sc-indent-region)
;; (define-key global-map [f4]      'sc-insert-commas2)
;; (define-key global-map [f5]      'sc-find-definition)
;; (define-key global-map [f6]      'sc-find-string-in-file)
;; (define-key global-map [f7]      'upcase-region)
;; (define-key global-map [f8]      'downcase-region)
;; (define-key global-map [f9]      'insert-keywords)
;; (define-key global-map [f10]     'save-buffers-kill-emacs)

;; (define-key global-map [home]    'beginning-of-line)
;; (define-key global-map [C-home]  'beginning-of-buffer)
;; (define-key global-map [end]     'end-of-line)
;; (define-key global-map [C-end]   'end-of-buffer)

;; (define-key global-map [M-left]  'backward-sexp)
;; (define-key global-map [M-right] 'forward-sexp)
;; (define-key global-map "\M-\\"   'find-in-other-window)
;; (define-key global-map "\M->"    'search-mod-date-forward)
;; (define-key global-map "\M-<"    'search-mod-date-forward)



;;;--------------------------------------------------------------------------------

;; (show-paren-mode)


(defun insert-string-elem ()
  (interactive)
  (insert-doc-mark "elem"))

(defun insert-string-arg ()
  (interactive)
  (insert-doc-mark "arg"))

(defun insert-string-emph ()
  (interactive)
  (insert-doc-mark "emph"))

;; (define-key global-map [f5] 'insert-string-elem )
;; (define-key global-map [f6] 'insert-string-arg )
;; (define-key global-map [S-f5] 'insert-string-emph )


;; (defun ediff-file-with-original1 ()
;;   (interactive)
;;   (ediff-file-with-original))


;; (define-key global-map [f11]
;;   (lambda () (interactive) (ediff-file-with-original)))


