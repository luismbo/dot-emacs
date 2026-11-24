;; -*- lexical-binding: t; -*-

(setq fi:lisp-mode-hook
  (function
   (lambda ()
     (let ((map (current-local-map)))
       (define-key map "\C-c."	'find-tag)
       (define-key map "\C-c,"	'tags-loop-continue)
       (define-key map "\e."	'fi:lisp-find-definition)
       (define-key map "\e,"	'fi:lisp-find-next-definition)))))

(setq fi:find-tag-lock nil)
