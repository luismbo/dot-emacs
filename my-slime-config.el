;;;; SLIME

(unless siscog-p
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (add-to-list 'load-path "~/src/lisp/slime/")
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy slime-asdf slime-indentation slime-banner))

  ;(slime-require 'swank-listener-hooks)

  ;;(setq inferior-lisp-program "clisp -K full")
  ;;(setq inferior-lisp-program "~/Software/sbcl.sh")
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  ;;(setq inferior-lisp-program "/Users/luis/Software/bin/openmcl")
  ;;(setq inferior-lisp-program "acl")
  (setq slime-net-coding-system 'utf-8-unix))

(setq lisp-indent-function 'common-lisp-indent-function)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;(setq slime-complete-symbol-function 'slime-simple-complete-symbol)

(setq common-lisp-hyperspec-root
      (if siscog-p
          "z:/doc/clhs.chm::/"
          ; "http://intranet/TechDocs/Lisp/HyperSpec/"
          "file:///Users/luis/Documents/References/HyperSpec-7-0/HyperSpec/"))

;; (setq lisp-simple-loop-indentation 1
;;       lisp-loop-keyword-indentation 6
;;       lisp-loop-forms-indentation 6)

(font-lock-add-keywords 'lisp-mode
                        '(("(\\(\\(def\\|with-\\)\\(\\s_\\|\\w\\)*\\)"
                           1 font-lock-keyword-face)))

;(defun clhs-lookup (symbol-name)
;  (interactive
;   (list (let ((symbol-at-point (slime-symbol-name-at-point)))
;           (if (and symbol-at-point
;                    (intern-soft (downcase symbol-at-point)
;                                 common-lisp-hyperspec-symbols))
;               symbol-at-point
;             (completing-read
;              "Look up symbol in Common Lisp HyperSpec: "
;              common-lisp-hyperspec-symbols nil
;              t symbol-at-point
;              'common-lisp-hyperspec-history)))))
;  (ignore-errors
;    (info (concat "(~/Software/clhs/ansicl) " symbol-name) "*clhs*")))

;;; Various lisps

(defmacro defslime-start (name lisp &optional coding-system args)
  `(defun ,name ()
     (interactive)
     (slime-start :program ,lisp ,@(when args (list :program-args args))
                  :coding-system (or ',coding-system 'utf-8-unix))))

(unless siscog-p
  (defslime-start allegro "/Applications/AllegroCLexpress.app/Contents/Resources/alisp")
  (defslime-start allegro64 "~/Software/acl82.64/alisp")
  (defslime-start clisp "/usr/local/bin/clisp" utf-8-unix '("-I"))
  (defslime-start cmucl "~/Software/bin/lisp" iso-latin-1-unix)
  (defslime-start ccl "~/Software/ccl/scripts/ccl" iso-latin-1-unix)
  (defslime-start ccl64 "~/Software/ccl/scripts/ccl64" iso-latin-1-unix)
  (defslime-start sbcl "/usr/local/bin/sbcl" utf-8-unix)
  (defslime-start ecl "~/Software/bin/ecl" iso-latin-1-unix)
  (defslime-start lw "~/Software/bin/lw")
  (defslime-start abcl "java" utf-8-unix
    '("-jar" "/Users/luis/Software/abcl-bin/abcl.jar")))

(defmacro defslime-connect (name host port)
  `(defun ,name ()
     (interactive)
     (slime-connect ,host ,port)))

(defslime-connect lw-local "localhost" 4000)
;; Lisps running on virtual PC
(defslime-connect lw-win "virtual" 4000)
(defslime-connect allegro-win "virtual" 4001)
(defslime-connect clisp-win "virtual" 4002)
(defslime-connect corman-win "virtual" 4003)

(setq slime-save-buffers nil)

;; Getting rid of the Fontifying *SLIME
;; Compilation*... (regexps......) message.
(setq font-lock-verbose nil)

;;;; Keybindings

(global-set-key (kbd "C-c s") 'slime-selector)
(global-set-key (kbd "C-c h") 'clhs-lookup)
(global-set-key (kbd "C-c r") 'slime-pop-find-definition-stack)

(setq slime-autodoc-use-multiline-p t)
