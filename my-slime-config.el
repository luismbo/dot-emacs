;;;; SLIME

(add-to-list 'load-path "~/siscog/sc-emacs/slime/")

(unless siscog-p
  (add-to-list 'load-path "~/clbuild/source/slime/")

  (require 'slime)
  (slime-setup '(slime-fancy slime-asdf slime-indentation slime-banner
                                        ;slime-hyperdoc
                 slime-parse))

  ;;(setq inferior-lisp-program "clisp -K full")
  ;;(setq inferior-lisp-program "~/Software/sbcl.sh")
  (setq inferior-lisp-program "~/clbuild/clbuild lisp")
  ;;(setq inferior-lisp-program "/Users/luis/Software/bin/openmcl")
  ;;(setq inferior-lisp-program "acl")
  )

(setq lisp-indent-function 'common-lisp-indent-function)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)


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
  (setq browse-url-browser-function
        '(("\\.chm" . browse-url-chm)
          ("." . browse-url-default-windows-browser))))

(setq common-lisp-hyperspec-root
      (if siscog-p
          "z:/doc/clhs.chm::/"
          ; "http://intranet/TechDocs/Lisp/HyperSpec/"
          "file:///Users/luis/Documents/References/HyperSpec-7-0/HyperSpec/"))

;; (setq lisp-simple-loop-indentation 1
;;       lisp-loop-keyword-indentation 6
;;       lisp-loop-forms-indentation 6)

;(font-lock-add-keywords 'lisp-mode
;                        '(("(\\(\\(def\\|with-\\)\\(\\s_\\|\\w\\)*\\)"
;                           1 font-lock-keyword-face)))

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
  (defslime-start allegro "~/Software/bin/alisp")
  (defslime-start clisp "~/Software/bin/clisp" utf-8-unix '("-I" "-K" "full"))
  (defslime-start cmucl "~/Software/bin/lisp" iso-latin-1-unix)
  (defslime-start ccl "~/Software/bin/ccl" iso-latin-1-unix)
  (defslime-start sbcl "~/Software/sbcl.sh")
  (defslime-start ecl "~/Software/bin/ecl" iso-latin-1-unix)
  (defslime-start lw "~/Software/bin/lw")
  (defslime-start abcl "~/sources/abcl/abcl" iso-latin-1-unix))

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

;(setq slime-net-coding-system 'utf-8-unix)

;(define-key slime-mode-map (kbd "RET") 'newline-and-indent)
;(define-key slime-mode-map (kbd "<return>") 'newline-and-indent)

;;;; Keybindings

(global-set-key (kbd "C-c s") 'slime-selector)
(global-set-key (kbd "C-c h") 'clhs-lookup)
(global-set-key (kbd "C-c r") 'slime-pop-find-definition-stack)

(defun slime-toggle-trace-fdefinition (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let* ((spec (if using-context-p
                   (slime-extract-context)
                   (slime-symbol-at-point)))
         (spec (slime-trace-query spec)))
    (message "%s" (slime-eval `(swank:swank-toggle-trace ,spec)))))
