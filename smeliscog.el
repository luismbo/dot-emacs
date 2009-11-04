(defvar original-slime-eol-conversion-fixup #'slime-eol-conversion-fixup)

(defun slime-eol-conversion-fixup (n)
  (if (and (string-equal "allegro" (slime-lisp-implementation-name (slime-connection)))
           (>= (car (read-from-string (slime-lisp-implementation-version (slime-connection)))) 8.1))
      0
      (funcall original-slime-eol-conversion-fixup n)))

(defun sc-allegro (image &rest args)
  (setq slime-port (+ 4006 (random 100)))
  (with-temp-buffer
    (insert (format "%d" slime-port)))
  (let* ((w32-quote-process-args nil)
         (acl-process
          (apply 'start-process
                 "common-lisp"
                 (generate-new-buffer-name " common-lisp")
                 "c:/Program Files/acl81/allegro-ansi.exe"
                 "-I" (if (string-match " " image) (format "'%s'" image) image)
                 "-e" "'(progn " (concat "(setq lep::*connection* nil)"
                                         "(load \"z:/src/slime/swank-loader.lisp\" :verbose t)"
                                         "(funcall (intern :init :swank-loader))"
                                         (concat "(funcall (intern :start-server :swank) \""
                                                 (slime-swank-port-file)
                                                 "\" :dont-close t :coding-system \"iso-latin-1-unix\")"))
                 ")'"
                 args)))
    (slime-read-port-and-connect acl-process nil)))

(defun sc-data-manager-v8-1-0 ()
  (interactive)
  (ignore-errors (delete-file (expand-file-name "crews-siscog-before.lisp" (getenv "HOME"))))
  (sc-allegro "y:/siscog/crews-siscog-v8-1-0/bin/data-manager.dxl" "-design"))
