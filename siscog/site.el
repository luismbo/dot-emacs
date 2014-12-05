(require 'tramp)
(setq grep-find-use-xargs 'gnu)

(defun lbo:set-fixed-font ()
  (interactive)
  (set-frame-font "Fixedsys-10" t))

(defun lbo:set-default-font ()
  (interactive)
  (set-frame-font *default-font* t))

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

(defun lbo:browse-url-internet-explorer (url &rest ignore)
  (interactive "sURL: ")
  (shell-command (concat "explorer " (shell-quote-argument url))))

(setq browse-url-browser-function
      '(("\\.chm" . browse-url-chm)
        ("pms.siscog.com" . lbo:browse-url-internet-explorer)
        ("." . browse-url-default-windows-browser)))

(defun explorer ()
  (interactive)
  (w32-shell-execute "open" "explorer"
                     (concat "/e,/select," (convert-standard-filename buffer-file-name))))

(global-set-key [f12] 'explorer)
