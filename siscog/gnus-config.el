(setq-default gnus-init-file "~/.emacs.d/siscog/gnus.el")

;;; HACK!

(require 'tls)

(defun open-tls-stream (name buffer host port)
  "Open a TLS connection for a port to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST PORT.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg PORT is an integer specifying a port to connect to."
  (let ((cmds tls-program)
	(use-temp-buffer (null buffer))
	process	cmd done)
    (if use-temp-buffer
	(setq buffer (generate-new-buffer " TLS"))
      ;; BUFFER is a string but does not exist as a buffer object.
      (unless (and (get-buffer buffer)
		   (buffer-name (get-buffer buffer)))
	(generate-new-buffer buffer)))
    (with-current-buffer buffer
      (message "Opening TLS connection to `%s'..." host)
      (while (and (not done) (setq cmd (pop cmds)))
	(let ((process-connection-type tls-process-connection-type)
	      (formatted-cmd
	       (format-spec
		cmd
		(format-spec-make
		 ?h host
		 ?p (if (integerp port)
			(int-to-string port)
		      port)))))
	  (message "Opening TLS connection with `%s'..." formatted-cmd)
	  (setq process (start-process
			 name buffer shell-file-name shell-command-switch
			 formatted-cmd))
	  (while (and process
		      (memq (process-status process) '(open run))
		      (progn
			(goto-char (point-min))
			(not (setq done (re-search-forward
					 tls-success nil t)))))
	    (unless (accept-process-output process 1)
	      (sit-for 1)))
	  (message "Opening TLS connection with `%s'...%s" formatted-cmd
		   (if done "done" "failed"))
	  (if (not done)
	      (delete-process process)
	    ;; advance point to after all informational messages that
	    ;; `openssl s_client' and `gnutls' print
	    (let ((start-of-data nil))
	      (while
		  (not (setq start-of-data
			     ;; the string matching `tls-end-of-info'
			     ;; might come in separate chunks from
			     ;; `accept-process-output', so start the
			     ;; search where `tls-success' ended
			     (save-excursion
			       (if (re-search-forward tls-end-of-info nil t)
				   (match-end 0)))))
		(accept-process-output process 1))
	      (if start-of-data
		  ;; move point to start of client data
		  (goto-char start-of-data)))
	    (setq done process))))
      (when (and done
		 (or
		  (and tls-checktrust
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-untrusted nil t))
		       (or
			(and (not (eq tls-checktrust 'ask))
			     (message "The certificate presented by `%s' is \
NOT trusted." host))
			(not (yes-or-no-p
			      (format "The certificate presented by `%s' is \
NOT trusted. Accept anyway? " host)))))
		  (and tls-hostmismatch
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-hostmismatch nil t))
		       (not (yes-or-no-p
			     (format "Host name in certificate doesn't \
match `%s'. Connect anyway? " host))))))
	(setq done nil)
	(delete-process process))
      ;; XXX JT was here hacking away. -- LBO
      (delete-region (point-min) (point)))
    (message "Opening TLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    (when use-temp-buffer
      (if done (set-process-buffer process nil))
      (kill-buffer buffer))
    done))
