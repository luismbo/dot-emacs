(defun lbo:set-fixed-font ()
  (interactive)
  (set-frame-font "Fixedsys-10" t))

(defun lbo:set-default-font ()
  (interactive)
  (set-frame-font *default-font* t))
