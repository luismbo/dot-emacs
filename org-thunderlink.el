(require 'org)

(setq org-thunderlink-thunderbird-path "c:/Program Files (x86)/Mozilla Thunderbird/thunderbird.exe")

(org-add-link-type "thunderlink" 'org-thunderlink-open)

(defun org-thunderlink-open (path)
  "Opens an email in Thunderbird with ThunderLink."
  (start-process "myname" nil org-thunderlink-thunderbird-path "-thunderlink" (concat "thunderlink:" path)))

(provide 'org-thunderlink)
