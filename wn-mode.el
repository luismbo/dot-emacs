(defun wn-window-list ()
  "Return a list of ordered windows on the current frame"
  (window-list (selected-frame) 0 (minibuffer-window)))

(defun wn-select-nth (n)
  "Select window number N in current frame"
  (interactive "P")
  (let (window)
    (or (and (or (integerp n)
                 (setq n (read-number "Window number: ")))
             (setq window (nth n (wn-window-list)))
             (select-window window))
        (error "No such window."))))

(defun wn-selected-window-number ()
  "Return the number of the selected window"
  (length (memq (selected-window) (reverse (wn-window-list)))))

;(defun wn-selected-window-modeline ()
;  "Return the string for the current window modeline"
;  (propertize (format " #%s" (wn-selected-window-number))
;              'face 'wn-modeline-face))

(defun wn-selected-window-modeline ()
  (format " #%s" (wn-selected-window-number)))

(defvar wn-mode-map nil
  "Keymap.")

(unless wn-mode-map
  (setq wn-mode-map (make-sparse-keymap))
        
  (define-key wn-mode-map (kbd "M-1")
    (lambda nil (interactive)
      (wn-select-nth 0)))

  (define-key wn-mode-map (kbd "M-2")
    (lambda nil (interactive)
      (wn-select-nth 1)))

  (define-key wn-mode-map (kbd "M-3")
    (lambda nil (interactive)
      (wn-select-nth 2)))

  (define-key wn-mode-map (kbd "M-4")
    (lambda nil (interactive)
      (wn-select-nth 3)))

  (define-key wn-mode-map (kbd "M-5")
    (lambda nil (interactive)
      (wn-select-nth 4)))

  (define-key wn-mode-map (kbd "M-6")
    (lambda nil (interactive)
      (wn-select-nth 5)))

  (define-key wn-mode-map (kbd "M-7")
    (lambda nil (interactive)
      (wn-select-nth 6)))

  (define-key wn-mode-map (kbd "M-8")
    (lambda nil (interactive)
      (wn-select-nth 7)))

  (define-key wn-mode-map (kbd "M-9")
    (lambda nil (interactive)
      (wn-select-nth 8)))

  (define-key wn-mode-map (kbd "M-0")
    (lambda nil (interactive)
      (wn-select-nth 9))))

(define-minor-mode wn-mode
  "A minor mode that enables selection of windows according to
       numbers with the C-c number"
  :group 'windows
  :global t
  :init-value nil
  :lighter (:eval (wn-selected-window-modeline)))

;(defface wn-modeline-face 
;  '((t (:inherit mode-line)))
;  "wn-mode modeline face")