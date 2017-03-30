; smoother scrolling
(setq scroll-step 1)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position nil)

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))

;; M-n and M-p
(global-unset-key "\M-p")
(global-unset-key "\M-n")

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))


(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key (kbd "<C-down>") 'scroll-down-in-place)
(global-set-key (kbd "<C-up>") 'scroll-up-in-place)
;(global-set-key (kbd "<C-down>") 'scroll-up-keep-cursor)
;(global-set-key (kbd "<C-up>") 'scroll-down-keep-cursor)
;(global-set-key (kbd "C-M-g") 'goto-line)

;(global-set-key "\M-n" 'scroll-down-in-place)
;(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-up-keep-cursor)
(global-set-key "\M-p" 'scroll-down-keep-cursor)
