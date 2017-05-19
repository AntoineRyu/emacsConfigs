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

;;(global-set-key (kbd "<C-down>") 'scroll-down-in-place)
;;(global-set-key (kbd "<C-up>") 'scroll-up-in-place)
;(global-set-key (kbd "<C-down>") 'scroll-up-keep-cursor)
;(global-set-key (kbd "<C-up>") 'scroll-down-keep-cursor)
;(global-set-key (kbd "C-M-g") 'goto-line)

;(global-set-key "\M-n" 'scroll-down-in-place)
;(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-up-keep-cursor)
(global-set-key "\M-p" 'scroll-down-keep-cursor)

;; temps to get used to alt+ctrl+no arrow
(defvar minor-navig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "C-M-k") 'scroll-down-in-place)
    (define-key map (kbd "C-M-i") 'scroll-up-in-place)
    (define-key map (kbd "C-M-l") 'right-word)
    (define-key map (kbd "C-M-j") 'left-word)
    map)
  "minor-navig-mode leymap")

(define-minor-mode minor-navig-mode
  "A minor mode so that the key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(minor-navig-mode 1)

;; (global-set-key (kbd "M-j") 'next-line)
;; (global-set-key (kbd "M-k") 'previous-line)
;; (global-set-key (kbd "M-l") 'forward-char)
;; (global-set-key (kbd "M-h") 'backward-char)

;; (global-set-key (kbd "C-M-j") 'scroll-down-in-place)
;; (global-set-key (kbd "C-M-k") 'scroll-up-in-place)
;; (global-set-key (kbd "C-M-l") 'right-word)
;; (global-set-key (kbd "C-M-h") 'left-word)
