;; (require 'helm-gtags)
;; (require 'rtags)

;; (defun use-rtags (&optional useFileManager)
;;   (and (rtags-executable-find "rc")
;;        (cond ((not (helm-gtags-find-tag-directory)) t)
;;              ((and (not (eq major-mode 'c++-mode))
;;                    (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
;;              (useFileManager (rtags-has-filemanager))
;;              (t (rtags-is-indexed)))))

;; (defun tags-find-symbol-at-point (&optional prefix)
;;   (interactive "P")
;;   (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
;;       (helm-gtags-find-tag)))
;; (defun tags-find-references-at-point (&optional prefix)
;;   (interactive "P")
;;   (if (and (not (rtags-find-all-references-at-point prefix)) rtags-last-request-not-indexed)
;;       (helm-gtags-find-rtag)))
;; (defun tags-find-symbol ()
;;   (interactive)
;;   (call-interactively (if (use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))
;; (defun tags-find-references ()
;;   (interactive)
;;   (call-interactively (if (use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))
;; (defun tags-find-file ()
;;   (interactive)
;;   (call-interactively (if (use-rtags t) 'rtags-find-file 'helm-gtags-find-file)))
;; (defun tags-imenu ()
;;   (interactive)
;;   (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))
;; (defun tags-pop-stack ()
;;   (interactive)
;;   (call-interactively (if (not (eq 'rtags-location-stack nil)) 'rtags-location-stack-back 'helm-gtags-pop-stack)))
;; (defun tags-clear-stack ()
;;   (interactive)
;;   (call-interactively (if (not (eq 'rtags-location-stack nil)) 'rtags-location-stack-reset 'helm-gtags-clear-stack)))
