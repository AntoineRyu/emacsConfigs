;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Speed up initialization. Suggestions taken from Doom emacs configs.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

(defvar custom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist custom--file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(horizontal-scroll-bars. 0) default-frame-alist)

;; (when load-file-name
;;  (setq-default user-emacs-directory (file-name-directory load-file-name)))

;; Always load newest byte code
(setq load-prefer-newer t)

(setq initial-scratch-message "")

(provide 'early-init)
;;; early-init.el ends here
