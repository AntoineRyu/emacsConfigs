;(package-initialize)

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

(unless load-file-name
  (cd (getenv "HOME")))

;(require 'cl-lib)

(when load-file-name
  (setq-default user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer noninteractive)

(setq initial-scratch-message "")
