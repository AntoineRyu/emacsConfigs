;;; core-flycheck.el ---                             -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Fix error list to bottom of window
  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-reuse-window display-buffer-at-bottom)
       (window-height   . 0.33)))
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (global-flycheck-mode . flycheck-pos-tip-mode))

(provide 'core-flycheck)
;;; core-flycheck.el ends here
