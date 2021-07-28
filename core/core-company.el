;;; core-company.el ---                              -*- lexical-binding: t; -*-

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (("C-M-/" . company-complete)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-tooltip-align-annotations t "aligns annotation to the right")
  (company-tooltip-limit 12 "bigger popup window")
  (company-idle-delay .5 "decrease delay before autocompletion popup shows")
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

(use-package company-prescient
  :hook (company . company-prescient-mode))

(provide 'core-company)
;;; core-company.el ends here
