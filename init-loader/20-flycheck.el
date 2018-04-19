(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
