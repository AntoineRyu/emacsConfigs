;;; core-rust.el ---                               -*- lexical-binding: t; -*-

(use-package toml-mode)

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :custom (rust-format-on-save t))

(use-package cargo-mode
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(provide 'core-rust)
;;; core-rust.el ends here
