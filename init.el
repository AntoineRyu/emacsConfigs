;; (emacs-init-time)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))

(require 'core-packages)
(require 'core-editor)
(require 'core-ui)
(require 'core-git)
;;(require 'core-helm)
(require 'core-vertico)
(require 'core-company)
(require 'core-python)
(require 'core-rust)
(require 'core-flycheck)
(require 'core-lsp)
(require 'core-hydra)
(require 'core-docker)

(provide 'init)
