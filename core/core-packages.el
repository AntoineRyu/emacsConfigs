;;; core-packages.el --- core package setup          -*- lexical-binding: t; -*-

;; Package configs
(require 'package)
(require 'core-load-paths)

(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized
  (package-initialize))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; Only enable for benchmarking
(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-last-update-day-filename
   (concat core-cache-directory "auto-package-last-update-day")))


(defun core-update ()
  "Update packages and configure repository."
  (if (executable-find "git")
    (progn
      (print "Pulling configuration update from git")
      (call-process-shell-command "cd ~/.emacs.d && git pull" nil nil t)))
  (print "Pulling configuration update from git")
  (auto-package-update-now))

(provide 'core-packages)
;;; core-packages.el ends here
