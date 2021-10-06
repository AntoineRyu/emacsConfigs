;;; core-helm.el ---                                 -*- lexical-binding: t; -*-

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-o" . helm-find-files)
   ("C-x f" . helm-recentf)
   ("s-r". helm-recentf)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)
   ("C-;" . helm-for-files)
   ("C-'" . helm-mini)
   ("C-." . helm-do-ag-project-root)
   ("C-]" . helm-resume)
   ("C-z" . helm-occur)
   :map helm-map
   ("TAB" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action))
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-prevent-escaping-from-minibuffer t)
  (helm-bookmark-show-location t)
  (helm-display-header-line nil)
  (helm-split-window-inside-p t)
  (helm-always-two-windows t)
  (helm-echo-input-in-header-line nil)
  (helm-autoresize-min-height 10)
  (helm-autoresize-max-height 25)
  (helm-buffer-max-length 40 "make helm buffer wider to display full file names")
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-show-completion-display-function #'helm-display-buffer-in-own-frame)
  :config
  (progn
    (helm-autoresize-mode t))
  :hook (after-init . helm-mode))

(use-package helm-swoop
  :disabled
  :defer t
  :custom
  (helm-swoop-split-with-multiple-windows t)
  (helm-swoop-split-direction 'split-window-vertically)
  (helm-swoop-speed-or-color t)
  :config
  (progn
    (setq helm-swoop-split-window-function 'helm-default-display-buffer))
  :bind (:map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)))

(use-package helm-ag
   :ensure helm-ag
   :init (setq helm-ag-insert-at-point 'symbol
               helm-ag-source-type 'file-line
	       helm-ag-base-command "ag --nocolor --nogroup -i"))

(use-package helm-projectile
  :after (helm projectile)
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile-ag
             helm-projectile-rg
             helm-projectile
             helm-projectile-switch-project
             core-helm-rg)
  :init (helm-projectile-on)
  :config
  (defun core-helm-rg ()
    "Run projectile-rg if in a project, otherwise rg current directory."
    (interactive)
    (if (projectile-project-p)
      (helm-projectile-rg)
      (helm-rg nil)))
  :bind
  (("C-<" . core-helm-rg)))

(use-package helm-rg
  :after (helm)
  :bind
  (:map helm-rg-map
    ("M-b" . nil)
    ("M-d" . nil)
    ("M-i" . helm-rg--bounce)
    ("M-o" . helm-rg--set-dir))
  :custom-face
  (helm-rg-preview-line-highlight ((t (:inherit highlight :distant-foreground "black")))))

(provide 'core-helm)
;;; core-helm.el ends here
