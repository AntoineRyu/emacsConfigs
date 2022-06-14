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

(defun helm-ag--do-ag-up-one-level ()
  "Overwrite default helm-ag--do-ag-up-one-level to not ask for parent dir, losing it's helm-input."
  (interactive)
  (if (let ((parent (file-name-directory (directory-file-name default-directory)))
            (initial-input helm-input))
        (helm-run-after-exit
         (lambda ()
           (let ((default-directory parent)
                 (helm-ag--default-directory parent))
             (setq helm-ag--last-default-directory default-directory)
             (helm-ag--do-ag-set-source default-directory)
             (helm :sources 'helm-source-do-ag :buffer "*helm-ag*"
                   :keymap helm-do-ag-map :input helm-input
                   :history 'helm-ag--helm-history)))))
    (message nil)))

(require 'helm-buffers)
(defclass custom_helm-source-file-buffers (helm-source-buffers)
((candidate-transformer :initform (lambda (buffers)
                                   (cl-loop for buf in buffers
                                            when (with-current-buffer
                                                       buf buffer-file-name)
                                            collect buf))))
)

(defclass custom_helm-source-nonfile-buffers (helm-source-buffers)
((candidate-transformer :initform (lambda (buffers)
                                   (cl-loop for buf in buffers
                                            unless (with-current-buffer
                                                       buf buffer-file-name)
                                            collect buf))))
)

(setq custom_helm-source-file-buffers-list
      (helm-make-source "File Buffers" 'custom_helm-source-file-buffers))
(setq custom_helm-source-nonfile-buffers-list
      (helm-make-source "Non-file Buffers" 'custom_helm-source-nonfile-buffers))

(setq helm-mini-default-sources '(custom_helm-source-file-buffers-list
                                  helm-source-recentf 
                                  custom_helm-source-nonfile-buffers-list
                                  helm-source-buffer-not-found))

(provide 'core-helm)
;;; core-helm.el ends here
