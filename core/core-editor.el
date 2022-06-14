;;; core-editor.el ---                               -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'core-load-paths))

(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit hydra--call-interactively-remap-maybe hydra-show-hint hydra-set-transient-map))

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
;; (setq-default tab-width 8)            ;; but maintain correct appearance

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(defun my-c++-mode-hook ()
  (setq tab-width 4)
  (setq c-indent-level 4)
)
(defun my-c-mode-hook ()
  (setq tab-width 4)
  (setq c-indent-level 4)
)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; handle sooooooooo long line like one liner json file
(when (>= emacs-major-version 27)
  (global-so-long-mode t))

;; Do not create backup and auto save files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .#interlock files

(use-package tramp
  :defer t
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

;; Core package anzu better search and replace
(use-package anzu
  :diminish anzu-mode
  :bind (("C-%" . anzu-query-replace-at-cursor-thing)
         ([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Better isearch behavior
(use-package swiper
  :defer t
  :custom
  (swiper-action-recenter t)
  :bind (:map swiper-map
         ("M-%" . swiper-query-replace)
         :map isearch-mode-map
         ("M-i" . swiper-from-isearch)
         ([remap isearch-delete-char] . isearch-del-char)))

(use-package dired
  :ensure nil
  :config
  ;; dired - reuse current buffer
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  :bind
  (:map dired-mode-map
   ("RET" . dired-find-alternate-file)))

;; Core package Projectile
(use-package projectile
  :diminish
  :commands projectile-project-root
  :bind
  (:map projectile-mode-map
        ("C-," . projectile-find-file)
        ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
        projectile-cache-file (concat core-cache-directory "projectile.cache")
        projectile-known-projects-file (concat core-cache-directory
                                               "projectile-bookmarks.eld")))
(use-package recentf
  :hook
  (after-init . recentf-mode)
  :config
  (progn
    (setq recentf-save-file (concat core-cache-directory "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude (recentf-expand-file-name core-cache-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))))

;; Power package visualizing undo redo history
(use-package undo-tree
  :ensure t
  :hook (after-init . global-undo-tree-mode)
  :custom
  ;; autosave the undo-tree history
  (undo-tree-history-directory-alist `((".*" . ,core-cache-directory)))
  (undo-tree-auto-save-history t))
;;  (undo-tree-enable-undo-in-region nil "Known to be problematic so disable it https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16377"))


;; Pair parentheses, brace, quotes
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Core package smartparens
(use-package smartparens
  :disabled
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :hook
  (after-init . smartparens-global-mode)
  :init
  (progn
    ;; settings
    (setq sp-show-pair-delay 0.2
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil))
  :config
  (progn
    (require 'smartparens-config)
    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))

(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2))

(use-package copy-as-format
  :bind
  ("C-c w g" . copy-as-format-github))

;; Core package easy kill. easy to copy the buffer name/path
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  :commands (easy-kill-echo easy-kill-adjust-candidate)
  :config
  (progn
    (defun core/easy-kill-on-buffer-file-name (n)
      "extends buffer file name kill function. if `n' is 8, return the path in repo
if `n' is 9, return root dir + repo path."
      (unless (or buffer-file-name (projectile-project-root))
        (easy-kill-echo "No `buffer-file-name'")
        (return))
      (let* ((repo-buffer-name (substring buffer-file-name (length (projectile-project-root))))
             (repo-root-dir-name (car (last (split-string (or (projectile-project-root) "") "/" t))))
             (repo-dir-file-name (concat (if repo-root-dir-name
                                             (concat repo-root-dir-name
                                                     "/") repo-root-dir-name) repo-buffer-name))
             (text (pcase n
                      (`8 (concat repo-dir-file-name ":" (format-mode-line "%l")))
                      (`9 repo-dir-file-name))))
        (easy-kill-adjust-candidate 'buffer-file-name text)))
    (advice-add 'easy-kill-on-buffer-file-name :after #'core/easy-kill-on-buffer-file-name)))

;; Core package expand-region. Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Core package hideshow. Code folding tool
(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-`" . hs-toggle-hiding))

;; Core package winum. Easy navigation to different buffers
(use-package winum
  :hook (after-init . winum-mode)
  :bind
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8))

;; Ace window. specify which window to jump to
(use-package ace-window
  :bind
  ("s-w" . ace-window)
  ([remap other-window] . ace-window))

(use-package multiple-cursors
  :diminish
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-multiple-cursors (:hint nil)
      "
^ ^ M/C^ ^ ^ |^Cancel^
^-^-^-^-^-^-+-^-^-----------
^ ^ _k_ ^ ^ | _q_uit
_h_ ^+^ _l_ |
^ ^ _j_ ^ ^ |
"
      ("j" mc/mark-next-like-this)
      ("k" mc/mark-previous-like-this)
      ("h" mc/skip-to-previous-like-this)
      ("l" mc/skip-to-next-like-this)
      ("q" nil :color blue)))

  :bind
  ("C->"   . mc/mark-next-like-this)
  ("C-<"   . mc/mark-previous-like-this)
  ("C-M->" . mc/skip-to-next-like-this)
  ("C-M-<" . mc/skip-to-previous-like-this))

(use-package crux
  :bind
  (("C-c o" . crux-open-with)
   ("M-o" . crux-smart-open-line)
   ("s-o" . crux-smart-open-line-above)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c I" . crux-find-user-init-file)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-top-join-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package quickrun
  :bind (("<f9>" . quickrun)
         ("C-c x" . quickrun)))

(use-package bm
  :bind
  (("<f2>" . bm-next)
   ("<S-f2>" . bm-previous)
   ("<C-f2>" . bm-toggle))
  :custom
  (bm-buffer-persistence nil "Do not save bookmarks"))

;; goto-last-change
(use-package goto-chg
  :bind
  (("<M-up>"   . goto-last-change)
   ("<M-down>" . goto-last-change-reverse)))

(require 'core-auto-complete)
(require 'core-align-text)
(provide 'core-editor)
;;; core-editor.el ends here
