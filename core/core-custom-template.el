;;; core-custom.el --- initialize custom values before starting up  -*- no-byte-compile: t -*-

;; Font
(when window-system
  (when (and (x-list-fonts "Operator Mono"))
    (let ((font "Operator Mono:weight=light:style=book:pixelsize=12"))
      ;; (set-frame-font font)
      (add-to-list 'default-frame-alist `(font . ,font))))

  (when (x-list-fonts "Noto Sans")
    (set-fontset-font t 'han "Noto Sans CJK SC Regular")
    (set-fontset-font t 'kana "Noto Sans CJK JP Regular")
    (set-fontset-font t 'cjk-misc "Noto Sans CJK Sc Regular")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-colors
   '("IndianRed3" "goldenrod3" "Springgreen3" "DeepSkyBlue3" "RoyalBlue3" "DarkViolet") nil nil "colors from inside to outside")
 '(highlight-parentheses-delay 0.2 nil nil "delay of parentheses highlight")
 '(highlight-parentheses-highlight-adjacent t nil nil "like show-paren-mode")
 '(package-selected-packages
   '(tabbar helm-ag docker docker-tramp docker-compose-mode dockerfile-mode lsp-treemacs lsp-ui lsp-python-ms flycheck-pos-tip flycheck po-mode qml-mode cmake-mode go-mode swift-mode yaml-mode markdown-mode parinfer-rust-mode graphql-mode web-mode typescript-mode json-mode prettier-js pytest sphinx-doc python-black company-prescient company helm-rg helm-projectile helm diff-hl gitignore-mode gitconfig-mode gitattributes-mode magit which-key rainbow-mode symbol-overlay volatile-highlights hl-todo highlight-numbers rainbow-delimiters highlight-parentheses dashboard doom-modeline doom-themes tree-sitter-langs bm quickrun editorconfig crux multiple-cursors move-text ace-window avy winum expand-region easy-kill copy-as-format comment-dwim-2 mwim undo-tree projectile evil swiper anzu hydra auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-rg-preview-line-highlight ((t (:inherit highlight :distant-foreground "black"))))
 '(hl-paren-face ((nil (:weight ultra-bold))) t))







;; unbind
(global-unset-key (kbd "C-z")) ; iconify-or-deiconify-frame

;; japanese input
(global-unset-key (kbd "C-\\")) ; toggle-input-method
;;(global-set-key (kbd "C-S-SPC") 'toggle-input-method)

;; anzu
(global-set-key (kbd "M-%")   'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-%")   'anzu-query-replace-at-cursor-thing)

;; helm
(global-set-key (kbd "C-?")  'helm-apropos)
(global-set-key (kbd "C-;")  'helm-for-files)
(global-set-key (kbd "C-'")  'helm-mini)
(global-set-key (kbd "C-.")  'helm-do-ag-project-root)
(global-set-key (kbd "C->")  'helm-ag-project-root)
(global-set-key (kbd "C-,")  'helm-multi-swoop-all)
(global-set-key (kbd "C-o")  'helm-find-files) ; overwrite open-line
;(global-set-key (kbd "C-h")  'delete-backward-char); over
(global-set-key (kbd "C-]")  'helm-resume)
(global-set-key (kbd "C-z")  'helm-occur) ;

;; window operation
(global-set-key (kbd "C-t")  'other-window); overwrite transpose-chars
(global-set-key (kbd "C-\+") 'other-window-or-split)
(global-set-key (kbd "C-\-") 'delete-other-windows)

;(global-set-key (kbd "C-SPC") 'company-complete)

;; navigation inside window
(global-set-key [M-up]    'goto-last-change)
(global-set-key [M-down]  'goto-last-change-reverse)
(global-set-key (kbd "M-,") 'avy-goto-char-2)

;; git-gutter
;;(global-set-key [f1]     'git-gutter+-next-hunk)
;;(global-set-key [S-f1]   'git-gutter+-previous-hunk)

;; magit
(global-set-key [f3]     'magit-diff)
(global-set-key [C-f3]   'magit-status)

;; align
(global-set-key [f4]     'align-current)
(global-set-key [C-f4]   'align-regexp)

(global-set-key [f5]     'helm-imenu)

;; tabbar
(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab)

(remove-hook 'post-command-hook   'magit-blame-goto-chunk-hook t)


;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

(defun open-itl ()
  (interactive)
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/realtimeitlplanning3.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlplanner.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlexecutor.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlserver.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlcache.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/pathtiming.py"))

(defun open-itlprocess ()
  (interactive)
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlapi.py")
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlslave.py")
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlmath.py"))


;; Scrolling related
; smoother scrolling
(setq scroll-step 1)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position nil)

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))

;; M-n and M-p
(global-unset-key "\M-p")
(global-unset-key "\M-n")
(global-set-key "\M-n" 'scroll-up-keep-cursor)
(global-set-key "\M-p" 'scroll-down-keep-cursor)
;;; core-custom.el ends here
