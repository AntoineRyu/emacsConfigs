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
   '(helm-ag docker docker-tramp docker-compose-mode dockerfile-mode lsp-treemacs lsp-ui lsp-python-ms flycheck-pos-tip flycheck po-mode qml-mode cmake-mode go-mode swift-mode yaml-mode markdown-mode parinfer-rust-mode graphql-mode web-mode typescript-mode json-mode prettier-js pytest sphinx-doc python-black company-prescient company helm-rg helm-projectile helm diff-hl gitignore-mode gitconfig-mode gitattributes-mode magit which-key rainbow-mode symbol-overlay volatile-highlights hl-todo highlight-numbers rainbow-delimiters highlight-parentheses dashboard doom-modeline doom-themes tree-sitter-langs bm quickrun editorconfig crux multiple-cursors ace-window winum expand-region easy-kill copy-as-format comment-dwim-2 undo-tree projectile swiper anzu hydra auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-rg-preview-line-highlight ((t (:inherit highlight :distant-foreground "black"))))
 '(hl-paren-face ((nil (:weight ultra-bold))) t))


;; window operation
(global-set-key (kbd "C-t")  'other-window); overwrite transpose-chars

;; navigation inside window
(global-set-key (kbd "M-,") 'avy-goto-char-2)

;; (global-set-key [f5]     'helm-imenu)

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

;;; core-custom.el ends here
