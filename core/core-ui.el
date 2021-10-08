;;; core-ui.el --- Basic UI settings                 -*- lexical-binding: t; -*-

(eval-when-compile
 (require 'core-load-paths))

;; Minimal UI
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(unless (>= emacs-major-version 27)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  ;; relegate tooltips to echo area only
  (when (boundp 'tooltip-mode)
    (tooltip-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Turn on line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight the current line
(global-hl-line-mode t)

;; Initial size to be full screen height, half screen width and in the middle
;; (when (display-graphic-p)
;;   (let ((frame (selected-frame)))
;;     (set-frame-width frame (/ (display-pixel-width) 2) nil 'pixelwise)
;;     (set-frame-height frame (display-pixel-height) nil 'pixelwise)
;;     (set-frame-position frame (/ (display-pixel-width) 4) 0)
;;     ;; A little transparency
;;     (set-frame-parameter frame 'alpha '(95 95))
;;     (add-to-list 'default-frame-alist '(alpha 95 95))))

;; right border takes 1 pixel on right splited buffer, make fringe visible
(setq-default window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
(window-divider-mode)

;; Tree sitter for better syntax highlight
(when (functionp (quote module-load))
 (use-package tree-sitter-langs
   :after tree-sitter
   :config
   (tree-sitter-require 'tsx)
   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

 (use-package tree-sitter
   :hook ((prog-mode . global-tree-sitter-mode))
         (tree-sitter-after-on . tree-sitter-hl-mode)))

;; Theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package doom-modeline
  :config
  (doom-modeline-def-modeline 'doom-core
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name grip gnus debug lsp minor-modes input-method indent-info buffer-encoding major-mode vcs checker))
  (defun core-doom-modeline ()
    "Setup custom doom modeline."
    (doom-modeline-set-modeline 'doom-core 'default))
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . core-doom-modeline)))

(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (member "all-the-icons" (font-family-list))
          (all-the-icons-install-fonts t)))

;; Highlight nested parentheses
(use-package highlight-parentheses
  :custom
  (hl-paren-highlight-adjacent t "like show-paren-mode")
  (hl-paren-delay 0.2 "delay of parentheses highlight")
  (hl-paren-colors '("IndianRed3"
                     "goldenrod3"
                     "Springgreen3"
                     "DeepSkyBlue3"
                     "RoyalBlue3"
                     "DarkViolet") "colors from inside to outside")
  :custom-face
  (hl-paren-face ((nil (:weight ultra-bold))))
  :hook
  (prog-mode . highlight-parentheses-mode))

;; Color delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Highlight number variables
(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode)
  (asm-mode . highlight-numbers--turn-off))
 
;; Highlight TODO keywords FIXME HACK DONE FAIL OKAY)
(use-package hl-todo
  :hook
  ((prog-mode text-mode) . hl-todo-mode))

;; Highlight operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode))

(use-package symbol-overlay
  :diminish
  :defer t
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-symbol-overlay (:hint nil)
      "
^Highlight^            ^Navigation^           ^Manipulate^
^^^^^^^^-------------------------------------------------
[_h_]ighlight symbol    [_n_]ext              [_s_]earch
[_t_]oogle scope        [_p_]revious          [_r_]eplace
[_c_]lean               ^ ^                   [_R_]ename
"
      ("h" symbol-overlay-put)
      ("n" symbol-overlay-jump-next)
      ("p" symbol-overlay-jump-prev)
      ("t" symbol-overlay-toggle-in-scope)
      ("s" symbol-overlay-isearch-literally)
      ("r" symbol-overlay-query-replace)
      ("R" symbol-overlay-rename)
      ("c" symbol-overlay-remove-all)
      ("q" nil :color blue))))

;; Show color of color text #FFE4C4
(use-package rainbow-mode
  :hook
  (prog-mode text-mode))

(defun set-font-size ()
    "Set font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

;; Which Key
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

;; TABBAR
(use-package tabbar
  :ensure t)

(set-face-attribute
 'tabbar-default nil
 :background "gray60")
(set-face-attribute
 'tabbar-unselected nil
 :background "gray85"
 :foreground "gray30"
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background "#f2f2f6"
 :foreground "#FF1493"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator nil
 :height 0.7)

 (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
 (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward-tab)

(tabbar-mode 1)

(provide 'core-ui)
;;; core-ui.el ends here
