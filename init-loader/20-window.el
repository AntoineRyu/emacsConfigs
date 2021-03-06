(setq inhibit-startup-screen t)


(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

(require 'generic-x)
;(windmove-default-keybindings)          ; Shift + 矢印キー でウィンドウ間移動

(global-font-lock-mode t)
(transient-mark-mode 1)			; 選択範囲に色を付ける

(require 'ansi-color)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (ansi-color-apply-on-region (point-min) (point-max))))

;; theme
(require 'monokai-theme)
(load-theme 'monokai t)
(enable-theme 'monokai)
;(setq frame-background-mode 'dark)

(defun other-window-or-split ()
  (interactive)
  (setq threshold 800)
  (setq width (window-body-width nil t))
  (setq height (window-body-height nil t))
  (setq max_width_height (max width height))
  (if (>= max_width_height threshold)
      (if (>= width height)
          (split-window-horizontally)
        (split-window-vertically))
    (other-window 1))
  )


(blink-cursor-mode 0)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#402222")
(set-face-foreground 'isearch "#272822")
(set-face-foreground 'highlight nil)

;(set-face-foreground 'mode-line "white")
;(set-face-background 'mode-line "tomato")
;(set-face-background 'modeline-inactive "dim gray")
;(set-face-background 'default "#000010")

;;package-listのformat
(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format [("Package" 35 package-menu--name-predicate)
                               ("Version" 15 package-menu--version-predicate)
                               ("Status"  10 package-menu--status-predicate)
                               ("Description" 10 package-menu--description-predicate)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header))

(setq confirm-kill-emacs 'y-or-n-p)

(set-frame-height (selected-frame) 70)
(set-frame-width (selected-frame) 115)
(setq default-fill-column 100)
