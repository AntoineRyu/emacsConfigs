(setq save-place t)
(recentf-mode t)
(setq compilation-scroll-output t)
(setq kill-whole-line t)

(require 'server)
(unless (server-running-p)
  (server-start)
  )

(require 'uniquify)

(delete-selection-mode 1) ; overwrite when yanking

(global-auto-revert-mode 1) ; バッファ自動再読み込み

(require 'git-gutter-fringe+)
;(global-git-gutter+-mode t)
;(setq git-gutter+-diff-options '("--ignore-space-at-eol"))


; (require 'smartparens-config)
; (smartparens-global-mode t)

(global-linum-mode t)

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(require 'generic-x)

(global-anzu-mode t)

;; no alarm bells
(setq ring-bell-function 'ignore)

;; highlight
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0)


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

;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

(ido-mode 0)
(setq ido-enable-flex-matching t)
(setq x-select-enable-clipboard t)

(setq comint-buffer-maximum-size 10000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun open-itl ()
  (interactive)
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlplanner.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlexecutor.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/itlserver.py")
  (find-file "~/mujin/checkoutroot/planningcommon/python/mujinplanningcommon/planning/itlplanning3/realtimeitlplanning3.py"))

(defun open-itlprocess ()
  (interactive)
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlapi.py")
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlslave.py")
  (find-file "~/mujin/checkoutroot/itlprocess/python/mujinitlprocess/itlmath.py"))

;; Can also define this function in bashrc: gdbmacs(){emacs -Q -l ~/rioux_emacs/init.el --eval "(gdbmacs \"$1\")"}
;; (gdb-many-windows)
(defun gdbmacs (args)
  (interactive)
  (toggle-frame-maximized)
  (gdb (concat "gdb -i=mi " args))
  (gdb-many-windows))
