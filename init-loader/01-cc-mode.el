;; C,C++ファイル
(require 'indent-style nil t)

;; cpp-highlight-buffer
(setq cpp-known-face 'default)
(setq cpp-unknown-face 'default)
(setq cpp-face-type 'dark)
(setq cpp-known-writable 't)
(setq cpp-unknown-writable 't)
(setq cpp-edit-list
      '(("1" default
         (background-color . "dim gray")
         both nil)
        ("0"
         (background-color . "dim gray")
         nil both nil)))

;;(defun my-cc-mode-hook ()
;;  (if (fboundp 'c-subword-mode)
;;      (c-subword-mode t)
;;    (subword-mode t))
;;   (setq c-basic-offset 4)
;;   (setq c-tab-always-indent nil)
;;   (c-set-offset 'substatement-open 0)
;;   (cpp-highlight-buffer t)
;;(add-hook 'c-mode-common-hook 'my-cc-mode-hook)

;; c-mode

;; c++-mode
(setq c++-mode-hook
'(lambda ()
   (setq c++-class-member-indent 4)
   (setq c++-friend-offset 4)
   (c-set-offset 'innamespace 4)))

(setq auto-mode-alist
      (append '(("\\.dox\\'" . c++-mode)
                ("\\.inl\\'" . c++-mode))
              auto-mode-alist))

;; idlファイルのデフォルトインデントおかしい
(setq idl-mode-hook '(lambda () (c-set-style "stroustrup")))

(setq compile-command "jhbuild buildone -n mujin")
