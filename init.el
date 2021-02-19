(load (concat user-emacs-directory "init-el-get.el"))

;; (emacs-init-time)
;; time emacs --eval '(kill-emacs)' ;; Use this line in terminal to evaluate load time quickly

;; init-loader
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init-loader"))

