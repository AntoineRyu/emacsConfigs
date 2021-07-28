;;; core-python.el ---                               -*- lexical-binding: t; -*-

(defun core/python-hideshow-forward-sexp-function (arg)
  "Python specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  arg  ; Shut up, byte compiler.
  (python-nav-end-of-block))

(defun core/python-setup-hs-mode ()
  "Replace `hs-special-modes-alist' for `python-mode'."
  (let
    ((python-mode-hs-info
       '(python-mode
          "\\s-*\\_<\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\|with\\)\\_>" "" "#"
          core/python-hideshow-forward-sexp-function
          nil)))
    (setq hs-special-modes-alist (remove-if #'(lambda (x) (eq (car x) 'python-mode)) hs-special-modes-alist))
    (add-to-list 'hs-special-modes-alist python-mode-hs-info)
    (hs-grok-mode-type)))

(use-package python
  :defer t
  :defines gud-pdb-command-name pdb-path
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :hook
  (python-mode . core/python-setup-hs-mode))

;; Format using YAPF
;; Install:
;; pip install black
;; pip install black-macchiato
(use-package python-black
  :demand t
  :after python
  :custom
  (python-black-extra-args '("--line-length=120" "--skip-string-normalization"))
  :bind
  (:map python-mode-map
    ("C-c C-l" . python-black-partial-dwim)))

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode))

(use-package pytest
  :defer t)

(provide 'core-python)
;;; core-python.el ends here
