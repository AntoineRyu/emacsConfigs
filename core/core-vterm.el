(use-package multi-vterm
  :after vterm
  :commands (multi-vterm))

(defun core/vterm-split-right ()
  "Split windwow and create a new term horizontally."
  (interactive)
  (let* ((ignore-window-parameters t))
    (select-window (split-window-horizontally))
    (multi-vterm)))

(defun core/vterm-split-below ()
  "Split windwow and create a new term horizontally."
  (interactive)
  (let* ((ignore-window-parameters t))
    (select-window (split-window-vertically))
    (multi-vterm)))

(use-package vterm
  :hook (vterm-mode . (lambda()
                        (setq-local global-hl-line-mode nil)))
  :bind (:map vterm-mode-map
          ("C-x 3" . core/vterm-split-right)
          ("C-x 2" . core/vterm-split-below))
  :custom (vterm-buffer-name-string "vterm %s"))

(use-package vterm-toggle
  :bind
  (("<f3>" . vterm-toggle-cd)
   (:map vterm-mode-map
         ("<f3>" . vterm-toggle-cd))))

(provide 'core-vterm)
