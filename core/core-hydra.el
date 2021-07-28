;;; core-hydra.el --- Hydra entries                  -*- lexical-binding: t; -*-

(defhydra core-hydra (:hint nil :color blue)
    "
Gate of Babylon
^^^^^^^^-------------------------------------------------
[_h_]ighlight
te[_x_]t
[_m_]ulti-cursor
"
  ("h" hydra-symbol-overlay/body)
  ("x" hydra-align-text/body)
  ("m" hydra-multiple-cursors/body))

(global-set-key (kbd "M-m") 'core-hydra/body)

(provide 'core-hydra)
;;; core-hydra.el ends here
