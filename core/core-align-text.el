;;; core-align-text.el ---                           -*- lexical-binding: t; -*-

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun core/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun core/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro core|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "core/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (core/align-repeat start end ,regexp ,justify-right after)))))

(core|create-align-repeat-x "comma" "," nil t)
(core|create-align-repeat-x "semicolon" ";" nil t)
(core|create-align-repeat-x "colon" ":" nil t)
(core|create-align-repeat-x "equal" "=")
(core|create-align-repeat-x "math-oper" "[+\\-*/]")
(core|create-align-repeat-x "ampersand" "&")
(core|create-align-repeat-x "bar" "|")
(core|create-align-repeat-x "left-paren" "(")
(core|create-align-repeat-x "right-paren" ")" t)
(core|create-align-repeat-x "backslash" "\\\\")
(core|create-align-repeat-x "period" "\\\.")

;; END align functions

(with-eval-after-load 'hydra
  (defhydra hydra-align-text (:hint nil)
    "
Align text
^^^^^^^^-------------------------------------------------
[_x_] regex         [_,_] comma       [_=_] equal
[_;_] semicolon     [_._] period      [_o_] math-oper
[_:_] colon         [_&_] ampersand
"
    ("x" core/align-repeat)
    ("," core/align-repeat-comma)
    (";" core/align-repeat-semicolon)
    (":" core/align-repeat-colon)
    ("=" core/align-repeat-equal)
    ("o" core/align-repeat-math-oper)
    ("&" core/align-repeat-ampersand)
    ("|" core/align-repeat-bar)
    ("(" core/align-repeat-left-paren)
    (")" core/align-repeat-right-paren)
    ("\\" core/align-repeat-backslash)
    ("\." core/align-repeat-period)))

(global-set-key [f4]     'align-current)

(provide 'core-align-text)
;;; core-align-text.el ends here
