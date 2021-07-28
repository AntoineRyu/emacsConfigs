;;; core-load-paths.el ---                           -*- lexical-binding: t; -*-

(defun add-to-load-path (dirs)
  "Add a list of directories `DIRS' to 'load-path."
  (add-to-list 'load-path dirs))

(defvar core-directory
  (file-truename user-emacs-directory)
  "Root directory of Core Emacs.")

(defconst core-core-directory
  (expand-file-name "core" core-directory)
  "Core core directory.")

(defconst core-modules-directory
  (expand-file-name "modules" core-directory)
  "Core modules directory.")

(defconst core-cache-directory
  (expand-file-name (concat core-directory ".cache/"))
  "Core cache directory.")

(unless (file-exists-p core-cache-directory)
    (make-directory core-cache-directory))

;; Custom file
(setq custom-file (expand-file-name "custom.el" core-directory))
(unless (file-exists-p custom-file)
  (copy-file (expand-file-name "core-custom-template.el" core-core-directory) custom-file))
(load custom-file)

;; load paths
(mapc 'add-to-load-path
      `(
        ,core-core-directory
        ,core-modules-directory))

(provide 'core-load-paths)
;;; core-load-paths.el ends here
