;;; core-docker.el --- Docker related package conf   -*- lexical-binding: t; -*-

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")


(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package docker
  :bind ("C-c d" . docker))

(provide 'core-docker)
;;; core-docker.el ends here
