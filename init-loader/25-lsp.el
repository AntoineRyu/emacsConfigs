
(require 'lsp-mode)

(require 'lsp-ui)

(require 'company-lsp)

(require 'lsp-python-ms)
(add-hook 'python-mode #'lsp) ; or lsp-deferred

;; for executable of language server, if it's not symlinked on your PATH
;;(setq lsp-python-ms-executable "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
