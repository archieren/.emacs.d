;;; init-yaml --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'yaml-mode)

(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
(add-hook 'yaml-mode-hook 'goto-address-prog-mode)


(provide 'init-yaml)
;;; init-yaml ends here