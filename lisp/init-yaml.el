;;; init-yaml --- Nothing.
;;; Commentary:
;;; Code:
(require 'init-utils)

(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
(add-hook 'yaml-mode-hook 'goto-address-prog-mode)


(provide 'init-yaml)
;;; init-yaml ends here
