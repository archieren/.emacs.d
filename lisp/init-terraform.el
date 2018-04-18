;;; init-terraform --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'terraform-mode)
(require-package 'company-terraform)
(after-load 'terraform-mode
  (company-terraform-init))
(provide 'init-terraform)
;;; init-terraform ends here
