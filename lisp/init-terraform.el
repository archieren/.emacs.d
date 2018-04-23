;;; init-terraform --- Nothing.
;;; Commentary:
;;; Code:
(require 'terraform-mode)
(require 'company-terraform)
(with-eval-after-load 'terraform-mode
  (company-terraform-init))
(provide 'init-terraform)
;;; init-terraform ends here
