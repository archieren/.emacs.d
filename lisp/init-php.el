;;; init-php --- Nothing.
;;; Commentary:
;;; Code:

(require 'php-mode)
(require 'smarty-mode)
(require 'company-php)
(require 'init-company)
(with-eval-after-load 'company
  (add-hook 'php-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-ac-php-backend))))

(provide 'init-php)
;;; init-php ends here
