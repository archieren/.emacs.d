;;; init-rust --- Nothing.
;;; Commentary:
;;; Code:
(require 'rust-mode)
(require 'racer)
(require 'company)
(require 'flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup) )


(provide 'init-rust)
;;; init-rust ends here
