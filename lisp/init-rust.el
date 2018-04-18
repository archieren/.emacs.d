;;; init-rust --- Nothing.
;;; Commentary:
;;; Code:
(require-package 'rust-mode)
(require-package 'racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(maybe-require-package 'company)
(add-hook 'racer-mode-hook #'company-mode)
(maybe-require-package 'flycheck-rust)
(after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust ends here
