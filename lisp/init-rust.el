;;; init-rust --- Nothing.
;;; Commentary:
;;; Code:
(require 'rust-mode)
(require 'racer)
(require 'cargo)
(require 'company)
(require 'flycheck-rust)

(require 'diminish)
(with-eval-after-load 'rust-mode
  (diminish 'cargo-minor-mode)
  (diminish 'racer-mode)
  (add-hook 'rust-mode-hook (lambda () (setq mode-name "")))
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup) )


(provide 'init-rust)
;;; init-rust ends here
