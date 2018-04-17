;;; init-flycheck.el --- Nothing
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
;; Flycheck使用的搜索目录.
(setq-default flycheck-emacs-lisp-load-path 'inherit)


(provide 'init-flycheck)
;;; init-flycheck ends here
