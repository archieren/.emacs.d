;;; init-ledger --- Nothing.
;;; Commentary:
;; 一个会计模式.
;;; Code:
(require 'ledger-mode)
(require 'flycheck-ledger)

(with-eval-after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line))

(setq ledger-highlight-xact-under-point nil
      ;;ledger-use-iso-dates nil
      ;; Is it a free variable?
      )


(add-hook 'ledger-mode-hook 'goto-address-prog-mode)
(provide 'init-ledger)
;;; init-ledger ends here
