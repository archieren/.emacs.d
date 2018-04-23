;;; init-purescript --- Nothing.
;;; Commentary:
;;; Code:
(require 'purescript-mode)
(require 'psc-ide)
(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))

(provide 'init-purescript)
;;; init-purescript ends here
