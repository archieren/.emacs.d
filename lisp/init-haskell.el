;;; init-haskell --- Nothing

;;; Commentary:
;; Ghc-mode is a submode of haskell-mode.

;;; Code:
(require-package 'haskell-mode)
(require-package 'intero)
(require-package 'hindent)
(require-package 'ghc)
(require-package 'dhall-mode)

;; Use intero for completion and flycheck
(after-load 'haskell-mode
  (intero-global-mode)
  (add-hook 'haskell-mode-hook 'eldoc-mode))
(after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart))
(after-load 'intero
  (define-key intero-mode-map (kbd "M-?") nil)
  (after-load 'flycheck
    (flycheck-add-next-checker 'intero
                               '(warning . haskell-hlint))))


(add-auto-mode 'haskell-mode "\\.ghci\\'")

;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'hindent-mode)

(after-load 'hindent
  (when (require 'nadvice)
    (defun sanityinc/hindent--before-save-wrapper (oldfun &rest args)
      (with-demoted-errors "Error invoking hindent: %s"
        (let ((debug-on-error nil))
          (apply oldfun args))))
    (advice-add 'hindent--before-save :around 'sanityinc/hindent--before-save-wrapper)))

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))

(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))


;;
(add-hook 'dhall-mode-hook 'init-whitespace-no-trailing-whitespace)

;;; Ghc-mode is a submode of haskell mode!!! I want it!
;;;

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(provide 'init-haskell)
;;; init-haskell ends here
