;;; init-haskell --- Nothing

;;; Commentary:
;; Ghc-mode is a submode of haskell-mode.

;;; Code:


;; Use intero for completion and flycheck
(require 'init-utils)
(require 'haskell-mode)
(require 'intero)
(with-eval-after-load 'haskell-mode
  (intero-global-mode)
  (add-hook 'haskell-mode-hook 'eldoc-mode))
(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart))
(with-eval-after-load 'intero
  (define-key intero-mode-map (kbd "M-?") nil)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'intero
                               '(warning . haskell-hlint))))


(add-auto-mode 'haskell-mode "\\.ghci\\'")
;; Mode-name
(add-hook 'haskell-mode-hook (lambda () (setq mode-name "")))
;; Indentation. But turn-on-haskell-indentation was made obsolete since 2015.
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;;; Using external formatters. Stylish-haskell should be intsalled!
;;(custom-set-variables '(haskell-stylish-on-save t))

;; Source code helpers
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode) ;;; C-M-a C-M-e C-M-h
;;(require 'speedbar)
;;(speedbar-add-supported-extension ".hs")

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'hindent-mode)

(with-eval-after-load 'hindent
  (when (require 'nadvice)
    (defun  init-haskell-hindent--before-save-wrapper (oldfun &rest args)
      (with-demoted-errors "Error invoking hindent: %s"
        (let ((debug-on-error nil))
          (apply oldfun args))))
    (advice-add 'hindent--before-save :around 'init-haskell-hindent--before-save-wrapper)))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))

(with-eval-after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))

(define-minor-mode stack-exec-path-mode
  "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
  :global nil
  (if stack-exec-path-mode
      (when (and (executable-find "stack")
                 (locate-dominating-file default-directory "stack.yaml"))
        (setq-local
         exec-path (parse-colon-path
                    (replace-regexp-in-string
                     "[\r\n]+\\'" ""
                     (shell-command-to-string "stack path --bin-path")))))
    (kill-local-variable 'exec-path)))

(add-hook 'haskell-mode-hook 'stack-exec-path-mode)


;;; Dhall is a programmable configuration language that is not Turing-complete
;;; You can think of Dhall as: JSON + functions + types + imports
;; 暂时不考虑dhall-mode
;;(add-hook 'dhall-mode-hook 'init-whitespace-no-trailing-whitespace)

;;; Ghc-mode is a submode of haskell mode!!! I want it!
;;; 但他覆盖了多少命令呢？

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(provide 'init-haskell)
;;; init-haskell ends here
