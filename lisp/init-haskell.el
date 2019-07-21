;;; init-haskell --- Support the haskell language -*- lexical-binding: t -*-

;;; Commentary:
;; Ghc-mode is a submode of haskell-mode.

;;; Code:

(require 'init-utils)
(require 'haskell-mode)
(require 'hindent)
(require 'rainbow-delimiters)
(require 'flycheck)
(require 'diminish)

(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-cabal-mode 'subword-mode)

(custom-set-variables '(haskell-tags-on-save t))
(add-auto-mode 'haskell-mode "\\.ghci\\'")
(add-hook 'haskell-mode-hook (lambda () (setq mode-name "")))
(add-hook 'haskell-mode-hook 'eldoc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode) ;;; C-M-a C-M-e C-M-h
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)

(define-key haskell-mode-map (kbd "C-c h") 'hoogle)
(define-key haskell-mode-map (kbd "C-o")   'open-line)

(push 'haskell-mode page-break-lines-modes) ;; page-break-lines

;; hindent-mode 不是 haskell-indent-mode
;; 需要系统按装hindent
(when (require 'nadvice)
  (defun  init-haskell-hindent--before-save-wrapper (oldfun &rest args)
    (with-demoted-errors "Error invoking hindent: %s"
      (let ((debug-on-error nil))
        (apply oldfun args))))
  (advice-add 'hindent--before-save :around 'init-haskell-hindent--before-save-wrapper))
(diminish 'hindent-mode)


;;; 其实禁用了 haskell-mode 定义的几个checker.
;;; 下面有关intero checker的情况也是如此.
;;; (add-hook 'haskell-mode-hook (lambda () (flycheck-select-checker 'haskell-hlint)))

;;;
;;; Indentation. But turn-on-haskell-indentation was made obsolete since 2015.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Using external formatters. Stylish-haskell should be intsalled!
;;  (custom-set-variables '(haskell-stylish-on-save t))

;; Source code helpers
;; 奇怪，unicode-input-method对company-ghc有影响。
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; speedbar 华而不实，暂不用。
;;(require 'speedbar)
;;(speedbar-add-supported-extension ".hs")

(define-minor-mode stack-exec-path-mode
  "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
  :global nil
  (if stack-exec-path-mode
      (when (and (executable-find "stack")
                 (locate-dominating-file default-directory "stack.yaml"))
        (let ((stack-path (append
                           (list (concat (string-trim-right (shell-command-to-string "stack path --local-install-root")) "/bin"))
                           (parse-colon-path (replace-regexp-in-string "[\r\n]+\\'" "" (shell-command-to-string "stack path --bin-path"))))))
          (setq-local exec-path (seq-uniq stack-path 'string-equal))
          (make-local-variable 'process-environment)
          (setenv "PATH" (string-join exec-path path-separator))))
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)))


(add-hook 'haskell-mode-hook 'stack-exec-path-mode)

;;; intero
;;; 采用 intero,则交互模式只有两种,inferior-haskell-mode和intero-mode,
;;; inferior-haskell-mode基于Comint-mode和eshell,简单好用.
;;; 而interactive-haskell-mode被intero屏蔽了
(require 'intero)
(intero-global-mode) ;; 或者 (add-hook 'haskell-mode-hook 'intero-mode)
(diminish 'intero-mode "")
(define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart)
(define-key intero-mode-map (kbd "M-?") nil)
(flycheck-add-next-checker 'intero  '(warning . haskell-hlint))
;;(setq intero-debug t)

;;; Purcell自己都改用intero的变种dante,那我也试试!
;;(require 'dante)
;;(diminish 'dante-mode)
;;(require 'haskell)
;;(diminish 'interactive-haskell-mode)
;;(add-hook 'haskell-mode-hook 'dante-mode)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;(flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
;;(setq flycheck-check-syntax-automatically '(save mode-enabled))


;;; hie
;;(require 'lsp-mode)
;;(require 'lsp-ui)
;;(require 'lsp-haskell)
;;
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
;;(diminish 'lsp-mode)
(provide 'init-haskell)
;;; init-haskell ends here
