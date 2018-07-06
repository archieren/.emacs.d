;;; init-haskell --- Nothing

;;; Commentary:
;; Ghc-mode is a submode of haskell-mode.

;;; Code:

(require 'init-utils)
(require 'haskell-mode)
(require 'hindent)
(require 'rainbow-delimiters)
(require 'flycheck)
(require 'intero)
(require 'diminish)
(custom-set-variables '(haskell-tags-on-save t))
(add-auto-mode 'haskell-mode "\\.ghci\\'")
(add-hook 'haskell-mode-hook (lambda () (setq mode-name "")))
(add-hook 'haskell-mode-hook 'eldoc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode) ;;; C-M-a C-M-e C-M-h
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'hindent-mode)
;; hindent-mode 不是 haskell-indent-mode
;; 需要系统按装hindent
(when (require 'nadvice)
  (defun  init-haskell-hindent--before-save-wrapper (oldfun &rest args)
    (with-demoted-errors "Error invoking hindent: %s"
      (let ((debug-on-error nil))
        (apply oldfun args))))
  (advice-add 'hindent--before-save :around 'init-haskell-hindent--before-save-wrapper))
(diminish 'hindent-mode)

(require 'haskell-compile)
(defun init-haskell-haskell-compile ()
  "Nothing."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((prjdir (intero-project-root))
         (command (format "cd %s && stack build" prjdir)))
    (compilation-start command 'haskell-compilation-mode)))
(define-key haskell-mode-map (kbd "C-c s-c") 'init-haskell-haskell-compile)

;;; 其实禁用了 haskell-mode 定义的几个checker.
;;; 下面有关intero checker的情况也是如此.
;;; (add-hook 'haskell-mode-hook (lambda () (flycheck-select-checker 'haskell-hlint)))

;;;
;;; Indentation. But turn-on-haskell-indentation was made obsolete since 2015.
;;  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Using external formatters. Stylish-haskell should be intsalled!
;;  (custom-set-variables '(haskell-stylish-on-save t))

;; Source code helpers
;; 奇怪，unicode-input-method对company-ghc有影响。
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; speedbar 华而不实，暂不用。
;;(require 'speedbar)
;;(speedbar-add-supported-extension ".hs")




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
         exec-path
         (seq-uniq
          (append
           (list (concat
                  (string-trim-right
                   (shell-command-to-string "stack path --local-install-root"))
                  "/bin"))
           (parse-colon-path
            (replace-regexp-in-string
             "[\r\n]+\\'" ""
             (shell-command-to-string "stack path --bin-path"))))
          'string-equal)
         ))
    (kill-local-variable 'exec-path)))

(add-hook 'haskell-mode-hook 'stack-exec-path-mode)

;;; intero
;;; 采用 intero,则交互模式只有两种,inferior-haskell-mode和intero-mode,
;;; inferior-haskell-mode基于Comint-mode和eshell,简单好用.
;;; 而interactive-haskell-mode被intero屏蔽了
(intero-global-mode)
(diminish 'intero-mode "")
(define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart)
(define-key intero-mode-map (kbd "M-?") nil)
(flycheck-add-next-checker 'intero  '(warning . haskell-hlint))
;;(setq intero-debug t)

;;; hie
;;(require 'lsp-mode)
;;(require 'lsp-ui)
;;(require 'lsp-haskell)
;;
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
;;(add-hook 'haskell-mode-hook 'flycheck-mode)
;;(diminish 'lsp-mode)
(provide 'init-haskell)
;;; init-haskell ends here
