;;; init-haskell --- Support the haskell language -*- lexical-binding: t -*-

;;; Commentary:
;;  Haskell-Mode, Dante Or Intero

;;; Code:

(require 'init-utils)
;; (require 'haskell-mode)
;; (require 'haskell-cabal)
;; (require 'haskell-compile)
;; 上面三行可能没必要,下面一句把所有的都加载了
(require 'haskell)
(require 'hindent)
(require 'rainbow-delimiters)
(require 'flycheck)
(require 'diminish)

(define-minor-mode stack-exec-path-mode
  "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
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


;;; About Haskell-Cabal-Mode
;;  Edit the .cabal File
(with-eval-after-load 'haskell  ;; haskell-cabal
  (add-hook 'haskell-cabal-mode-hook 'stack-exec-path-mode)
  (add-hook 'haskell-cabal-mode-hook 'subword-mode)
  (add-hook 'haskell-cabal-mode-hook (lambda () (setq mode-name "")))
  (define-key haskell-cabal-mode-map (kbd "C-c C-p") 'haskell-compile)
  (custom-set-variables '(haskell-compile-ignore-cabal t))
  )

;;; Hakell-Mode
(with-eval-after-load 'haskell  ;; haskell-mode
  (add-auto-mode 'haskell-mode "\\.ghci\\'")
  (add-hook 'haskell-mode-hook 'stack-exec-path-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook (lambda () (setq mode-name "")))
  (add-hook 'haskell-mode-hook 'eldoc-mode)
  (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode) ;;; C-M-a C-M-e C-M-h
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'flycheck-mode)

  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o")   'open-line)
  (define-key haskell-mode-map (kbd "C-c C-p") 'haskell-compile)

  (push 'haskell-mode page-break-lines-modes) ;; page-break-lines

  ;; hindent-mode
  ;; 需要系统按装hindent
  (when (require 'nadvice)
    (defun  init-haskell-hindent--before-save-wrapper (oldfun &rest args)
      (with-demoted-errors "Error invoking hindent: %s"
        (let ((debug-on-error nil))
          (apply oldfun args))))
    (advice-add 'hindent--before-save :around 'init-haskell-hindent--before-save-wrapper))
  (diminish 'hindent-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)

  ;; Indentation.
  ;; Haskell Mode ships with two indentation modes:
  ;;      -- haskell-indention-mode
  ;;      -- haskell-indent-mode  --Deprecated!!
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  ;; 其实禁用了 haskell-mode 定义的几个checker.
  ;; haskell-hlint is shipped with  flycheck!
  ;; But hlint should be installed in the os system!
  ;; (add-hook 'haskell-mode-hook (lambda () (flycheck-select-checker 'haskell-hlint)))


  ;; Using external formatters. Stylish-haskell should be intsalled!
  (custom-set-variables '(haskell-stylish-on-save t))
  (custom-set-variables '(haskell-tags-on-save t))
  (custom-set-variables '(haskell-compile-ignore-cabal t))
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
  )



;;; intero
;;; haskell-mode中,交互模式只有两种,inferior-haskell-mode和interactive-haskell-mode,
;;; But:
;;;    Inferior-haskell-mode was deprecated!
;;;    Interactive-haskell-mode: 和Intero处于一个级别,Intero直接将他屏蔽了,Intero的REPL直接采用的是"stack ghci".
;; (require 'intero)
;; (with-eval-after-load 'haskell
;;   (intero-global-mode) ;; 或者 (add-hook 'haskell-mode-hook 'intero-mode)
;;   (diminish 'intero-mode "")
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart)
;;   (define-key intero-mode-map (kbd "M-?") nil)
;;   (flycheck-add-next-checker 'intero  '(warning . haskell-hlint))
;;   (setq flycheck-check-syntax-automatically '(save new-line))
;;   )
;; ;;(setq intero-debug t)

;;; Intero-Mode-Map
;; (define-key intero-mode-map (kbd "C-c C-t") 'intero-type-at)
;; (define-key intero-mode-map (kbd "M-?") 'intero-uses-at)
;; (define-key intero-mode-map (kbd "C-c C-i") 'intero-info)
;; (define-key intero-mode-map (kbd "M-.") 'intero-goto-definition)
;; (define-key intero-mode-map (kbd "C-c C-l") 'intero-repl-load)
;; (define-key intero-mode-map (kbd "C-c C-c") 'intero-repl-eval-region)
;; (define-key intero-mode-map (kbd "C-c C-z") 'intero-repl)
;; (define-key intero-mode-map (kbd "C-c C-r") 'intero-apply-suggestions)
;; (define-key intero-mode-map (kbd "C-c C-e") 'intero-expand-splice-at-point)


;;; Purcell自己都改用intero的变种dante,那我也试试!
;;; 感觉不是很爽,倒是搞明白一些事情!
;;; dante可以不用！还不如直接用interacative-haskell-mode
;;; 注意，好像还有个haskell-interactive-mode,这个应当不要去用！
;;; interactive-haskell-mode 的process-type在auto情况下,选择的次序是cabal沙箱、stack ghci 、cabal等
(with-eval-after-load 'haskell
  ;; (require 'dante)
  ;; (diminish 'dante-mode "")
  ;; (add-hook 'haskell-mode-hook 'dante-mode)
  ;; (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
  (require 'haskell-interactive-mode)
  (diminish 'interactive-haskell-mode "")
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )


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
