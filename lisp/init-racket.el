;;; init-racket --- Nothing
;;; Commentary:
;;; Code:

(require 'racket-mode)

(setq racket-program "racket")
;;; racket-racket-program, racket-raco-program are obsolete variables!
;;; (setq racket-raco-program "raco")
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

(add-hook 'racket-mode-hook (lambda () (paredit-mode t)))

;;Has been defined at init-company
;;(setq tab-always-indent 'complete) ;; 使用tab自己主动补全

(provide 'init-racket)
;;; init-racket ends here
