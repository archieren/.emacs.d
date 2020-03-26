;;; init-python --- Nothing.
;;; Commentary:
;;; Code:
(require 'diminish)

(require 'elpy)
(require 'flycheck)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

;; run command `pip install rope jedi flake8 autopep8 yapf` in shell,
;; or just check https://github.com/jorgenschaefer/elpy
(elpy-enable)

(defun python-mode-hook-setup ()
  "Nothing."
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  ;;(setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  )

(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq flycheck-flake8-maximum-line-length 240)

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=240"))

;;; elpy 使用了 highlight-indentation 包
(require 'highlight-indentation)
(with-eval-after-load 'elpy
  (diminish 'elpy-mode)
  (diminish 'highlight-indentation-mode)
  )

;; Just for fun.
(add-hook 'python-mode-hook (lambda () (setq mode-name "")))

(provide 'init-python)
;;; init-python ends here
