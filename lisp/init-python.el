;;; init-python --- Nothing.
;;; Commentary:
;;; Code:

(require 'elpy)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

;; run command `pip install rope jedi flake8 autopep8 yapf` in shell,
;; or just check https://github.com/jorgenschaefer/elpy
(elpy-enable)

(defun python-mode-hook-setup ()
  "Nothing."
  (setq electric-indent-chars (delq ?: electric-indent-chars)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook 'python-mode-hook-setup)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;(elpy-use-ipython) ;;deprecated!

(provide 'init-python)
;;; init-python ends here
