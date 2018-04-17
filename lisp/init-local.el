;;; init-local.el --- do some customizations
;;; Commentary:
;;; Code:
;; 杂项

(desktop-save-mode nil)

;; The init-local.el 将经常被修改，所以定义一个快捷键 s-f .

(defun init-local-open-init-local ()
  "The init-local.el is used for customization."
  (interactive)
  (find-file (expand-file-name "lisp/init-local.el" user-emacs-directory)))

(global-set-key (kbd "s-f") 'init-local-open-init-local)

;;; 设置 shell
(setq shell-file-name "/usr/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)




(provide 'init-local)
;;; init-local.el ends here
