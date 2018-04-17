;;; init-local.el --- do some customizations
;;; Commentary:
;;; Code:
;; 杂项
(delete-selection-mode t) ;; 选择一片区域后，用键入字符代替
(global-linum-mode t) ;; 显示行号
(setq-default cursor-type 'bar ) ;;;; Set cursor-type
(setq make-backup-files nil)  ;;;; 编辑时,不需要备份文件！
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
;;; 设置ido-mode 但这好像是内置模式，可能没必要！
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere t)
(ido-mode 1)




;;; 设置 multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "s-;") 'mc/mark-next-like-this)
(global-set-key (kbd "s-'") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-;") 'mc/mark-all-like-this)

;;;Haskell 编程 : ghc-mode is a submode of haskell mode!!! I want it!

(require-package 'ghc)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))



(provide 'init-local)
;;; init-local.el ends here
