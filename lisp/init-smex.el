;;; init-smex --- Nothing
;;; Commentary:
;;; Code:
;; Use smex to handle M-x
;; Change path for ~/.smex-items
(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
;;(global-set-key [remap execute-extended-command] 'smex)
  ;;; 设置Smex
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-X")         'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'init-smex)
;;; init-smex ends here
