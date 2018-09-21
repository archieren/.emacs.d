;;; init-projectile --- Nothing.

;;; Commentary:

;;; Code:

(require 'projectile)

(with-eval-after-load 'projectile
  (add-hook 'after-init-hook 'projectile-mode)
  (define-key projectile-mode-map (kbd "s-x C-p") 'projectile-command-map)
  ;; Shorter-mode-line
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         ""
       (format " [%s]" (projectile-project-name))))))


(provide 'init-projectile)
;;; init-projectile ends here
