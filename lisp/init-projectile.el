;;; init-projectile --- Nothing.

;;; Commentary:

;;; Code:

(require 'projectile)
(add-hook 'after-init-hook 'projectile-mode)

;; The following code means you get a menu if you hit "C-c p" and wait
(require 'guide-key)
(with-eval-after-load 'guide-key
  (add-to-list 'guide-key/guide-key-sequence "C-c p"))

;; Shorter modeline
(with-eval-after-load 'projectile
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         ""
       (format " [%s]" (projectile-project-name))))))


(provide 'init-projectile)
;;; init-projectile ends here
