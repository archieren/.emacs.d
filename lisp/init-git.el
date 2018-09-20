;;; init-git --- Nothing
;;; Commentary:
;;; Code:
(require 'diminish)

(require 'magit)
(require 'git-gutter)
(require 'init-utils)

;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.


(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (add-hook 'magit-popup-mode-hook 'init-whitespace-no-trailing-whitespace)
  (fullframe magit-status magit-mode-quit-window)
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  )


;; {{ git-gutter
(global-git-gutter-mode t)
(diminish 'git-gutter-mode)


(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; }}
(provide 'init-git)
;;; init-git ends here
