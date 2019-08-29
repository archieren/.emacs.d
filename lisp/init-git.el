;;; init-git --- Nothing
;;; Commentary:
;;  All about VC goes here!
;;; Code:
(require 'diminish)

(require 'magit)
(require 'git-gutter)
(require 'init-utils)
(require 'fullframe)

;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.


(define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
(add-hook 'magit-popup-mode-hook 'init-gui-no-trailing-whitespace)
;; (fullframe magit-status magit-mode-quit-window)
(global-set-key [(meta f12)] 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;; {{ git-gutter
(global-git-gutter-mode t)
(diminish 'git-gutter-mode " ")

(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; }}

;; {{ Use git logo in vc-modeline instead of "Git:master"
;; See: https://www.reddit.com/r/emacs/comments/5fjri7/how_to_use_git_logo_in_modeline_instead_of/
(declare-function vc-mode "vc-hooks")
(defadvice vc-mode-line (after strip-backend () activate)
  "Nothing to say."
  (when (stringp vc-mode)
    (let ((gitlogo (replace-regexp-in-string "^ Git." "  - " vc-mode)))
      (setq vc-mode gitlogo))))
;; }}
(provide 'init-git)
;;; init-git ends here
