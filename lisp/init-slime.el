;;; init-slime --- Nothing.
;;; Commentary:
;;; Code:

;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there. See
;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
;; 这是什么bug？
(mapc #'delete-file
      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

;;; Lisp buffers

(require 'slime)
(require 'slime-company)
(require 'paredit)
(require 'hippie-expand-slime)
;;(require 'slime-repl)
(require 'slime-c-p-c)
(defun sanityinc/slime-setup ()
  "Mode setup function for slime Lisp buffers."
  (set-up-slime-hippie-expand))

(with-eval-after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((extras (when (require 'slime-company nil t)
                  '(slime-company))))
    (slime-setup (append '(slime-repl slime-fuzzy) extras)))
  (setq slime-complete-symbol*-fancy t)
  ;; The custom variable "slime-complete-symbol-function" was obsoleted.
  ;; Use slime-completion-at-point-functions instead.
  (setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook 'sanityinc/slime-setup))



(provide 'init-slime)
;;; init-slime ends here
