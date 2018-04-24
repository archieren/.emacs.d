;;; init-slime --- Nothing.
;;; Commentary:
;; SLIME:The Superior Lisp Interaction Mode for Emacs
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
(require 'slime-repl)
;; Lisp Buffers
(require 'slime-c-p-c)
(require 'init-lisp)
(defun init-slime-hippie-setup ()
  "Call set-up-slime-hippie-expand in hippie-expand-slime.
Hippie Mode setup function for slime Lisp buffers."
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
  (add-hook 'slime-mode-hook 'init-slime-hippie-setup))
;;; REPL

(defun init-slime-slime-repl-setup ()
  "Mode setup function for slime REPL."
  (init-lisp-lisp-setup)
  (set-up-slime-hippie-expand)
  (setq show-trailing-whitespace nil))

(with-eval-after-load 'slime-repl
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'init-slime-slime-repl-setup)
  )


(provide 'init-slime)
;;; init-slime ends here
