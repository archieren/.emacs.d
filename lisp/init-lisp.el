;;; init-lisp --- Nothing.
;;; Commentary:
;;; 主要和Emacs Lisp的编辑有关.
;;; Code:
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "♥")))


;;(setq-default initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; Make C-x C-e run 'eval-region if the region is active

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "PREFIX:the last sexp.
Eval region from begin-mark to end-mark if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(require 'lisp-mode)
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region))

;;;
(require 'ipretty)
(add-hook 'after-init-hook 'ipretty-mode)


(defadvice pp-display-expression (after sanityinc/make-read-only (expression out-buffer-name) activate)
  "Enable VIEW-MODE in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(defun init-lisp-maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'init-lisp-maybe-set-bundled-elisp-readonly)

;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar sanityinc/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'sanityinc/repl-original-buffer)

(defvar sanityinc/repl-switch-function 'switch-to-buffer-other-window)

(defun sanityinc/switch-to-ielm ()
  "Nothing."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall sanityinc/repl-switch-function "*ielm*")
      (ielm))
    (setq sanityinc/repl-original-buffer orig-buffer)))

(defun sanityinc/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if sanityinc/repl-original-buffer
      (funcall sanityinc/repl-switch-function sanityinc/repl-original-buffer)
    (error "No original buffer")))
(require 'elisp-mode)
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'sanityinc/switch-to-ielm))
(require 'ielm)
(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'sanityinc/repl-switch-back))


;; Hippie-expand
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))

;; Automatic byte compilation
(require 'auto-compile)
(add-hook 'after-init-hook 'auto-compile-on-save-mode)
(add-hook 'after-init-hook 'auto-compile-on-load-mode)

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))

;; Enable desired features for all lisp modes

(require 'indent-guide)
(require 'paredit)
(defun init-lisp-enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun init-lisp-disable-indent-guide ()
  "Nothing."
  (when (bound-and-true-p indent-guide-mode)
    (indent-guide-mode -1)))

(defvar init-lisp-lispy-modes-hook
  '(enable-paredit-mode
    turn-on-eldoc-mode
    init-lisp-disable-indent-guide
    init-lisp-enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(require 'aggressive-indent)
(add-to-list 'init-lisp-lispy-modes-hook 'aggressive-indent-mode)

(defun  init-lisp-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'init-lisp-lispy-modes-hook))

(defun init-lisp-emacs-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst init-lisp-emacs-lispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst init-lisp-lispy-modes
  (append init-lisp-emacs-lispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")


;;;{{ REPL 来自原来的init-slime.el,我觉得移到这儿，好些.
;;;init-slime.el 在前，却在搞些和emacs lisp 有关的事，不合适。
;;;所以移到这儿.
(require 'slime-repl)
(require 'paredit)
(defun init-lisp-slime-repl-setup ()
  "Mode setup function for slime REPL."
  ( init-lisp-lisp-setup)
  (set-up-slime-hippie-expand)
  (setq show-trailing-whitespace nil))

(with-eval-after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'init-lisp-slime-repl-setup))
;;;}}
(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name init-lisp-lispy-modes))
  (add-hook hook ' init-lisp-lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name init-lisp-emacs-lispy-modes))
  (add-hook hook 'init-lisp-emacs-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (require 'eldoc-eval)
  (add-hook 'after-init-hook 'eldoc-in-minibuffer-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(require 'cl-lib-highlight)
(with-eval-after-load 'lisp-mode
  (cl-lib-highlight-initialize))

(require 'macrostep)
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(require 'highlight-quoted)
(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)


(require 'flycheck-package)
(with-eval-after-load 'flycheck
  (flycheck-package-setup))

;; ERT
(require 'ert)
(with-eval-after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))

(provide 'init-lisp)
;;; init-lisp ends here
