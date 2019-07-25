;;; init-lisp --- Nothing.
;;; Commentary:
;;; Code:

;;--------------------------------------------------
;; Enable desired features for emacs lisp
;;--------------------------------------------------
(require 'elisp-mode) ;; build-in
(require 'ielm)  ;; build-in : Inferior Emacs Lisp Mode
(require 'elisp-slime-nav)
;; Enable the elisp-slime-nav mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "")))
(diminish 'elisp-slime-nav-mode)
(setq-default initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs  you!\n\n"))

;; Make C-x C-e run 'eval-region if the region is active
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "PREFIX:the last sexp.
Eval region from begin-mark to end-mark if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)
(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region)

;;read-only
(defun init-lisp-maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))
(add-hook 'emacs-lisp-mode-hook 'init-lisp-maybe-set-bundled-elisp-readonly)


;; Use C-c C-z to toggle between elisp files and an ielm session
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
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'sanityinc/switch-to-ielm)
(define-key ielm-map (kbd "C-c C-z") 'sanityinc/repl-switch-back)


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

;;--------------------------------------------------
;; Enable desired features for all lisp modes
;;--------------------------------------------------
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
(diminish 'aggressive-indent-mode)
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

;;--------------------------------------------------------------------
;;;杂项
(require 'cl-lib-highlight)
(require 'macrostep)
(require 'highlight-quoted)
(require 'ert)

(cl-lib-highlight-initialize)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
(define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests)


;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)


;;---------------------------------------------------------------
;; SLIME:The Superior Lisp Interaction Mode for Emacs
;;---------------------------------------------------------------
;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there. See
;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
;; 这是什么bug？
(mapc #'delete-file
      (file-expand-wildcards (concat
                              user-emacs-directory
                              (format "elpa-%s.%s/slime-2*/contrib/*.elc"
                                      emacs-major-version emacs-minor-version))))

(require 'slime)
(slime-setup)
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner))
;;; Another way! Install the slime from quicklisp!
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(require 'slime-company)
(require 'paredit)
(require 'hippie-expand-slime)
(require 'slime-repl)
(require 'slime-c-p-c)
;;;Slime---------------------
(defun init-lisp-slime-hippie-setup ()
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
  (add-hook 'slime-mode-hook 'init-lisp-slime-hippie-setup))

;;;Slime-REPL--------------------
(defun init-lisp-slime-repl-setup ()
  "Mode setup function for slime REPL."
  (init-lisp-lisp-setup)
  (set-up-slime-hippie-expand)
  (setq show-trailing-whitespace nil))

(define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
(define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)
(add-hook 'slime-repl-mode-hook 'init-lisp-slime-repl-setup)

;;---------------------------------------------------------------
;; Common-lisp
;;---------------------------------------------------------------
;; See http://bc.tech.coop/blog/070927.html
(require 'init-utils)
(add-auto-mode 'lisp-mode "\\.cl\\'")
(add-hook 'lisp-mode-hook (lambda () (unless (featurep 'slime) (require 'slime) (normal-mode))))
(when (executable-find "sbcl")
  (add-to-list 'slime-lisp-implementations
               '(sbcl ("sbcl") :coding-system utf-8-unix)))
(when (executable-find "lisp")
  (add-to-list 'slime-lisp-implementations
               '(cmucl ("lisp") :coding-system iso-latin-1-unix)))
(when (executable-find "ccl")
  (add-to-list 'slime-lisp-implementations
               '(ccl ("ccl") :coding-system utf-8-unix)))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Search lispdoc.com for SYMBOL currently under the curser."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

(provide 'init-lisp)
;;; init-lisp ends here
