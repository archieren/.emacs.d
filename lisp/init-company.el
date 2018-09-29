;;; init-company --- Nothing
;;; Commentary:
;;  和自动补全相关的有company,auto-complete和hippie.
;;  About Company
;;  Company is a text completion framework for Emacs.
;;  The name stands for "complete anything".
;;  It uses pluggable back-ends and front-ends to retrieve and display completion candidates.
;;  It comes with several back-ends such as
;;  Elisp,Clang,Semantic,Eclim,Ropemacs,Ispell,CMake,BBDB,Yasnippet,dabbrev,etags,gtags,files,keywords
;;  and a few others.
;;  The CAPF back-end provides a bridge to the standard COMPLETION-AT-POINT-FUNCTIONS facility,
;;  and thus works with any major mode that defines a proper completion function.

;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(require 'company)
;; 全局启用company-mode.
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (diminish 'company-mode "")
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t))

(require 'company-quickhelp)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(defun sanityinc/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (make-local-variable 'company-backends)
  (push backend company-backends))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(defvar init-company-page-break-lines-on-p nil)
(make-variable-buffer-local 'init-company-page-break-lines-on-p)
(with-eval-after-load 'company
  (with-eval-after-load 'page-break-lines
    (defun init-company-page-break-lines-disable (&rest ignore)
      (when (setq init-company-page-break-lines-on-p
                  (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun init-company-page-break-lines-maybe-reenable (&rest ignore)
      (when init-company-page-break-lines-on-p
        (page-break-lines-mode 1)))
    (add-hook 'company-completion-started-hook
              'init-company-page-break-lines-disable)
    (add-hook 'company-completion-finished-hook
              'init-company-page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook
              'init-company-page-break-lines-maybe-reenable)))

(provide 'init-company)
;;; init-company ends here
