;;; init-cc-mode --- Nothing
;;; Commentary:
;;  My-cc-mode

;;; Code:

(require 'cc-mode)
(require 'company)
(require 'company-c-headers)
(require 'flycheck-pkg-config)
;;; avoid default "gnu" style, use more popular one

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))
;;; All comes from CC-mode manual
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style.")
(c-add-style "PERSONAL" my-c-style)
;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  "Set my personal style for the current buffer."
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; Company-c-headers
(add-to-list 'company-backends 'company-c-headers)
;;To enable C++ header completion for standard libraries, you have to add its path, for example, like this:
;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/??/")

;;; Flycheck-pkg-config
;;Flycheck-pkg-config provides an interactive way for configuring flycheck to use C library headers.
;;It configures flycheck-clang-include-path, flycheck-gcc-include-path and flycheck-cppcheck-include-path interactively.
;;These three variables are defined in flycheck.el

(provide 'init-cc)
;;; init-cc ends here
