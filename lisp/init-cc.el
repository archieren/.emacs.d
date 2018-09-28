;;; init-cc-mode --- Nothing
;;; Commentary:
;;  My-cc-mode

;;; Code:

(require 'cc-mode)
(require 'company)
(require 'company-c-headers)
(require 'flycheck)
(require 'flycheck-pkg-config)
(require 'cmake-mode)
(require 'google-c-style)
;;; avoid default "gnu" style, use more popular one

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

;; Customizations for all modes in CC Mode.
(defun init-cc-c-mode-common-hook ()
  "Set my personal style for the current buffer."
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  )
;;(add-hook 'c-mode-common-hook 'init-cc-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; Company-c-headers
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-c-headers))
;;To enable C++ header completion for standard libraries, you have to add its path, for example, like this:
;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/??/")

;;; Flycheck-pkg-config
;;Flycheck-pkg-config provides an interactive way for configuring flycheck to use C library headers.
;;It configures flycheck-clang-include-path, flycheck-gcc-include-path and flycheck-cppcheck-include-path interactively.
;;These three variables are defined in flycheck.el
;;Usage: M-x flycheck-pkg-config

;;; Flycheck


;;; Cmake-mode
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;; Flycheck for cc-mode:
;;  Default support status of C,C++ standards
;;          C                           C++
;;  gcc     {c,gnu}{98,03,11,14,17/18}  {c,gnu}++{98,03,11,14,17}
;;  clang   c{98/03,11,14,17}           c++{98,03,11,14,17}
;; 开始配置cc-mode时,有些概念要清楚.
;; Style , Syntax , Complete , Navigation . 其实这也是所有编辑器要达到的基本功能.
;; cc-mode处理是语言文本的编辑风格.只须分清c,c++...就行了.
;; 而flycheck做的是语法检查,要选择检查器，并给检查器指定语言标准.
;; 因此,可以在cc-mode提供的buffer's local mode-hook里,设定buffer's local flycheck-checker,及相应的参数.
;; (当然,还可以在目录里，设置相应的变量)
;;
(defun init-cc-c-mode-hook ()
  "Set my c setups."
  (setq flycheck-checker               'c/c++-clang)
  ;;(setq flycheck-checker 'c/c++-gcc)
  (setq flycheck-clang-language-standard "c17")
  ;;(setq flycheck-gcc-language-standard "c17")
  ;; More flycheck-checker c/c++-gcc (c/c++-clang) 's parameters can go here!
  )

(defun init-cc-c++-mode-hook ()
  "Set my c setups."
  (setq flycheck-checker               'c/c++-clang)
  ;;(setq flycheck-checker 'c/c++-gcc)
  (setq flycheck-clang-language-standard "c++17")
  ;;(setq flycheck-gcc-language-standard 'c++17)
  ;; More flycheck-checker c/c++-gcc (c/c++-clang) 's parameters can go here!
  )
(add-hook 'c-mode-hook   'init-cc-c-mode-hook)
(add-hook 'c++-mode-hook 'init-cc-c++-mode-hook)

;;;--- Hooks of CC-mode
;; When you open a buffer, CC Mode first initializes it with the
;; currently active style (*note Styles::).  Then it calls
;; ‘c-mode-common-hook’, and finally it calls the language-specific hook.
;; Thus, any style settings done in these hooks will override those set by
;; ‘c-default-style’.
;;
;; -- Variable: c-initialization-hook
;; Hook run only once per Emacs session, when CC Mode is initialized.
;; This is a good place to change key bindings (or add new ones) in
;; any of the CC Mode key maps.  *Note Sample Init File::.
;;
;; -- Variable: c-mode-common-hook
;; Common hook across all languages.  It’s run immediately before the
;; language specific hook.
;;
;; -- Variable: c-mode-hook
;; -- Variable: c++-mode-hook
;; -- Variable: objc-mode-hook
;; -- Variable: java-mode-hook
;; -- Variable: idl-mode-hook
;; -- Variable: pike-mode-hook
;; -- Variable: awk-mode-hook
;; The language specific mode hooks.  The appropriate one is run as
;; the last thing when you enter that language mode.
;;
;; Although these hooks are variables defined in CC Mode, you can give
;; them values before CC Mode’s code is loaded—indeed, this is the only way
;; to use ‘c-initialization-hook’.  Their values aren’t overwritten when CC
;; Mode gets loaded.
;;;------------------------------------------------------------------------

(provide 'init-cc)
;;; init-cc ends here
