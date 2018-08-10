;;; init-javascript --- Nothing.
;;; Commentary:
;; js is a build-in mode.
;; 其他均基于它。
;;; Code:
(require 'js2-mode)
(require 'json-mode)
(require 'coffee-mode)
(require 'typescript-mode)
(require 'prettier-js)
(require 'xref-js2)
(require 'js-comint)
(require 'add-node-modules-path)

(require 'flycheck)
(require 'init-utils)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode
;; Change some defaults: customize them to override
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "")))

  (with-eval-after-load 'js2-mode
    (js2-imenu-extras-setup)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; xref-js2
;;(maybe-require-package 'xref-js2)
(when  (executable-find "ag")
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

;;; Coffee-mode for Coffeescript
;; 这是个什么东东?
;; CoffeeScript是逐句编译为JavaScript的一种小型语言，且没有运行时的解释器.
;; CoffeeScript旨在编译人类可读,美观优雅且速度不输原生的代码,
;; 且编译后的代码可以在任何JavaScript运行时正确运行.
;; 作者是GitHub!
;; An Emacs major mode for CoffeeScript and IcedCoffeeScript.
(with-eval-after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))



;; Run and interact with an inferior JS via js-comint.el
;; 那么就看 https://github.com/redguardtoo/js-comint 里的介绍吧.
;; 在 js-comint 中,倒是定义了custom变量js-comint-program-{command,arguments}
;; 真找不到 inferior-js-program-command的定义,也不知道谁会用.
;; 因为有个 js-comint-program-command的可定制变量，好像在起同样的作用。
;; 在 Linux 下,屏蔽它,好像也无事.
;; (setq inferior-js-program-command "node")

(defun inferior-js-mode-hook-setup ()
  "In order to get cleaner output when using NodeJS.
Suggested by chenbin"
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil "" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))


;;(maybe-require-package 'add-node-modules-path)
(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook 'add-node-modules-path))
(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook 'add-node-modules-path))


(provide 'init-javascript)
;;; init-javascript ends here
