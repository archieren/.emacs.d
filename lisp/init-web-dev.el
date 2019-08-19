;;; init-web-dev --- Nothing.
;;; Commentary:
;; js is a build-in mode.
;; 其他均基于它。
;;; Code:
(require 'js2-mode)
(require 'json-mode)
(require 'prettier-js)
(require 'xref-js2)
(require 'js-comint)
(require 'add-node-modules-path)
(require 'flycheck)
(require 'init-utils)
(require 'diminish)

;;;----------------------------------------------------
;;;JavaScript
;;;----------------------------------------------------
(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)
(setq-default js-indent-level preferred-javascript-indent-level);;js-mode


;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))
(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

;; js2-mode
;; Change some defaults: customize them to override
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)
;; Disable js2 mode's syntax error highlighting by default...
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)
;; ... but enable it if flycheck can't handle javascript
(autoload 'flycheck-get-checker-for-buffer "flycheck")
(defun sanityinc/disable-js2-checks-if-flycheck-active ()
  "Nothing."
  (unless (flycheck-get-checker-for-buffer)
    (set (make-local-variable 'js2-mode-show-parse-errors) t)
    (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
(add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
(add-hook 'js2-mode-hook (lambda () (setq mode-name "")))
(js2-imenu-extras-setup)

;; xref-js2
;;(maybe-require-package 'xref-js2)
(when  (executable-find "ag")
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; 那么就看 https://github.com/redguardtoo/js-comint 里的介绍吧.
;; 以后有机会,按红卫兵的方式来配置,毕竟现在由他来维护.
(setq js-comint-program-command "node")
(defun inferior-js-mode-hook-setup ()
  "In order to get cleaner output when using NodeJS."
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))

(add-hook 'js-comint-mode-hook 'inferior-js-mode-hook-setup t)
(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil "" inferior-js-minor-mode-map)
(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))


(add-hook 'js2-mode-hook 'add-node-modules-path)


;;;----------------------------------------------
;;;Php
;;;----------------------------------------------
(require 'php-mode)
(require 'smarty-mode)
(require 'company-php)
(require 'init-company)
(add-hook 'php-mode-hook
          (lambda () (sanityinc/local-push-company-backend 'company-ac-php-backend)))

;;;-----------------------------------------------
;;;Html
;;;-----------------------------------------------
;;; Html:Html用build-in里的sgml-mode来实现.
(require 'tagedit)
(require 'sgml-mode)
(tagedit-add-paredit-like-keybindings)
(define-key tagedit-mode-map (kbd "M-?") nil)
(add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1)))
(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;;; Haml (HTML abstraction markup language)
;;; It is based on one primary principle: markup should be beautiful.
;;; Haml 是什么？ 好像被sass-mode给加载上来了.
;; 原来sass-mode是它的派生mode.
(require 'haml-mode)
(define-key haml-mode-map (kbd "C-o") 'open-line)

;;;----------------------------------------------
;;; CSS / SASS(Syntactically awesome style sheets) / SCSS(Sassy css)
;;;----------------------------------------------
;; CSS的预处理有好几个: SASS,Less,Stylus,.....且不管他.
(require 'css-mode)  ;; Shipped with Emacs
(require 'rainbow-mode)
(require 'mmm-mode)
(require 'sass-mode)
(require 'skewer-mode)
(require 'skewer-less)
(require 'css-eldoc)

(diminish 'rainbow-mode "")
(diminish 'skewer-mode "")
(diminish 'skewer-less-mode "")
(diminish 'skewer-css-mode "")
(diminish 'skewer-html-mode "")

(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))
;;Embedding in html
(require 'mmm-vars)
(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css-inline
    :submode css-mode
    :face mmm-code-submode-face
    :front "style=\""
    :back "\"")))
(dolist (mode (list 'html-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))
;; SASS and SCSS
;; Prefer the scss-mode built into Emacs
;; Emacs在css-model.el中,内建了对scss,less-css的支持.
;;  (unless (fboundp 'scss-mode)  (require-package 'scss-mode))
;;  (unless (fboundp 'less-css-mode) (require-package 'less-css-mode))
(setq-default scss-compile-at-save nil)
(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'skewer-less-mode)

;;;Skewer: live web development with Emacs
(add-hook 'skewer-mode-hook (lambda () (inferior-js-keys-mode -1)))
;; Skewer CSS
(add-hook 'js2-mode-hook  'skewer-mode)
(add-hook 'css-mode-hook  'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
;;; Use eldoc for syntax hint
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;;;-----------------------------------------
;;; HttpRepl and Restclient
;;;-----------------------------------------
(require 'httprepl)
(require 'restclient)
(add-auto-mode 'restclient-mode "\\.rest\\'")

(defun sanityinc/restclient ()
  "Nothing."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))


(provide 'init-web-dev)
;;; init-web-dev ends here
