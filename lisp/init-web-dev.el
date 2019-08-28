;;; init-web-dev --- Nothing.
;;; Commentary:
;; js is a build-in mode.
;; 其他均基于它。
;;; Code:
(require 'js2-mode)
(require 'typescript-mode)
(require 'json-mode)
(require 'prettier-js)
(require 'xref-js2)
(require 'js-comint)
(require 'add-node-modules-path)
(require 'flycheck)
(require 'init-utils)
(require 'diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              JSON-mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'json-mode-hook (lambda () (setq mode-name "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       Js2-mode For JavaScript       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; js2-mode
;; Change some defaults: customize them to override
(setq-default js-indent-level 4);;js2-basic-offset is it's alias!
(setq-default js2-basic-offset 4)
(setq-default js2-bounce-indent-p nil)
(setq-default js2-global-externs '("module" "require" "assert" "setInterval" "console" "__dirname__"))
(add-hook 'js2-mode-hook (lambda () (setq mode-name "")))
(add-hook 'js2-mode-hook 'add-node-modules-path)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          JavaScript-Eslint & Tide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JAVASCRIPT-ESLINT is a builtin checker in flycheck.
;; 我怀疑这个问题已经在flycheck中修正了.但还是留着吧,反正不冲突.
(defun my/use-eslint-from-node-modules ()
  "No Document Now!"
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory) "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)



;;; Tide
(require 'tide)
(diminish 'tide-mode " ")
(defun my/setup-tide-mode ()
  "Setup tide-mode."
  (interactive)
  (tide-setup)
  ;;Infact ,in my configuration,both flycheck-mode and company-mode are globaly setted!
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (add-to-list (make-local-variable 'company-backends) 'company-files)
  (company-mode +1)
  )

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'js2-mode-hook #'my/setup-tide-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (my/setup-tide-mode)
                           (message "hello")
                           ;; 下面一句似乎也无必要!tide-setup里似乎就是这么干的.
                           (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
                           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Typescript-Mode           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook (lambda () (setq mode-name "")))
(add-hook 'typescript-mode-hook 'add-node-modules-path)
(add-hook 'typescript-mode-hook #'my/setup-tide-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Web-Mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)
(require 'company-web)                  ; Provides three backends: company-web-{html,jade,slim}

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-content-types-alist '(("vue" . "\\.vue\\'")))

(defun my/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-css-colorization t)
  )

(defun my/web-html-setup ()
  "Web html mode's specific settings."
  (setq mode-name "")
  (flycheck-add-mode 'html-tidy 'web-mode) ;;Tidy install in archlinux: sudo pacman -S tidy
  (flycheck-select-checker 'html-tidy)
  (flycheck-mode))

(defun my/web-vue-setup ()
  "Setup for web-mode vue files."
  (setq mode-name "")
  (flycheck-mode)
  (my/setup-tide-mode)
  (prettier-js-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-select-checker 'javascript-eslint)
  ;;(my/use-eslint-from-node-modules)
  ;; Duplicated!
  ;; Why should use eslint from local?
  (add-to-list (make-local-variable 'company-backends)
               '(company-tide company-web-html company-files company-css))
  )

(add-hook 'web-mode-hook  'my/web-mode-hook)
(add-hook 'web-mode-hook (lambda () (add-to-list (make-local-variable 'company-backends)
                                            '(company-web-html company-files))))
(add-hook 'web-mode-hook (lambda () (cond ((equal web-mode-content-type "html")
                                      (my/web-html-setup))
                                     ((equal web-mode-content-type "vue")
                                      (my/web-vue-setup)))))



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

(diminish 'rainbow-mode " ")
(diminish 'skewer-mode " ")
(diminish 'skewer-less-mode " ")
(diminish 'skewer-css-mode " ")
(diminish 'skewer-html-mode " ")

(dolist (hook '(css-mode-hook
                web-mode-hook
                sass-mode-hook))
  (add-hook hook 'rainbow-mode))
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
(add-hook 'web-mode-hook 'skewer-html-mode)
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
