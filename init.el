;;; init.el --- Bootstrap the configurations.
;;; Commentary:
;; S.Purcell modulizes the configurations.It's a good method.
;;; Version: 0.0.1
;;; HomePage: http://to
;;; Code:

(setq lexical-binding t)


;; Added by Package.el.  This must come before configurations of installed packages.
(package-initialize)

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Erase all the code with respect to enviroment-compatibility!
;;; Emacs > 25.3.
;;; Move out some utils from init-elpa.el.
;;; Borrowed some code from redguardtoo's work.
;;---------------------------------------------------------------------
;;; See redguardtoo's code.但最终感觉不是个好多少的方法.
(defmacro init-init (pkg)
  "PKG is the name of the related init file."
  `(load (file-truename (format "%s/lisp/%s",user-emacs-directory ,pkg))))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold  (* 4 1024 1024))))
;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(init-init 'init-utils)
(init-init 'init-site-lisp)
(init-init 'init-elpa)
(init-init 'init-exec-path)
(init-init 'init-compile)
;;----------------------------------------------------------------------------
;; Load most of basic packages in elpa ,and then config for specific features and modes
;;----------------------------------------------------------------------------

;; Project
(init-init 'init-projectile)
;; Emacs's expendation
(init-init 'init-gui)
;; Version Control
(init-init 'init-git)
(init-init 'init-dired)
(init-init 'init-grep)
(init-init 'init-flycheck)
(init-init 'init-ido)
(init-init 'init-ivy)
(init-init 'init-hippie-expand)
(init-init 'init-company)
(init-init 'init-editing-utils)

;; Programming language
(init-init 'init-cc)
(init-init 'init-lisp)
(init-init 'init-racket)
(init-init 'init-erlang)
(init-init 'init-python)
(init-init 'init-haskell)
(init-init 'init-rust)

;; Documents ML
(init-init 'init-doc)
(init-init 'init-org)

;; Web development
(init-init 'init-web-dev)





(setq-default uptimes-keep-count 200)
(add-hook 'after-init-hook (lambda () (require 'uptimes)))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file) (load custom-file))
;; Locales (setting them earlier in this file doesn't work in X)
(init-init 'init-locales)
;; Allow users to provide an optional "init-local" containing personal settings
(init-init 'init-local )
(init-init 'init-themes)
(provide 'init)
;;; init.el ends here
