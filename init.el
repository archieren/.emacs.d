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
;;; Move some utils from init-elpa.el.
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
          (lambda () (setq gc-cons-threshold  (* 20 1024 1024))))
;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(init-init 'init-utils)
(init-init 'init-site-lisp)
(init-init 'init-elpa)
(init-init 'init-exec-path)
;;----------------------------------------------------------------------------
;; Load most of basic packages in elpa ,and then config for specific features and modes
;;----------------------------------------------------------------------------

;; Project
(init-init 'init-projectile)
;; Emacs's expendation
(init-init 'init-mode-line)
(init-init 'init-frame-hooks)
(init-init 'init-xterm)
(init-init 'init-gui-frames)
(init-init 'init-dired)
(init-init 'init-isearch)
(init-init 'init-grep)
(init-init 'init-uniquify)
(init-init 'init-ibuffer)
(init-init 'init-flycheck)
(init-init 'init-recentf)
(init-init 'init-ido)
(init-init 'init-smex)
(init-init 'init-ivy)
(init-init 'init-helm)
(init-init 'init-hippie-expand)
(init-init 'init-company)
(init-init 'init-windows)
;;(init-init 'init-sessions)
(init-init 'init-fonts)
(init-init 'init-mmm)

(init-init 'init-editing-utils)
(init-init 'init-whitespace)
;; Version Control
(init-init 'init-vc)
(init-init 'init-darcs)
(init-init 'init-git)
  ;;; From now on ,packages are loaded by themself!
;; Editing expandation
(init-init 'init-paredit)
(init-init 'init-slime)
(init-init 'init-misc)
(init-init 'init-folding)
;;(init-init 'init-dash);; Dash, a new list api.
;; Programming language
(init-init 'init-lisp)
(init-init 'init-common-lisp)
(init-init 'init-clojure)
(init-init 'init-erlang)
(init-init 'init-python)
(init-init 'init-haskell)
;;(init-init 'init-c)
;; Web development
(init-init 'init-compile)
(init-init 'init-textile)
(init-init 'init-markdown)
(init-init 'init-csv)
(init-init 'init-javascript)
(init-init 'init-org)
(init-init 'init-nxml)
(init-init 'init-html)
(init-init 'init-css)
(init-init 'init-haml)
(init-init 'init-http)
(init-init 'init-elm)
;;(init-init 'init-purescript)
;;(init-init 'init-ruby)
;;(init-init 'init-rails)
;;(init-init 'init-sql)
;;(init-init 'init-rust)
;;(init-init 'init-toml)
(init-init 'init-yaml)
;;(init-init 'init-docker)
;;(init-init 'init-terraform)
;;(require 'init-nix)


;;(init-init 'init-twitter)
;; (init-init 'init-mu)
;;(init-init 'init-ledger)

(setq-default uptimes-keep-count 200)
(add-hook 'after-init-hook (lambda () (require 'uptimes)))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))
;; Locales (setting them earlier in this file doesn't work in X)
(init-init 'init-locales)
;; Allow users to provide an optional "init-local" containing personal settings
(init-init 'init-local )
(init-init 'init-themes)
(provide 'init)
;;; init.el ends here
