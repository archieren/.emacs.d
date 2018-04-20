;;; init.el --- Bootstrap the configurations.
;;; Commentary:
;; S.Purcell modulizes the configurations.It's a good method.

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
;;; See redguardtoo's code.
(defmacro require-init (pkg)
  "PKG is the name of the related init file."
  `(load (file-truename (format "~/.emacs.d/lisp/%s" ,pkg))))


;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

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
(require-init 'init-utils)
(require-init 'init-site-lisp)
(require-init 'init-elpa)
(require-package 'exec-path-from-shell)
(require-init 'init-exec-path)
;;----------------------------------------------------------------------------
;; Load most of basic packages in elpa ,and then config for specific features and modes
;;----------------------------------------------------------------------------

;; Project
(require-init 'init-projectile)
;; Emacs's expendation
(require-init 'init-frame-hooks)
(require-init 'init-xterm)
(require-init 'init-gui-frames)
(require-init 'init-dired)
(require-init 'init-isearch)
(require-init 'init-grep)
(require-init 'init-uniquify)
(require-init 'init-ibuffer)
(require-init 'init-flycheck)
(require-init 'init-recentf)
(require-init 'init-ido)
(require-init 'init-smex)
(require-init 'init-ivy)
(require 'init-helm)
(require-init 'init-hippie-expand)
(require-init 'init-company)
(require-init 'init-windows)
;;(require-init 'init-sessions)
(require-init 'init-fonts)
(require-init 'init-mmm)

(require-init 'init-editing-utils)
(require-init 'init-whitespace)
;; Version Control
(require-init 'init-vc)
(require-init 'init-darcs)
(require-init 'init-git)
  ;;; From now on ,packages are loaded by themself!
;; Editing expandation
(require-init 'init-paredit)
(require-init 'init-slime)
(require-init 'init-misc)
(require-init 'init-folding)
;;(require-init 'init-dash);; Dash, a new list api.
;; Programming language
(require-init 'init-lisp)
(require-init 'init-common-lisp)
(require-init 'init-clojure)
(require-init 'init-erlang)
(require-init 'init-python)
(require-init 'init-haskell)
;;(require-init 'init-c)
;; Web development
;;(require-init 'init-compile)
;;(require-init 'init-textile)
;;(require-init 'init-markdown)
;;(require-init 'init-csv)
;;(require-init 'init-javascript)
(require-init 'init-org)
;;(require-init 'init-nxml)
;;(require-init 'init-html)
;;(require-init 'init-css)
;;(require-init 'init-haml)
;;(require-init 'init-http)
;;(require-init 'init-elm)
;;(require-init 'init-purescript)
;;(require-init 'init-ruby)
;;(require-init 'init-rails)
;;(require-init 'init-sql)
;;(require-init 'init-rust)
;;(require-init 'init-toml)
;;(require-init 'init-yaml)
;;(require-init 'init-docker)
;;(require-init 'init-terraform)
;;(require 'init-nix)
;;(require-package 'nginx-mode)

;;(require-init 'init-twitter)
;; (require-init 'init-mu)
;;(require-init 'init-ledger)

(require-package 'uptimes)
(setq-default uptimes-keep-count 200)
(add-hook 'after-init-hook (lambda () (require 'uptimes)))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))
;; Locales (setting them earlier in this file doesn't work in X)
(require-init 'init-locales)
;; Allow users to provide an optional "init-local" containing personal settings
(require-init 'init-local )
(require-init 'init-themes)
(provide 'init)
;;; init.el ends here
