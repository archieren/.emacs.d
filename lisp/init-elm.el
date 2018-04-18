;;; init-elm --- Nothing.
;;; Commentary:
;;  Elm is a functional language, with support for functional graphical layout and reactive programming.
;;  Elm is transpiled into JavaScript so it can run in all modern browsers.
;;  See https://legacy.gitbook.com/@csmith111
;;; Code:
(require-package 'elm-mode)
(require-package 'flycheck-elm)

(setq-default elm-format-on-save t)
(after-load 'elm-mode
  (diminish 'elm-indent-mode)
  (add-hook 'elm-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-elm)))
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t)))

(after-load 'elm-mode
  (flycheck-elm-setup))

(provide 'init-elm)
;;; init-elm ends here
