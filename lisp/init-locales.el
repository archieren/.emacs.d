;;; init-locales --- Nothing.
;;; Commentary:
;;; Code:

(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun init-locales/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (init-locales/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (init-locales/utf8-locale-p (getenv "LC_ALL"))
      (init-locales/utf8-locale-p (getenv "LC_CTYPE"))
      (init-locales/utf8-locale-p (getenv "LANG"))))

(when (or window-system (init-locales/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init-locales)
;;; init-locales ends here
