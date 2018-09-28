;;; init-themes.el --- Config the themes!
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;;;  Themes
;;----------------------------------------------------------------------------
;; sanityinc-solarized-dark sanityinc-tomorrow-bright sanityinc-tomorrow-night
;; xresources spacemacs-dark spacemacs-light
;; dracula

;;; Dimmer
(require 'dimmer)
(setq-default dimmer-fraction 0.1)
(add-hook 'after-init-hook 'dimmer-mode)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-solarized-dark))
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
(add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light , dark and dark-solarized
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))
(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (reapply-themes))
(defun solarized-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-solarized-dark))
  (reapply-themes))



(provide 'init-themes)
;;; init-themes ends here
