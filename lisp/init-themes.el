;;; init-themes.el --- Config the themes!
;;; Commentary:
;; 设置主题
;; sanityinc-solarized-dark sanityinc-tomorrow-bright sanityinc-tomorrow-night
;; xresources spacemacs-dark spacemacs-light
;;; Code:

(require-package 'xresources-theme)
(require-package 'spacemacs-theme)
(require-package 'zenburn-theme)
(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'monokai-theme)
(require-package 'molokai-theme)
(require-package 'moe-theme)
(require-package 'cyberpunk-theme)
(require-package 'ample-theme)
(require-package 'gotham-theme)
(require-package 'gruvbox-theme)
(require-package 'alect-themes)
(require-package 'grandshell-theme)
(require-package 'tangotango-theme)
(require-package 'gruber-darker-theme)
(require-package 'ample-zen-theme)
(require-package 'flatland-theme)
(require-package 'clues-theme)
(require-package 'darkburn-theme)
(require-package 'soothe-theme)
(require-package 'dakrone-theme)
(require-package 'busybee-theme)
(require-package 'bubbleberry-theme)
(require-package 'cherry-blossom-theme)
(require-package 'heroku-theme)
(require-package 'hemisu-theme)
(require-package 'distinguished-theme)
(require-package 'challenger-deep-theme)
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


;;------------------------------------------------------------------------------
;; Toggle between light , dark and dark-solarized
;;------------------------------------------------------------------------------
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

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.1)
  (add-hook 'after-init-hook 'dimmer-mode))


(provide 'init-themes)
;;; init-themes ends here
