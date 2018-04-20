;;; init-elpa.el --- Init the elpa or it's mirrors.
;;; Commentary:

;;; Code:
(require 'package)


;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))



;;; Mirrors of Standard package repositories
;;; 我使用清华的镜像
;;(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")))
;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)
;;;{{{  Install some basic packages.
(require-package 'fullframe)
(require-package 'cl-lib)
(require-package 'wgrep)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'command-log-mode)
(require-package 'disable-mouse)
  ;;; init-projectile
(require-package 'projectile)
  ;;; init-frame-hooks init-xterm init-gui-frames
  ;;; init-dired
(require-package 'diredfl)
(require-package 'diff-hl)
  ;;; init-isearch
(require-package 'anzu)
  ;;; init-grep init-uniquify
  ;;; init-ibuffer
  ;;; init-flycheck
(require-package 'flycheck)
(require-package 'flycheck-color-mode-line)
  ;;; init-recentf
  ;;; init-ido
(require-package 'flx-ido)
  ;;; init-smex
(require-package 'smex)
  ;;;init-ivy
(require-package 'ivy)
(require-package 'ivy-historian)
(require-package 'ivy-xref)
(require-package 'counsel)
(require-package 'swiper)
  ;;; init-helm
  ;;; init-hippie-expand init-company
  ;;; init-window
(require-package 'switch-window)
(require-package 'window-number)
  ;;; init-fonts
(require-package 'default-text-scale)
  ;;; init-mmm
(require-package 'mmm-mode)
  ;;; init-editing-utils
(require-package 'unfill)
(require-package 'list-unicode-display)
(require-package 'vlf)  ;; Very large file.
(require-package 'mode-line-bell)
(require-package 'beacon)
(require-package 'rainbow-delimiters)
(require-package 'undo-tree)
(require-package 'symbol-overlay)
(require-package 'browse-kill-ring)
(require-package 'expand-region)
(require-package 'avy)
(require-package 'multiple-cursors)
(require-package 'page-break-lines)
(require-package 'move-dup)
(require-package 'whole-line-or-region)
(require-package 'highlight-escape-sequences)
(require-package 'guide-key)
  ;;; init-whitespace
(require-package 'whitespace-cleanup-mode)
  ;;; init-vc
(require-package 'diff-hl)
(require-package 'browse-at-remote)
  ;;; init-darcs
(require-package 'darcsum)
(require-package 'vc-darcs)
  ;;; init-git
(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-timemachine)
(require-package 'magit)
(require-package 'fullframe)
(require-package 'git-commit)
(require-package 'git-messenger)
(require-package 'yagist)
(require-package 'bug-reference-github)
(require-package 'github-clone)
(require-package 'magithub)
(require-package 'git-gutter)
  ;;; init-paredit
(require-package 'paredit)
(require-package 'paredit-everywhere)
  ;;; init-slime
(require-package 'slime)
(require-package 'hippie-expand-slime)
(require-package 'slime-company)
;;; init-misc
;;; init-folding
(require-package 'origami)
;;; init-org
(require-package 'org-cliplink)
(require-package 'writeroom-mode)
;; Extra packages which don't require any configuration
(require-package 'gnuplot)
;;(require-package 'lua-mode)
;;(require-package 'htmlize)
;;(require-package 'dsvn)
;;(require-package 'daemons)
;;(require-package 'dotenv-mode)
;;; init-themes
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
(require-package 'dimmer)
;;;}}}
(fullframe list-packages quit-window)
(require 'cl-lib)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)


(provide 'init-elpa)
;;; init-elpa.el ends here
