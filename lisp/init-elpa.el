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
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;;(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
;;("melpa" . "https://melpa.org/packages/")
;;))
;;; Fire up package.el
;;;
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
;;; 千万不要修改下面两句,和emacs的初始化机制有关。
(setq package-enable-at-startup nil)
(package-initialize)
;;;{{{
;;;;Install some basic packages.
(require-package 'exec-path-from-shell)
(require-package 'fullframe)
(require-package 'cl-lib)
(require-package 'wgrep)
;;When we diminish a mode,
;;we are saying we want it to continue doing its work for us,
;;but we no longer want to be reminded of it.
(require-package 'diminish)
(require-package 'scratch)
(require-package 'command-log-mode)
(require-package 'disable-mouse)
(require-package 'evil)
  ;;; init-projectile
(require-package 'projectile)
 ;;; init-mode-line
(require-package 'powerline)
;;(require-package 'smart-mode-line)
;;(require-package 'smart-mode-line-powerline-theme)
(require-package 'airline-themes)
  ;;; init-frame-hooks init-xterm init-gui-frames
  ;;; init-dired
(require-package 'diredfl)
(require-package 'diff-hl)
  ;;; init-isearch
(require-package 'anzu)
;;; init-grep
;;(require-package 'wgrep)
(require-package 'ag)
(require-package 'wgrep-ag)
(require-package 'rg)
;;;init-uniquify
  ;;; init-ibuffer
(require-package 'ibuffer-vc)
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
  ;;; init-hippie-expand
;;; It's a build-in package.
  ;;; init-company
(require-package 'company)
(require-package 'company-quickhelp)
  ;;; init-window
(require-package 'switch-window)
(require-package 'window-number)
  ;;; init-fonts
(require-package 'visual-fill-column)
(require-package 'default-text-scale)
  ;;; init-mmm
(require-package 'mmm-mode)
  ;;; init-editing-utils
;; It's quite a little of complification.
(require-package 'fill-column-indicator)
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
;;(require-package 'nlinum)
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
;; init-lisp
(require-package 'elisp-slime-nav)
(require-package 'ipretty)
(require-package 'auto-compile)
(require-package 'immortal-scratch)
(require-package 'indent-guide)
(require-package 'aggressive-indent)
(require-package 'eldoc-eval)
(require-package 'cl-lib-highlight)
(require-package 'macrostep)
(require-package 'rainbow-mode)
(require-package 'aggressive-indent)
(require-package 'highlight-quoted)
;;(require-package 'flycheck)
(require-package 'flycheck-package)
(require-package 'cask-mode)
;; init-common-lisp
;; init-clojure
(require-package 'clojure-mode)
(require-package 'cljsbuild-mode)
(require-package 'elein)
(require-package 'cider)
(require-package 'flycheck-clojure)
;;(require-package 'nrepl-sync) ;;-- Remove it.
;; init-python
(require-package 'elpy)
(require-package 'py-autopep8)
;; init-erlang
(require-package 'erlang)
;; init-haskell
(require-package 'haskell-mode)
(require-package 'intero)
(require-package 'hindent)
(require-package 'ghc)
(require-package 'dhall-mode)
;; init-compile
(require-package 'alert)
(require-package 'cmd-to-echo)
;; init-textfile
(require-package 'textile-mode)
;; init-markdown
(require-package 'markdown-mode)
;; init-csv
(require-package 'csv-mode)
;; init-javascript
(require-package 'json-mode)
(require-package 'js2-mode)
(require-package 'coffee-mode)
(require-package 'typescript-mode)
(require-package 'prettier-js)
(require-package 'xref-js2)
(require-package 'js-comint)
(require-package 'skewer-mode)
(require-package 'add-node-modules-path)

;; init-org
(require-package 'org-pomodoro)
;; init-nxml
;; init-html
(require-package 'tagedit)
;; init-css
(require-package 'rainbow-mode)
(require-package 'mmm-mode)
(require-package 'sass-mode)
;;(require-package 'skewer-mode)
(require-package 'css-eldoc)
(require-package 'skewer-less)
;; init-haml
(require-package 'haml-mode)
;; init-http
(require-package 'httprepl)
(require-package 'restclient)
;; init-elm
(require-package 'elm-mode)
(require-package 'flycheck-elm)
;; init-purescript
(require-package 'purescript-mode)
(require-package 'psc-ide)
;; init-rust
(require-package 'rust-mode)
(require-package 'racer)
;;(require-package 'company)
(require-package 'flycheck-rust)
;; init-yaml
(require-package 'yaml-mode)
;; init-toml
(require-package 'toml-mode)
;; init-terraform
(require-package 'terraform-mode)
(require-package 'company-terraform)
;; Extra packages which don't require any configuration
(require-package 'gnuplot)
;;(require-package 'lua-mode)
;;(require-package 'htmlize)
;;(require-package 'dsvn)
;;(require-package 'daemons)
;;(require-package 'dotenv-mode)



;;(require-package 'nginx-mode)
(require-package 'uptimes)
;;; init-themes
(require-package 'xresources-theme)
(require-package 'spacemacs-theme)
(require-package 'zenburn-theme)
(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
;;--(require-package 'monokai-theme)
;;--(require-package 'molokai-theme)
;;--(require-package 'moe-theme)
;;--(require-package 'cyberpunk-theme)
;;--(require-package 'ample-theme)
;;--(require-package 'gotham-theme)
;;--(require-package 'gruvbox-theme)
;;--(require-package 'alect-themes)
;;--(require-package 'grandshell-theme)
;;--(require-package 'tangotango-theme)
;;--(require-package 'gruber-darker-theme)
;;--(require-package 'ample-zen-theme)
;;--(require-package 'flatland-theme)
;;--(require-package 'clues-theme)
;;--(require-package 'darkburn-theme)
;;--(require-package 'soothe-theme)
;;--(require-package 'dakrone-theme)
;;--(require-package 'busybee-theme)
;;--(require-package 'bubbleberry-theme)
;;--(require-package 'cherry-blossom-theme)
;;--(require-package 'heroku-theme)
;;--(require-package 'hemisu-theme)
;;--(require-package 'distinguished-theme)
;;--(require-package 'challenger-deep-theme)
;;--(require-package 'dimmer)
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
