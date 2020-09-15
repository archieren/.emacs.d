;;; init.el --- Bootstrap the configurations
;;; Commentary:
;; Hello!
;;; Version: 0.0.1
;;; HomePage: http://to
;;; Code:

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;       Some Settings For Init.       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error t)
(setq gc-cons-threshold most-positive-fixnum)
(defvar init-best-gc-cons-threshold
  (* 4 1024 1024)
  "Best default gc threshold value.
Should Not be too big." )
(add-hook `after-init-hook
          (lambda ()
	    (setq gc-cons-threshold  init-best-gc-cons-threshold)
	    (setq debug-on-error nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                Backup               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;   Settings about Packages-Install   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;About Loading ...
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require `package)
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))
(setq package-archives `(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(package-initialize)
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install `use-package))
(eval-when-compile (require `use-package))
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;              Basic Env              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var `("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list `exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(use-package which-key
  :ensure t
  :diminish ""
  :config
  ;; Minibufer sometimes is too small.
  (setq which-key-popup-type `side-window)
  (setq which-key-side-window-location `bottom)
  (setq which-key-side-window-max-height 0.5)
  (which-key-mode t))
(use-package dash :ensure t)
(setq auto-save-default nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                  UI                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq-default cursor-type `bar )
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-hook `window-setup-hook `toggle-frame-maximized t)
(pixel-scroll-mode t)
(electric-pair-mode t)
(electric-indent-mode t)
(use-package winner
  :ensure t
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode t))
(defun init-defaults ()
  "Something should be done for all."
  (require `diminish)
  (diminish `subword-mode)
  (diminish `eldoc-mode)
  (diminish `hs-minor-mode)
  (add-hook `prog-mode-hook `hs-minor-mode)
  (column-number-mode t)
  ;;linum-mode has some problem with git-gutter-mode and multicursor-mode
  ;;(global-linum-mode t)
  (delete-selection-mode t))
(add-hook `after-init-hook `init-defaults)
;; The init.el 将经常被修改，所以定义一个快捷键 s-f .
(defun init-open-init ()
  "The init-local.el is used for customization."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "s-f") `init-open-init)
;;;
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)
;;;
(setq mouse-wheel-scroll-amount `(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;;;
(setq uniquify-buffer-name-style `reverse)
(setq uniquify-separator "@")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;;;
(subword-mode t)
(show-paren-mode t)
(global-prettify-symbols-mode t)
;;;
(add-hook 'after-init-hook '(lambda ()
			      (global-hl-line-mode) ;; 跟踪当前行
			      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 Avy,Ace-window      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :pin melpa
  :config
  (global-set-key (kbd "C-:") `avy-goto-char-timer))
(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] `ace-window)
  (global-set-key (kbd "M-o") `ace-window))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Window-number            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package window-number
;;   :ensure t
;;   :config
;;   (window-number-mode t)
;;   (window-number-meta-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                Winum                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package winum
  :ensure t
  :config
  ;;The default prefix for key bindings is C-x w for compatibility with native Emacs bindings.
  (winum-set-keymap-prefix (kbd "s-c w"))
  (setq winum-format "%s")
  (winum-mode t))

(require `autorevert)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;;;
(use-package beacon
  :ensure t
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 9)
  (beacon-mode t))
;;;Fonts
;;----------------------------------------------------------------------------
;;; 字体设置 - 用等宽字体，比较好对齐中英文!
;;----------------------------------------------------------------------------
(use-package all-the-icons
  :ensure t ; 第一次时,另外需要手工执行 M-x all-the-icons-install-fonts
  :config
  (set-face-attribute  'default
		       nil
		       :font  (font-spec :family "DejaVu Sans Mono"
					 :size 14))
  (dolist (script '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font  t ;;(frame-parameter nil 'font)
		       script
		       (font-spec :family "Noto Sans Mono CJK SC"
				  :size 14)))
  (setq face-font-rescale-alist '(("DejaVu Sans Mono" . 1.0)
				  ("Noto Sans Mono CJK SC" . 1.0))))
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme `doom-solarized-dark t)
  ;; (load-theme `doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Set the treemacs-theme
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))
(use-package doom-modeline
  :ensure t
  ;:init (doom-modeline-mode t)
  :config
  (doom-modeline-mode t)
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  (setq doom-modeline-buffer-file-name-style `truncate-except-project)
  (setq doom-modeline-project-detection `projectile)
  (setq doom-modeline-minor-modes nil))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  ;; whole-line-or-region-mode 是 whole-line-or-region-global-mode的别名!
  (whole-line-or-region-global-mode t))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish ""
  :config
  (require `whitespace)
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 256)
  (diminish `global-whitespace-mode)
  (global-whitespace-mode t)
  (setq-default show-trailing-whitespace t)
  (defun  init-gui-no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace nil))
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (dolist (hook `(special-mode-hook
		  Info-mode-hook
		  eww-mode-hook
		  term-mode-hook
		  comint-mode-hook
		  compilation-mode-hook
		  twittering-mode-hook
		  minibuffer-setup-hook
		  eshell-mode-hook
		  cmake-mode-hook))
    (add-hook hook `init-gui-no-trailing-whitespace))
  (add-hook `before-save-hook `whitespace-cleanup)
  (global-whitespace-cleanup-mode))


;; Help functions for (Emacs)lispy mode!
(require `derived)
(defconst init-emacs-lispy-modes
  `(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")
(defconst init-lispy-modes
  (append init-emacs-lispy-modes
          `(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")
(defun init-add-hook-to-emacss-lispy-modes (action)
  "Add ACTION to Emacs lispy modes."
  (dolist (hook (mapcar `derived-mode-hook-name init-emacs-lispy-modes))
    (add-hook hook action)))
(defun init-add-hook-to-lispy-modes (action)
  "Add ACTION to Emacs lispy modes."
  (dolist (hook (mapcar `derived-mode-hook-name init-lispy-modes))
    (add-hook hook action)))

(use-package rainbow-delimiters
  :ensure t
  :diminish ""
  :config
  (add-hook `prog-mode-hook `rainbow-delimiters-mode))
(use-package highlight-indentation
  :ensure t
  :diminish ""
  :config
  ;;(set-face-background 'highlight-indentation-face "#e3e3d3")
  ;;(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  )
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method `column )
  ;;(add-hook `prog-mode-hook `highlight-indent-guides-mode)
  )
(use-package highlight-escape-sequences
  :ensure t
  :diminish ""
  :config
  (hes-mode))
(use-package symbol-overlay ;;做文件中的替换挺有意思的!
  :ensure t
  :diminish ""
  :hook ((prog-mode html-mode css-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind-keymap ("C-c o" . symbol-overlay-map)
  :bind (
	 ("M-n" . symbol-overlay-jump-next)
	 ("M-p" . symbol-overlay-jump-prev)))
(use-package browse-kill-ring
  :ensure t
  :diminish ""
  :bind (("s-y" . browse-kill-ring)))
;;----------------------------------------------------------------------------
;; Page break lines
;; C-q C-l插入 ^L ，C-x {],[}导航
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :ensure t
  :diminish ""
  :config
  (global-page-break-lines-mode)
  (push `browse-kill-ring-mode page-break-lines-modes)
  (put `narrow-to-region `disabled nil)
  (put `narrow-to-page `disabled nil)
  (put `narrow-to-defun `disabled nil))

(use-package undo-tree ;;这个没有好好用过,据说很有用!
  :ensure t
  :diminish ""
  :config
  (global-undo-tree-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-c p") `projectile-command-map)
  ;; ------------------Added by you need
  ;; All
  (add-to-list `projectile-globally-ignored-directories ".vscode")
  (add-to-list `projectile-globally-ignored-directories "build")
  (add-to-list `projectile-globally-ignored-directories "data")
  (add-to-list `projectile-globally-ignored-directories ".stack-work")
  ;; java-script modes
  (add-to-list `projectile-globally-ignored-directories "node_modules")
  (add-to-list `projectile-globally-ignored-directories "flow-typed")
  ;; cc-mode
  (add-to-list `projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list `projectile-globally-ignored-directories ".clangd")
  (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
  (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json")
  ;; ------------------

  (projectile-mode t))

(use-package rg
  :ensure t
  ;:bind (("M-?" . rg-project))
  :config
  (global-set-key (kbd "s-?") 'rg-project)
  (unless (executable-find "rg")
    (warn "\nWARNING: Could not find the ripgrep executable."))
  (use-package deadgrep
    :ensure t))

(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 5)
  (setq tab-always-indent `complete)
  (use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :init
    (add-hook `prog-mode-hook `yas-minor-mode)
    :config
    (yas-reload-all))
  (use-package yasnippet-snippets :ensure t)
  (defvar my-company-point)
  (advice-add `company-complete-common
	      :after (lambda ()
		       (when (equal my-company-point (point))
			 (yas-expand))))
  (advice-add `company-complete-common
	      :before (lambda ()
			(setq my-company-point (point))))
  ;; hippie-expand 是内建的.
  ;; 暂时放在这儿!
  (global-set-key (kbd "M-/") `hippie-expand)
  (setq hippie-expand-try-functions-list
	`(try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill)))

(use-package flycheck
  :ensure t
  :diminish ""
  :config
  (global-flycheck-mode t)
  (setq flycheck-display-errors-function
	`flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-emacs-lisp-load-path `inherit)
  (use-package flycheck-package
    :ensure t
    :config
    (flycheck-package-setup)))

(use-package ivy
  :ensure t
  :diminish ""
  :bind (("C-c C-r" . ivy-resume)
	 :map ivy-minibuffer-map
	      ("RET" . ivy-alt-done)
	      ("C-j" . ivy-immediate-done))
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "[%d/%d] ")
  ;;Projectile-completion-system 有三 ido,helm,ivy
  (if (package-installed-p `projectile)
      (setq projectile-completion-system `ivy)))
(use-package ivy-xref
  :ensure t
  :config
  (require `xref)
  (setq xref-show-xrefs-function `ivy-xref-show-xrefs))
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x b" . counsel-switch-buffer)
	 ("s-c c r" . counsel-rg)
	 ("s-c c a" . counsel-ag))
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep,ag, because it's way faster
      (setq counsel-grep-base-command
	    "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
	    counsel-rg-base-command
	    "rg -i -M 120 --no-heading --line-number --color never %s ."
	    counsel-ag-base-command
	    "rg -i -M 120 --no-heading --line-number --color never %s .")
    (warn "\nWARNING: Could not find the ripgrep executable.")))
(use-package counsel-etags
  :ensure t
  :bind (("s-c c ." . counsel-etags-find-tag-at-point)
	 ("s-c c g" . counsel-etags-grep-symbol-at-point)
	 ("s-c c f" . counsel-etags-find-tag)
	 ("s-c c s" . counsel-etags-scan-code))
  :config
  ;; Ignores of ctags
  (add-to-list `counsel-etags-ignore-directories       "build_clang")
  (add-to-list `counsel-etags-ignore-directories       "build")
  (add-to-list `counsel-etags-ignore-directories       ".vscode")
  ;; haskell
  (add-to-list `counsel-etags-ignore-filenames         "*.yaml")
  (add-to-list `counsel-etags-ignore-filenames         "*.cabal")
  (add-to-list `counsel-etags-ignore-filenames         "*.hi")
  (add-to-list `counsel-etags-ignore-directories       ".stack-work")
  (add-to-list `counsel-etags-ignore-directories       "dist")
  ;;rust
  (add-to-list `counsel-etags-ignore-directories       "target")
  (add-to-list `counsel-etags-ignore-filenames         "*.lock")
  (add-to-list `counsel-etags-ignore-filenames         "*.toml")
  ;;clang
  (add-to-list `counsel-etags-ignore-filenames         "*.clang-format")
  ;; NodeJS
  (add-to-list `counsel-etags-ignore-directories       "node_modules"))
(if (package-installed-p `projectile)
    (use-package counsel-projectile
      :ensure t
      :diminish ""
      :config
      (counsel-projectile-mode t)))
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;;;Dired
(require `dired)
(setq-default dired-dwim-target t)
(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] `dired-find-alternate-file)
(define-key dired-mode-map (kbd "RET") `dired-find-alternate-file)
(define-key dired-mode-map (kbd "C-c C-p") `wdired-change-to-wdired-mode)
;; 只在一个buffer 里打开 dired-mode!
(put `dired-find-alternate-file 'disabled nil)
;;Dired里显示一些版本信息.
(use-package diff-hl
  :ensure t
  :diminish ""
  :config
  (add-hook `dired-mode-hook `diff-hl-dired-mode)
  (global-diff-hl-mode t))
(use-package diredfl
  :ensure t
  :diminish ""
  :config
  (diredfl-global-mode))
;; Modify the default ibuffer-formats (toggle with `)
(use-package ibuffer-vc
  :ensure t
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :init
  (require `ibuf-ext)
  (defun init-ibuffer-set-up-preferred-filters ()
    "IBuffer set up preferred filters."
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode `filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook `ibuffer-hook `init-ibuffer-set-up-preferred-filters)
  (setq-default ibuffer-show-empty-filter-groups nil)
  (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (file-size-human-readable (buffer-size)))
  (setq ibuffer-formats
	'((mark modified read-only vc-status-mini " "
		(name 22 22 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 12 12 :left :elide)
		" "
		vc-relative-file)
	  (mark modified read-only vc-status-mini " "
		(name 22 22 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 14 14 :left :elide)
		" "
		(vc-status 12 12 :left)
		" "
		vc-relative-file)))
  (setq ibuffer-filter-group-name-face `font-lock-doc-face))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;           VC,Git and Magit          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-blamed :ensure t)
(use-package gitignore-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine-toggle)))
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 :map  magit-status-mode-map
	 ("C-M-<up>" . magit-section-up)))
;(use-package magit-popup :ensure t)
(use-package git-commit
  :ensure t
  :hook (git-commit-mode . goto-address-mode))
(use-package git-messenger :ensure t)

(use-package bug-reference-github :ensure t)
(use-package github-clone :ensure t)
(use-package magithub :ensure t)
(use-package git-gutter
  :ensure t
  :bind (("C-x v =" . git-gutter:popup-hunk)
	 ("C-x v s" . git-gutter:stage-hunk)
	 ("C-x v r" . git-gutter:revert-hunk))
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup))
(use-package fullframe :ensure t
  :config
  (fullframe ibuffer ibuffer-quit)
  (fullframe magit-status magit-mode-quit-window)
  (fullframe list-packages quit-window))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;               Treemacs              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-is-never-other-window t)
  (use-package treemacs-icons-dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))
  (use-package treemacs-projectile
    :ensure t)
  (use-package treemacs-magit
    :ensure t)
  :bind
  (:map global-map
        ("s-c t 0"   . treemacs-select-window)
        ("s-c t 1"   . treemacs-delete-other-windows)
        ("s-c t t"   . treemacs)
        ("s-c t B"   . treemacs-bookmark)
        ("s-c t C-t" . treemacs-find-file)
        ("s-c t M-t" . treemacs-find-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;              Languages              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list `auto-mode-alist (cons pattern mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;        Common Lisp,ELisp,and Racket         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-lisp-disable-indent-guide ()
  "Nothing."
  (when (bound-and-true-p indent-guide-mode)
    (indent-guide-mode -1)))
(defun init-lisp-enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook `after-save-hook `check-parens nil t))
(use-package paredit ;; 特属Lispy的!
  :ensure t
  :diminish ""
  :config
  (autoload `enable-paredit-mode "paredit")
  ;; lsipy-modes is (emacs-lisp-mode ielm-mode lisp-mode inferior-lisp-mode lisp-interaction-mode)
  (init-add-hook-to-lispy-modes `enable-paredit-mode)
  (init-add-hook-to-lispy-modes `init-lisp-enable-check-parens-on-save)
  ;;
  (defun init-maybe-map-paredit-newline ()
    "Maybe map paredit newline."
    (unless (or (memq major-mode `(inferior-emacs-lisp-mode cider-repl-mode))
		(minibufferp))
      (local-set-key (kbd "RET") `paredit-newline)))
  (add-hook `paredit-mode-hook `init-maybe-map-paredit-newline)
  ;;
  (dolist (binding '("C-<2left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
  ;;
  (defvar init-paredit-minibuffer-commands `(eval-expression
					     pp-eval-expression
					     eval-expression-with-eldoc
					     ibuffer-do-eval
					     ibuffer-do-view-and-eval)
    "Interactive commands \\ for which paredit should be enabled in the minibuffer.")
  (defun init-conditionally-enable-paredit-mode ()
    "Enable paredit during lisp-related minibuffer commands."
    (if (memq this-command init-paredit-minibuffer-commands)
	(enable-paredit-mode)))
  (add-hook `minibuffer-setup-hook `init-conditionally-enable-paredit-mode))
(use-package paredit-everywhere
  :ensure t
  :diminish ""
  :hook ((prog-mode css-mode) . paredit-everywhere-mode))


(use-package elisp-slime-nav
  :ensure t
  :diminish ""
  :config
  ;; emacss-lispy-modes 含有 emacs-lisp-mode 和 ielm-mode
  (require `hippie-exp)
  (defun init-hippie-expand-for-elisp ()
    "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
    (make-local-variable 'hippie-expand-try-functions-list)
    (add-to-list `hippie-expand-try-functions-list `try-complete-lisp-symbol t)
    (add-to-list `hippie-expand-try-functions-list `try-complete-lisp-symbol-partially t))
  (init-add-hook-to-emacss-lispy-modes `init-hippie-expand-for-elisp)
  ;;
  (init-add-hook-to-emacss-lispy-modes `turn-on-eldoc-mode)
  (init-add-hook-to-emacss-lispy-modes `turn-on-elisp-slime-nav-mode)
  ;; (add-hook `emacs-lisp-mode-hook (lambda () (setq mode-name "")))
  ;; (add-hook `ielm-mode-hook (lambda () (setq mode-name "")))
  (setq-default initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs  you!\n\n"))
  ;; Make C-x C-e run 'eval-region if the region is active
  (defun init-eval-last-sexp-or-region (prefix)
    "PREFIX:the last sexp.
Eval region from begin-mark to end-mark if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
	(eval-region
	 (min (point) (mark))
	 (max (point) (mark)))
      (pp-eval-last-sexp prefix)))
  (global-set-key [remap eval-expression] `pp-eval-expression)
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") `init-eval-last-sexp-or-region)
  ;;Read-only
  (defun init-lisp-maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
	       (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))
  (add-hook `emacs-lisp-mode-hook `init-lisp-maybe-set-bundled-elisp-readonly)
  (use-package auto-compile
    :ensure t
    :config
    (auto-compile-on-save-mode t)
    (auto-compile-on-load-mode t))
  (setq load-prefer-newer t) ;; Load .el if newer than corresponding .elc
  (init-add-auto-mode `emacs-lisp-mode "\\.emacs-project\\'")
  (init-add-auto-mode `emacs-lisp-mode "archive-contents\\'")
  ;;Common for Lispy Or Emacs-Lispy
  (use-package ipretty :ensure t :config (ipretty-mode t))
  ;;
  (use-package immortal-scratch
    :ensure t
    :config (immortal-scratch-mode))
  ;;
  (use-package indent-guide
    :ensure t
    :config
    (init-add-hook-to-lispy-modes `init-lisp-disable-indent-guide))
  ;;
  (use-package aggressive-indent
    :ensure t)
  (use-package cl-lib-highlight
    :ensure t
    :config
    (cl-lib-highlight-initialize))
  (use-package macrostep
    :ensure t
    :config
    ;; The standard keybindings in macrostep-mode are the following:
    ;; e, =, RET
    ;; expand the macro form following point one step
    ;; c, u, DEL
    ;; collapse the form following point
    ;; q, C-c C-c
    ;; collapse all expanded forms and exit macrostep-mode
    ;; n, TAB
    ;; jump to the next macro form in the expansion
    ;; p, M-TAB
    ;; jump to the previous macro form in the expansion
    (define-key emacs-lisp-mode-map (kbd "C-c e") `macrostep-expand))
  (use-package highlight-quoted
    :ensure t
    :config
    (add-hook `emacs-lisp-mode-hook `highlight-quoted-mode))
  (use-package rainbow-mode ;; #a0a0a0
    :ensure t
    :diminish ""
    :config
    (defun init-enable-rainbow-mode-if-theme ()
      (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
	(rainbow-mode)))
    (add-hook `emacs-lisp-mode-hook `init-enable-rainbow-mode-if-theme)
    (add-hook `help-mode-hook `rainbow-mode)))



;;Common-Lisp
(use-package slime
  :ensure t
  :config
  ;;
  (require `slime)
  (init-add-auto-mode `lisp-mode "\\.cl\\'")
  ;; (add-hook `lisp-mode-hook (lambda () (setq mode-name "")))
  (add-hook `lisp-mode-hook
	    (lambda () (unless (featurep `slime)
		    (require `slime)
		    (normal-mode))))
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix)))
  (when (executable-find "lisp")
    (add-to-list 'slime-lisp-implementations
                 '(cmucl ("lisp") :coding-system iso-latin-1-unix)))
  (when (executable-find "ccl")
    (add-to-list 'slime-lisp-implementations
                 '(ccl ("ccl") :coding-system utf-8-unix)))
  ;;
  (slime-setup)
  (slime-setup `(slime-fancy slime-banner slime-repl slime-fuzzy))
  (use-package hippie-expand-slime
    :ensure t
    :config
    (add-hook `slime-mode-hook `set-up-slime-hippie-expand))
  (use-package slime-company
    :ensure t
    :config
    (slime-setup `(slime-company)))
  (setq slime-protocol-version `ignore)
  (setq slime-net-coding-system `utf-8-unix)
  (setq slime-completion-at-point-functions `slime-fuzzy-complete-symbol)
  (require `slime-repl)
  (require `slime-c-p-c)
  (require `slime-macrostep)
  (diminish `slime-autodoc-mode)
  (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") `indent-for-tab-command)
  (add-hook `slime-repl-mode-hook (lambda ()
				    ;; (setq mode-name "")
				    (setq show-trailing-whitespace nil)
				    (paredit-mode +1)
				    (turn-on-eldoc-mode))))


(use-package cask-mode :ensure t)

;; init-racket
(use-package racket-mode
  :ensure t
  :config
  (require 'racket-mode)
  ;;(add-hook 'racket-mode-hook      (lambda () (setq mode-name "")))
  ;;(add-hook 'racket-repl-mode-hook (lambda () (setq mode-name "")))
  (setq racket-program "racket")
  ;; racket-racket-program, racket-raco-program are obsolete variables!
  ;; (setq racket-raco-program "raco")
  (add-hook 'racket-mode-hook
	    (lambda ()
	      (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
  (add-hook 'racket-mode-hook      `racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook `racket-unicode-input-method-enable)

  (add-hook 'racket-mode-hook (lambda () (enable-paredit-mode))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;               Lsp-mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :diminish `lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "s-c l")
  :config
  (use-package lsp-ui
    :ensure t
    ;; By default, lsp-mode automatically activates lsp-ui unless lsp-auto-configure is set to nil.
    ;; You only have to put (use-package lsp-ui) in your config and the package will work out of the box.
    ;; :init (add-hook `lsp-mode-hook `lsp-ui-mode)
    :config
    ;;------------------
    (setq lsp-ui-doc-delay 0.1)
    (setq lsp-ui-doc-position `at-point)
    (setq lsp-ui-doc-max-height 8)
    (setq lsp-ui-doc-border "purple" )
    (setq lsp-ui-doc-enable nil)
    ;;------------------
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-ignore-duplicate t)
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-enable t)
    ;;------------------
    (define-key lsp-ui-mode-map (kbd "M-.") `lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map (kbd "M-?") `lsp-ui-peek-find-references)
    (setq lsp-ui-peek-enable t))
  ;; company-lsp is no longer supported!
  ;; (use-package company-lsp
  ;;   :ensure t
  ;;   :config
  ;;   (push `company-lsp company-backends))
  (use-package lsp-ivy
    :ensure t
    :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))
  (use-package lsp-treemacs
    :ensure t
    :commands lsp-treemacs-errors-list)
  (add-hook `lsp-mode-hook `lsp-enable-which-key-integration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            Erlang,Elixir            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erlang
  :ensure  t
  :init
  ;;erlang_ls 来自 https://github.com/erlang-ls 下的erlang_ls
  (add-to-list `exec-path "~/Projects/erlang_ls/_build/default/bin")
  :config
  (require `lsp-mode)
  (add-hook `erlang-mode-hook `lsp))

(use-package elixir-mode
  :ensure t
  :init
  ;;elixir-ls 来自 https://github.com/elixir-lsp下的elixir-ls
  (add-to-list `exec-path "~/Projects/elixir-ls/release")
  :config
  (require `lsp-mode)
  (add-hook `elixir-mode-hook `lsp)
  (add-hook `elixir-mode-hook
	    (lambda () (add-hook 'before-save-hook `elixir-format nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;               Haskell               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode init-stack-exec-path-mode
    "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
    nil
    :lighter ""
    :global nil
    (if init-stack-exec-path-mode
	(when (and (executable-find "stack")
		   (locate-dominating-file default-directory "stack.yaml"))
	  (let ((stack-path (append
			     (list (concat (string-trim-right (shell-command-to-string "stack path --local-install-root")) "/bin"))
			     (parse-colon-path (replace-regexp-in-string "[\r\n]+\\'" "" (shell-command-to-string "stack path --bin-path"))))))
	    (setq-local exec-path (seq-uniq stack-path 'string-equal))
	    (make-local-variable `process-environment)
	    (setenv "PATH" (string-join exec-path path-separator))))
      (kill-local-variable `exec-path)
      (kill-local-variable `process-environment)))
;; (use-package haskell-mode
;;   :ensure t
;;   :diminish (interactive-haskell-mode . "" )
;;   :config
;;   ;; About Haskell-Cabal-Mode
;;   ;;  Edit the .cabal File
;;   (add-hook `haskell-cabal-mode-hook `init-stack-exec-path-mode)
;;   (add-hook `haskell-cabal-mode-hook `subword-mode)
;;   (add-hook `haskell-cabal-mode-hook (lambda () (setq mode-name "")))
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-p") `haskell-compile)
;;   (custom-set-variables `(haskell-compile-ignore-cabal t))
;;   ;; Hakell-Mode
;;   (init-add-auto-mode `haskell-mode "\\.ghci\\'")
;;   (add-hook `haskell-mode-hook `init-stack-exec-path-mode)
;;   (add-hook `haskell-mode-hook `subword-mode)
;;   (add-hook `haskell-mode-hook (lambda () (setq mode-name "")))
;;   (add-hook `haskell-mode-hook `eldoc-mode)
;;   (add-hook `haskell-mode-hook `rainbow-delimiters-mode)
;;   (add-hook `haskell-mode-hook `haskell-decl-scan-mode) ;;; C-M-a C-M-e C-M-h
;;   (add-hook `haskell-mode-hook `haskell-auto-insert-module-template)
;;   (add-hook `haskell-mode-hook `flycheck-mode)

;;   (define-key haskell-mode-map (kbd "C-c C-h") `haskell-hoogle)
;;   (define-key haskell-mode-map (kbd "C-o")   `open-line)
;;   (define-key haskell-mode-map (kbd "C-c C-p") `haskell-compile)

;;   (push `haskell-mode page-break-lines-modes) ;; page-break-lines

;;   ;; hindent-mode
;;   ;; 需要系统按装hindent
;;   (use-package hindent
;;     :ensure t
;;     :diminish hindent-mode
;;     :config
;;     (add-hook `haskell-mode-hook `hindent-mode)
;;     (when (require `nadvice)
;;       (defun  init-haskell-hindent--before-save-wrapper (oldfun &rest args)
;; 	(with-demoted-errors "Error invoking hindent: %s"
;; 	  (let ((debug-on-error nil))
;; 	    (apply oldfun args))))
;;       (advice-add `hindent--before-save :around `init-haskell-hindent--before-save-wrapper)))

;;   ;; Indentation.
;;   ;; Haskell Mode ships with two indentation modes:
;;   ;;      -- haskell-indention-mode
;;   ;;      -- haskell-indent-mode  --Deprecated!!
;;   (add-hook `haskell-mode-hook `haskell-indentation-mode)

;;   ;; 其实禁用了 haskell-mode 定义的几个checker.
;;   ;; haskell-hlint is shipped with  flycheck!
;;   ;; But hlint should be installed in the os system!
;;   (require `flycheck)
;;   (add-hook `haskell-mode-hook (lambda () (flycheck-select-checker `haskell-hlint)))


;;   ;; Using external formatters. Stylish-haskell should be intsalled!
;;   (custom-set-variables `(haskell-stylish-on-save t))
;;   (custom-set-variables `(haskell-tags-on-save t))
;;   (custom-set-variables `(haskell-compile-ignore-cabal t))
;;   (define-key haskell-mode-map (kbd "M-.") `haskell-mode-jump-to-def-or-tag)

;;   ;; interactive-haskell-mode 是个子模式,
;;   ;; 负责haskell-mode如何与主模式haskell-interactive-mode交互.
;;   ;; 妈的,才明白!
;;   ;;(require `interactive-haskell-mode)
;;   (require `haskell)
;;   ;(diminish `interactive-haskell-mode "")
;;   (add-hook `haskell-mode-hook `interactive-haskell-mode)
;;   (setq flycheck-check-syntax-automatically `(save mode-enabled)))

;;-----------------------------------------------------------------------------
(use-package haskell-mode
  :ensure t
  :config
  ;; Only use the stack project
  (custom-set-variables `(haskell-compile-ignore-cabal t))
  ;; About Haskell-Cabal-Mode
  ;;  Edit the .cabal File
  (add-hook `haskell-cabal-mode-hook `subword-mode)
  ;; (add-hook `haskell-cabal-mode-hook (lambda () (setq mode-name "")))
  (define-key haskell-cabal-mode-map (kbd "C-c C-p") `haskell-compile)
  ;; Hakell-Mode
  (init-add-auto-mode `haskell-mode "\\.ghci\\'")
  (add-hook `haskell-mode-hook `haskell-auto-insert-module-template)
  (define-key haskell-mode-map (kbd "C-c C-p") `haskell-compile)
  (add-hook `haskell-mode-hook `haskell-indentation-mode)
  (add-hook `haskell-mode-hook `rainbow-delimiters-mode)
  (add-hook `haskell-mode-hook `subword-mode)
  ;; (add-hook `haskell-mode-hook (lambda () (setq mode-name "")))
  ;; 关注 interactive-haskell-mode 和 lsp-mode 是否会冲突
  (add-hook `haskell-mode-hook `interactive-haskell-mode)
  (use-package hindent
    :ensure t
    :diminish ""
    :config
    (add-hook `haskell-mode-hook `hindent-mode))
  ;; 需要安装haskell-ide-engine
  (use-package lsp-haskell
    :ensure t
    :config
    ;; For Haskell-language-server
    (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
    ;; For ghcide
    ;; (setq lsp-haskell-process-path-hie "ghcide")
    ;; (setq lsp-haskell-process-args-hie `())
    ;; Comment/uncomment this line to see interactions between lsp client/server.
    ;; (setq lsp-log-io t)
    (add-hook `haskell-mode-hook `lsp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                 Rust                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t
  :config
  ;;(add-hook `rust-mode-hook (lambda () (setq mode-name "")))
  (require `company)
  (define-key rust-mode-map (kbd "TAB") `company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq rust-format-on-save t)
  (use-package toml-mode
    :ensure t
    :config
    (add-hook `toml-mode-hook `goto-address-prog-mode))
  (use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :config
    (add-hook `rust-mode-hook `cargo-minor-mode))
  ;; lsp-mode 和 racer-mode 是互斥的
  (require `lsp-mode)
  (add-hook `rust-mode-hook `lsp)
  ;; (use-package racer
  ;;   :ensure t
  ;;   :diminish racer-mode
  ;;   :config
  ;;   (add-hook `rust-mode-hook `racer-mode)
  ;;   (add-hook `racer-mode-hook `eldoc-mode)
  ;;   (add-hook `racer-mode-hook `company-mode))
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook `flycheck-mode-hook `flycheck-rust-setup)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;                Python               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :ensure t
  :config
  (use-package pyvenv
    :ensure t)
  (use-package pipenv
    :ensure t)
  (use-package conda
    :ensure t
    :config
    (setq  conda-anaconda-home (expand-file-name "~/anaconda3/"))
    (setq conda-env-home-directory (expand-file-name "~/anaconda3/"))
    ;;
    (defun init-conda-env-shell-init (orig_fun &rest args)
      "Activate the current env in a newly opened shell PROCESS."
      (let* ((process (car args))
	     (activate-command (if (eq system-type 'windows-nt)
				   '("activate")
				 '("conda" "activate")))
	     (full-command (append activate-command `(,conda-env-current-name "\n")))
	     (command-string (combine-and-quote-strings full-command)))
	(comint-send-string process command-string)))
    (advice-add `conda-env-shell-init :around `init-conda-env-shell-init)
    (global-set-key (kbd "s-c .") `conda-env-activate)
    (global-set-key (kbd "s-c ,") `conda-env-deactivate)
    ;;
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell))
  ;;
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list `python-shell-completion-native-disabled-interpreters "jupyter")
  ;;
  (defun init-python-mode-hook-setup ()
    "Nothing."
    (setq electric-indent-chars (delq ?: electric-indent-chars))
    (setq indent-tabs-mode nil)
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    ;; (setq mode-name "")
    (setq flycheck-flake8-maximum-line-length 240))
  (add-hook `python-mode-hook `init-python-mode-hook-setup)
  ;; (use-package lsp-python-ms :ensure t)
  ;; ;; 注意lsp-pyls的优先级比lsp-python-ms高
  ;; ;; python 安装python-language-server包后,会自动用lsp-pyls.
  ;; ;; 注意python安装的jedi包的版本问题，这个似乎老在变
  ;; (require `lsp-mode)
  ;; (require `lsp-python-ms)
  (require `lsp-mode)
  (add-hook `python-mode-hook `lsp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;            CC-mode:C/C++/Object-C   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :config
  (use-package modern-cpp-font-lock
    :ensure t
    :config
    (modern-c++-font-lock-global-mode))
  (defun init-cc-c-mode-common-hook ()
    "Set my personal style for the current buffer."
    ;; --
    (setq c-basic-offset 4
	  tab-width 4
	  ;; this will make sure spaces are used instead of tabs
	  indent-tabs-mode nil)
    (subword-mode +1))
  (add-hook `c-mode-hook `init-cc-c-mode-common-hook)
  (add-hook `c++-mode-hook `init-cc-c-mode-common-hook)
  (add-hook `objc-mode-hook `init-cc-c-mode-common-hook)
  (add-hook `c-mode-common-hook `rainbow-delimiters-mode)

  (use-package cmake-mode
    :ensure t
    :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	   ("\\.cmake\\'" . cmake-mode))
    :config
    (add-hook `cmake-mode-hook (lambda() (add-to-list (make-local-variable `company-backends) `company-cmake))))

  ;; Use lsp, auto fall back from ccls to clangd
  ;; 有个概念要清楚：compilation database和文件compile_commands.json对很多工具的辅助作用！
  ;; 我的方法是用CMAKE来生成它。
  ;; 1） 在 ProjectRoot/CMakeList.txt 中包含： set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  ;; 2)  在 build/ 运行: cmake ..
  ;; 3)  在 ProjectRoot/ 运行: ln -s build/compile_commands.json compile_commands.json 建立符号链接
  ;; 4)  在 ProjectRoot/.ccls 中 为ccls如常配置。
  ;; 即可。这样项目的目录清晰。用设置参数的方式，不是太好！
  (use-package ccls
    :ensure t
    :config
    (setq ccls-args `("--log-file=/tmp/ccls.log")))
  ;; (setq lsp-clients-clangd-args '("--compile-commands-dir=./build" "-background-index"))

  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (add-hook `c++-mode-hook `lsp)
  (add-hook `c-mode-hook `lsp)
  (add-hook `objc-mode-hook `lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;       JavaScript Development        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 牵涉到安装那个language-server。注意typescript-language-server是微软的tsserver的包装。
;; 一般我避开用微软的，尽管他们很好！
;;   $npm i -g javascript-typescript-langserver
;;   $npm i -g typescript-language-server
;; 另外，安装eslint。
;;   $npm i -g eslint
(use-package json-mode
  :ensure t
  :config
  (require 'lsp)
  (add-hook 'json-mode-hook 'lsp))
(use-package js
  :ensure t
  :config
  (setq-default js-indent-level 4)
  (require 'lsp)
  (add-hook 'js-mode-hook 'lsp))
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (require 'lsp)
  (add-hook 'typescript-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;           Web Development           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 需要安装html-tidy：$sudo pacman -S tidy
;; 需要安装css-stylelint的后端： $npm install -g stylelint
;;                             $npm install -g stylelint-config-recommended
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.vue\\'" "\\.jsx\\'")
  :config
  ;;
  (use-package company-web :ensure t)
  ;;
  (defun init-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-css-colorization t))
  ;;
  (defun init-web-html-setup ()
    "My web setups!"
    ;; (setq mode-name "")
    (flycheck-add-mode 'html-tidy 'web-mode)
    (flycheck-select-checker 'html-tidy)
    (add-to-list (make-local-variable 'company-backends)
		 '(company-web-html company-files)))
  ;;
  (defun init-web-vue-setup ()
    "Setup for web-mode vue files."
    ;; (setq mode-name "")
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-select-checker 'javascript-eslint)
    (add-to-list (make-local-variable 'company-backends)
		 '(company-web-html company-files company-css)))
  ;;
  (add-hook 'web-mode-hook 'init-web-mode-hook)
  (add-hook 'web-mode-hook (lambda () (cond ((string-equal "html" (file-name-extension buffer-file-name))
					(init-web-html-setup))
				       ((string-equal "vue" (file-name-extension buffer-file-name))
					(init-vue-mode-hook))))))
(use-package css-eldoc :ensure t)
(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (setq-default css-indent-offset 4)
  ;; (setq flycheck-stylelintrc "~/.stylelintrc")
  (setq-default css-indent-offset 4)
  (add-to-list (make-local-variable 'company-backends) '(company-css company-files company-capf company-dabbrev))
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))
(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" "\\.sass\\'"))
(use-package emmet-mode
  :ensure t
  :config
  (dolist (hook '(web-mode-hook sgml-mode-hook css-mode-hook scss-mode-hook js2-mode-hook))
    (add-hook hook 'emmet-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;           Structureed Doc.          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\.yml\\.erb\\'"
  :hook (yaml-mode . goto-address-prog-mode))
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :init
  (push `markdown-mode whitespace-cleanup-mode-ignore-modes))
(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")
(use-package csv-mode
  :ensure t
  :mode  "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))
(use-package terraform-mode
  :ensure t
  :config
  (use-package company-terraform
    :ensure t
    :config
    (company-terraform-init)))

(use-package org
  :ensure t
  :config
  (setq org-hide-emphasis-markers t
	org-startup-indented t)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . nil)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t))))




(provide 'init)
;;; init.el ends here

