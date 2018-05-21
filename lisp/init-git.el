;;; init-git --- Nothing
;;; Commentary:
;; diff-hl:
;; git-gutter:
;;; Code:
;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require 'diminish)

(require 'diff-hl)
(require 'magit)
(require 'compile)
(require 'vc)
(require 'git-gutter)
(require 'git-messenger)
(require 'init-utils)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'after-init-hook 'global-diff-hl-mode)

(with-eval-after-load 'diff-hl
  (define-key diff-hl-mode-map
    (kbd "<left-fringe> <mouse-1>")
    'diff-hl-diff-goto-hunk))
;;}}

(setq-default magit-diff-refine-hunk t)

;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (add-hook 'magit-popup-mode-hook 'init-whitespace-no-trailing-whitespace))

(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))


(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))



;;; git-svn support

;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))

(with-eval-after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands.")
(defun git-svn--available-commands ()
  "Nothing."
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "In DIR, run  a git COMMAND."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))



;; Though see also vc-annotate's "n" & "p" bindings
(with-eval-after-load 'vc
  (setq git-messenger:show-detail t)
  (define-key vc-prefix-map (kbd "p") #'git-messenger:popup-message))

;;; Github
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; {{ git-gutter
(require 'git-gutter)
(diminish 'git-gutter-mode)
(defun git-gutter-reset-to-head-parent()
  (interactive)
  (let (parent (filename (buffer-file-name)))
    (if (eq git-gutter:vcs-type 'svn)
        (setq parent "PREV")
      (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty='format:%H' " filename)) "^") "HEAD^")))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))

(defun git-gutter-reset-to-default ()
  "Nothing."
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))


;; If you enable global minor mode
(global-git-gutter-mode t)

;; nobody use bzr
;; people are forced use subversion or hg, so they take priority
(custom-set-variables '(git-gutter:handled-backends '(svn hg git)))

(git-gutter:linum-setup)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; }}
(provide 'init-git)
;;; init-git ends here
