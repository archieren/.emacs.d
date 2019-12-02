;;; init-erlang --- Nothing.
;;; Commentary:
;;; Code:
(require 'projectile)
(require 'erlang-start)

(require 'elixir-mode)
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook
          (lambda () (if (projectile-project-p)
                    (setq elixir-format-arguments (list "--dot-formatter" (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                  (setq elixir-format-arguments nil))))

(provide 'init-erlang)
;;; init-erlang ends here
