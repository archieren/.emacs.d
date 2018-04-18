;;; init-docker  --- Nothing.
;;; Commentary:
;; 暂时放在这儿，以后可能有用。
;;; Code:

(require-package 'docker)
(require-package 'dockerfile-mode)
(require-package 'docker-compose-mode)

(fullframe docker-images tablist-quit)
(fullframe docker-machines tablist-quit)
(fullframe docker-volumes tablist-quit)
(fullframe docker-networks tablist-quit)
(fullframe docker-containers tablist-quit)

(provide 'init-docker)
;;; init-docker ends here
