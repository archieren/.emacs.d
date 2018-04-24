;;; init-crontab --- Nothing.
;;; Commentary:
;;; Code:

(require 'init-utils)
(require 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(provide 'init-crontab)
;;; init-crontab ends here
