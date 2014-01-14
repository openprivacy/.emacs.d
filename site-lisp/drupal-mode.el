;;; @file drupal-mode.el
;;; Provide Drupal coding standards and a few other useful functions

(require 'php-mode)
(require 'etags)
(require 'flymake)

;; Remove php-mode from the auto-mode-alist
(setq-default auto-mode-alist (rassq-delete-all 'php-mode auto-mode-alist))

(defcustom drupal-api-version 7
        "Drupal API version to search"
        :type 'integer
        :group 'drupal)

(defcustom drupal-api-url "http://api.drupal.org/api/search"
        "URL for API search."
        :type 'string
        :group 'drupal)

;; Search Drupal API for function at point
(defun drupal-browse-api ()
  "Browse Drupal API docs."
  :group 'drupal
  (interactive)
  (browse-url
   (format "%s/%i/%s" drupal-api-url drupal-api-version (symbol-at-point))))

;; Define drupal-mode (derived from php-mode)
(define-derived-mode drupal-mode php-mode "Drupal"
  "Major mode for Drupal coding.\n\n\\{drupal-mode-map}"
  :group 'drupal
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  (run-mode-hooks 'drupal-mode-hook))

(provide 'drupal-mode)

;; CiviCRM mode deprecated; now using Drupal coding standards

;; (define-derived-mode civicrm-mode php-mode "CiviCRM"
;;   "Major mode for CiviCRM coding.\n\n\\{civicrm-mode-map}"
;;   (setq c-basic-offset 4)
;;   (setq indent-tabs-mode nil)
;;   (setq fill-column 78)
;;   (setq show-trailing-whitespace t)
;;   (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;   (c-set-offset 'case-label '+)
;;   (c-set-offset 'arglist-close 0)
;;   (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
;;   (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
;;   (run-hooks 'drupal-mode-hook))

;; (provide 'civicrm-mode)
