;; from http://drupal.org/node/59868#comment-1386940
;; from http://drupal.org/node/59868 ; 2011-05-10 fen

(require 'etags)
(require 'flymake)

(defcustom drupal-api-version 7
        "Drupal API version to search"
        :type 'integer
        :group 'drupal)

(defcustom drupal-api-url "http://api.drupal.org/api/search"
        "URL for API search."
        :type 'string
        :group 'drupal)

(defun drupal-browse-api ()
  "Browse Drupal API docs."
  :group 'drupal
  (interactive)
  (browse-url
   (format "%s/%i/%s" drupal-api-url drupal-api-version (symbol-at-point))))

(defun my-php-symbol-lookup ()
  :group 'drupal
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

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

(define-derived-mode civicrm-mode php-mode "CiviCRM"
  "Major mode for CiviCRM coding.\n\n\\{civicrm-mode-map}"
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  (run-hooks 'drupal-mode-hook)
)
(provide 'civicrm-mode)

;; (defconst my-php-style           ; correct arglist closing parenthesis
;;   '((c-offsets-alist . ((arglist-close . c-lineup-close-paren))))
;;   "My PHP Programming style"
;; )
;; (c-add-style "my-php-style" my-php-style)

;; (defun my-php-mode ()
;;   "My personal php-mode customizations"
;;   (c-set-style "my-php-style")
;;   ; More generic PHP customizations here
;;   (setq show-paren-mode t)
;; )

;; (defun my-php ()
;;   ; PHP
;;   (add-hook 'php-mode-hook 'my-php-mode)

;;   ; Drupal
;; (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode)) ; set drupal-mode manually
;; (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

;;   ; More startup-setup for PHP customizations to work here
;; )

;; (provide 'my-php)
;; (provide 'my-php-mode)
