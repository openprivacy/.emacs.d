;; Package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; My personal keybindings
(defvar bind-fens-keys t)               ; to get some extra key bindings
(require 'fen)                          ; handy editor functions

;; Drupal support
(autoload 'geben "geben" "PHP Debugger on Emacs" t)
(autoload 'drupal-mode "drupal-mode" "Major mode for Drupal." t)

(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|php\\|inc\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

(add-hook 'drupal-mode-hook
          '(lambda nil
             ; "Drupal" coding standards obtained from most recent coder module:
             ; sudo cp -al /path/to/coder/coder_sniffer/Drupal \
             ;             $(pear config-get php_dir)/PHP/CodeSniffer/Standards
             (setq-local flymake-phpcs-standard "Drupal")
             (local-set-key '[M-S-right] '(lambda nil (interactive) (flymake-phpcs-load)))
             (local-set-key '[M-S-up] 'flymake-goto-prev-error)
             (local-set-key '[M-S-down] 'flymake-goto-next-error)
             (local-set-key "\C-hf" 'drupal-browse-api)
             (local-set-key "\C-hp" 'php-symbol-lookup)))

;; Clojure, Cider, Nrepl
(load "clojure")

;; NXML-mode for xdi and xsd files
(add-to-list 'auto-mode-alist '("\\.\\(xdi\\|xsd\\)$" . nxml-mode))

;; tell emacs I know what I'm doing
(put 'eval-expression  'disabled nil)    ; convenience for elisp hackers
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; set some variables
(setq-default indent-tabs-mode nil)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(tool-bar-mode 0)
(setq require-final-newline t
      backup-by-copying-when-linked t
      split-height-threshold 20
      scroll-step 2
      inhibit-startup-message t)

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; stuff for calendar
(setq calendar-latitude [40 21 north]
      calendar-longitude [80 2 west]
      calendar-location-name "115 Roycroft, Mt. Lebanon")

(if (boundp 'x-display-name)
    (setq frame-title-format
          '((buffer-file-name "%f" (dired-directory dired-directory "%b")) " - "
            invocation-name "@" system-name)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(comment-column 48)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes (quote ("8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" default)))
 '(fci-rule-color "#383838")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
