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
(autoload 'drupal-mode "drupal-mode" "Major mode for Drupal." t)

(add-to-list 'auto-mode-alist
             '("\\.\\(module\\|test\\|install\\|theme\\|php\\|inc\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

(add-hook 'drupal-mode-hook
          '(lambda nil
             ; "Drupal" coding standards obtained from most recent coder module:
             ; sudo cp -a /path/to/coder/coder_sniffer/Drupal \
             ;            $(pear config-get php_dir)/PHP/CodeSniffer/Standards
             (setq-local flymake-phpcs-standard "Drupal")
             (local-set-key '[M-S-right]
                            '(lambda nil (interactive)
                               (flymake-phpcs-load)
                               (custom-set-variables         ;Put error in mini-buffer
                                '(help-at-pt-timer-delay 0.9)
                                '(help-at-pt-display-when-idle '(flymake-overlay)))))
             (local-set-key '[M-S-up]    'flymake-goto-prev-error)
             (local-set-key '[M-S-down]  'flymake-goto-next-error)
             (local-set-key "\C-c\C-p" 'php-search-documentation) ;also default "\C-c\C-f"
             (local-set-key "\C-c\C-d" 'drupal-browse-api))) ;was: c-hungry-delete-forward

;; Clojure, Cider, Nrepl
(load "clojure")

;; NXML-mode for xdi and xsd files
(add-to-list 'auto-mode-alist '("\\.\\(xdi\\|xsd\\)$" . nxml-mode))

;; Tell emacs I know what I'm doing
(tool-bar-mode 0)
(put 'eval-expression  'disabled nil)    ; convenience for elisp hackers
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; Set some variables
(setq split-height-threshold 20
      scroll-step 2
      inhibit-startup-message t)
(setq-default indent-tabs-mode nil
              sh-basic-offset 2
              sh-indentation 2
              require-final-newline t
              backup-by-copying-when-linked t)

;; Set my location for accurate Solstice/Equinox times
(setq calendar-latitude [40 21 north]
      calendar-longitude [80 2 west]
      calendar-location-name "115 Roycroft, Mt. Lebanon")

;; Set window title
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
