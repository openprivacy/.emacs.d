(require 'cider)

;; cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
; hide the *nrepl-connection* and *nrepl-server* buffers from appearing
(setq nrepl-hide-special-buffers t)
; Stop the error buffer from popping up while working in buffers other than the REPL:
(setq cider-popup-stacktraces t)
; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)
; To auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
; To adjust the maximum number of items kept in the REPL history:
(setq cider-repl-history-size 1000) ; the default is 500
; To make the REPL history wrap around when its end is reached:
(setq cider-repl-wrap-history t)
; Limit the number of items of each collection the printer will print to 100:
(setq cider-repl-print-length 100) ; the default is nil, no limit

;; minor modes
; Enable CamelCase support
(add-hook 'cider-repl-mode-hook 'subword-mode)
; The use of paredit when editing Clojure (or any other Lisp) code is highly recommended
(add-hook 'cider-repl-mode-hook 'paredit-mode)
; RainbowDelimiters is a minor mode which highlights parentheses, brackets,
; and braces according to their depth.
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; unused (for now)
; Prevent the auto-display of the REPL buffer in a separate window
; after connection is established:
; (setq cider-repl-pop-to-buffer-on-connect nil)
; Make C-c C-z switch to the CIDER REPL buffer in the current window:
; (setq cider-repl-display-in-current-window t)
; Change the result prefix for REPL evaluation (by default there's no prefix):
; (set cider-repl-result-prefix ";; => ")

(provide 'my-clojure)
