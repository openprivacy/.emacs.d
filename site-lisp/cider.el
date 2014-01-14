(require 'cider)
;; http://ianeslick.com/2013/05/17/clojure-debugging-13-emacs-nrepl-and-ritz/
;; https://github.com/clojure-emacs/cider#installation
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
; Prevent the auto-display of the REPL buffer in a separate window
; after connection is established:
(setq cider-repl-pop-to-buffer-on-connect nil)
; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window t)
; Change the result prefix for REPL evaluation (by default there's no prefix):
(set cider-repl-result-prefix ";; => ")
