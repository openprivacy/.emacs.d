;;; @file fen.el
;;; Some handy functions and their key bindings
;;; MODIFIED    (MM/DD/YY)
;;;      fen	4/ 6/85 - Created.
;;;      fen	2/15/93 - ported to General Magic
;;;      fen    5/10/11 - mash-up from my-fns & my-keys
;;;      fen    2/14/14 - moved from ~/elisp/ to ~/.emacs.d/site-lisp/

;; Ask before killing emacs
(setq kill-emacs-query-functions '(lambda nil (yes-or-no-p "Really kill emacs? ")))

;; Give me a small window for keeping track of things
(defun split-window-small ()
  "Bound to \\[split-window-small]  mnemonic: 'eight-line-window'"
  (interactive)
  (let ((size (window-height (selected-window))))
    (if (< size 12)
	(split-window)
      (split-window nil (max 6 (/ size 7))))))

;; Transpose this window with the next one
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; insert a time stamp and name
(defun stamp ()		; fen 11/12/85 - 2011-05-10 fen
  "Insert at dot a short form of the date and user login useful for comments."
  (interactive)
  (insert (format "%s %s"
		  (format-time-string "%Y-%m-%d")
		  (user-real-login-name))))

;; key bindings
(if (and (boundp 'bind-fens-keys) bind-fens-keys)
    (progn
      ;; key bindings for my functions above
      (global-set-key "\C-x8" 'split-window-small)
      (define-key ctl-x-4-map (kbd "t") 'transpose-windows)
      (global-set-key (kbd "M-s M-t") 'stamp)

      ;; other non-standard key bindings
      (global-set-key "\er"   'replace-string)             ;was: move-to-window-line
      (global-set-key "\C-xn" 'next-multiframe-window)     ;was: narrow-to-{defun,region,page}
      (global-set-key "\C-xp" 'previous-multiframe-window)

      ;; for the safety of naive users (or sloppy typists like me)
      (global-unset-key "\C-xd")                           ;was: dired
      (global-unset-key "\C-x\C-l")                        ;was: downcase-region
      (global-unset-key "\C-x\C-u")                        ;was: upcase-region

      ;; move to first or last line in the window
      (global-set-key "\C-x," '(lambda nil (interactive) (move-to-window-line 0)))
      (global-set-key "\C-x." '(lambda nil (interactive)   ;was: set-fill-prefix
                                 (move-to-window-line -1)
                                 (end-of-line)))

      ;; transpose-chars and transpose-words *before* point
      (global-set-key "\C-t"  '(lambda nil (interactive)
                                 (transpose-subr 'forward-char -1)
                                 (forward-char 1)))
      (global-set-key "\et"   '(lambda nil (interactive)
                                 (transpose-subr 'forward-word -1)
                                 (forward-word 1)))
      )
)

(provide 'fen)
