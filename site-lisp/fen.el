;;; Some handy functions and their key bindings
;;; $Id: my-fns.el,v 1.3 2001/08/12 08:23:53 fen Exp $
;;;    NAME
;;;      fen.el -
;;;    FUNCTION
;;;    NOTES
;;;    MODIFIED    (MM/DD/YY)
;;;      fen	     4/ 6/85 - Created.
;;;      fen	     2/15/93 - ported to General Magic
;;;      fen         5/10/11 - mash-up from my-fns & my-keys

;;;
;;; some special functions with optional key-bindings below
;;;

(setq kill-emacs-query-functions '(lambda nil (yes-or-no-p "Really kill emacs? ")))

(defun split-window-small ()
  "Bound to \\[split-window-small]  mnemonic: 'eight-line-window'"
  (interactive)
  (let ((size (window-height (selected-window))))
    (if (< size 12)
	(split-window)
      (split-window nil (max 6 (/ size 7))))))

;;;
;;; insert a time stamp and name
;;;
(defun stamp ()		; fen 11/12/85 - 2011-05-10 fen
  "Insert at dot a short form of the date and user login useful for comments."
  (interactive)
  (insert (format "%s %s"
		  (format-time-string "%Y-%m-%d")
		  (user-real-login-name))))

;;;
;;; key bindings
;;;
(if (and (boundp 'bind-fens-keys) bind-fens-keys)
    (progn
      ;; key bindings for my functions above
      (global-set-key "\C-x8" 'split-window-small)
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
