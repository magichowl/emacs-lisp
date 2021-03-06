;;; imenu-go.el - Go to definition of item using imenu and tags

;; Copyright (C) 1995  Ilya Zakharevich

;; Author: Ilya Zakharevich <ilya@math.mps.ohio-state.edu>

;; This file is not a part of GNU Emacs, but is made available under
;; the same conditions.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary ============================================================

;;; To use this package add 

;;; (autoload 'imenu-go-find-at-position "imenu-go"
;;;  "Go to the definition of the current word." t) 
;;; (autoload 'imenu-go--back "imenu-go"
;;;  "Return back to a position saved during `imenu-go-find-at-position'." t) 

;;; and bindings like

;;; (global-set-key [M-S-mouse-2] 'imenu-go-find-at-position)
;;; (global-set-key "\e\"" 'imenu-go-find-at-position)
;;; (global-set-key [M-S-C-mouse-2] 'imenu-go--back)
;;; ;(global-set-key "\e'" 'imenu-go--back)  ; Conflicts with defined key.
;;; (global-set-key [?\C-\"] 'imenu-go--back)

;;; to your .emacs file. The usability if this package decreases a lot
;;; unless you have a simple access to `imenu', like in

;;; (global-set-key [M-S-down-mouse-3] 'imenu)

;;; To cache information about interesting places you should either
;;; run `imenu' in the interesting buffers, or run `etags *.c *.h' (or
;;; whatever) on interesting files. After this calling
;;; `imenu-go-find-at-position' when the cursor or pointer is over the
;;; interesting word will warp you to the definition of this word. You
;;; can unwind this warping by doing `imenu-go--back'.

;;; Alternately, for Emacs-Lisp hacking you may install package
;;; `find-function', which will be automatically used in Emacs Lisp mode:
;;;      (autoload 'find-function "find-func" nil t)


;;; Part of functionality of this package belongs to imenu.el, but is
;;; not there.

;;;; Changes:
;;; Now `pop-to-window' with `pop-up-windows' set to nil instead of 
;;; `switch-to-window'.
;;; Search is made case-sensitive. Minor bugs corrected.
;;;; 08/97:
;;; Bug in `let': "will not restore local variables if buffer is changed" 
;;;  circumvented.
;;; Will use `find-function' if present/autoloadable.
;;;; 09/97:
;;; Will use `find-function' in `debugger-mode' and `lisp-interaction-mode' as well.
;;; Workaround for a reported bug in `imenu--generic-function'.
;;;; 01/98:
;;; intern-soft may be not enough to run (symbol-function (intern default)).
;;;; 02/98
;;; Use `find-function' in help-buffer too.
(require 'imenu)

(or (fboundp 'point-to-mouse-maybe)	; Is defsubst fboundp-ing?
(defsubst point-to-mouse-maybe ()
  "Moves point to the position of the click if the last event is click.
Intended for use in commands that can be bound both to mouse and keyboard."
  (and (listp last-input-event) (mouse-set-point last-input-event)))
    )

(defvar imenu---can-seen)		; Keeps seen alists to avoid recursion

(defun imenu---can-find (str)
  "Check whether the string STR is known to `imenu'."
  (let (imenu---can-seen)
    (and (boundp 'imenu--index-alist) imenu--index-alist
	 (imenu---in-alist str imenu--index-alist))))

(defun imenu---can-find-all-buffers (str)
  "Check whether the string STR is known to `imenu' in some buffers."
  (let ((blist (buffer-list)) buffer imenu---can-seen done)
    (save-excursion
      (while (and (not done) blist)
	(setq buffer (car blist) blist (cdr blist))
	(set-buffer buffer)
	(setq done (imenu---can-find str))))
    (if done (list buffer done))))

(defun imenu---in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res prob-res (initial alist))
    (while alist
      (setq elt (car alist) alist (cdr alist) head (car elt) tail (cdr elt))
      (if head
	  (if (string= str head) (setq alist nil res tail)
	    (and (listp tail)
		 ;; Avoid recursion
		 (setq prob-res (or (if (memq tail imenu---can-seen) nil
				      (setq imenu---can-seen 
					    (cons tail imenu---can-seen))
				      (imenu---in-alist str tail))
				    prob-res)))
	    (or prob-res 
		(if (string-match (concat "\\<" (regexp-quote str) "\\>") head)
		    (setq prob-res tail))))))
      (or res prob-res)))

(defvar imenu---stack nil "List of positions to return back to later.")

;;;###autoload
(defun imenu-go-find-at-position (&optional default)
  "Go to a definition of the word under cursor or pointer.
Tries to find `imenu'-information on the current word, if cannot,
tries to find it as an Emacs function (in Emacs Lisp mode, and if
`find-function' is available), if cannot, falls back to tags search.
Saves the position to allow return back by `imenu-go--back'.  Can be
bound both to mouse and keyboard event.  Will show found buffer in a
different window unless the current window is the only one in the
frame."
  (interactive)
  (require 'etags)
  (point-to-mouse-maybe)
  (or default (setq default 
		    (funcall (or find-tag-default-function
				 (get major-mode 'find-tag-default-function)
				 'find-tag-default))))
  (let* ((pos (imenu---can-find-all-buffers default))
	 (pop-up-windows nil) 
	 ;; `let' does not work across buffer change for local variables
	 (old-c-f-s case-fold-search)
	 (b (current-buffer)))
    (setq imenu---stack (cons (point-marker) imenu---stack))
    (if pos (progn
	      (pop-to-buffer (car pos))
	      (goto-char (nth 1 pos)))
      (if (and (or
		(memq major-mode 
		      '(emacs-lisp-mode debugger-mode lisp-interaction-mode))
		(member (buffer-name) '("*Messages*" "*Help*" "*scratch*")))
	       (fboundp 'find-function)
	       (intern-soft default)
	       (fboundp (intern default))
	       (symbol-function (intern default))
	       (not (subrp (symbol-function (intern default))))
	       (condition-case nil
		   ;; find-function always returns nil
		   (let ((orig-point (point))
			 (orig-buf (window-buffer)))
		    (find-function (intern default))
		    (not (and (eq (point) orig-point)
			      (eq (window-buffer) orig-buf))))
		 (error nil)))
	  nil
	(setq case-fold-search nil)
	(find-tag default)
	(save-excursion
	  (set-buffer b)
	  (setq case-fold-search old-c-f-s))))))

;;;###autoload
(defun imenu-go--back ()
  "Return back to a position saved during `imenu-go-find-at-position'."
  (interactive)
  (if (null imenu---stack)
      (error "No previous locations recorded"))
  (let ((marker (car imenu---stack)))
    ;; Pop the stack.
    (setq imenu---stack (cdr imenu---stack))
    (prog1
	;; Move to the saved location.
	(switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      ;; Kill that marker so it doesn't slow down editing.
      (set-marker marker nil nil))))

(provide 'imenu-go)
