;;; perl-quote.el --- helpers for Perl quoted strings

;; Copyright 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 2
;; Keywords: languages
;; URL: http://www.geocities.com/user42_kevin/perl-quote/index.html
;; EmacsWiki: PerlLanguage

;; perl-quote.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; perl-quote.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This spot of code converts Perl single-quote '' strings to "", or back
;; again.  q{} and qq{} styles are supported, as are Locale::TextDomain
;; style __"" and N__"" translations.
;;
;; Put point at the start of the string to convert and use
;; `perl-quote-single' to make it a single '', or `perl-quote-double' to
;; make it a double.  The suggested key bindings are C-c ' and C-c "
;; respectively, which `perl-quote-keybindings' below can install.  See the
;; docstrings for more.
;;
;; Note that both quote functions use `forward-sexp' to find the end of
;; string, so if you're not in perl-mode or cperl-mode then the mode will
;; have to understand perl strings well enough for `forward-sexp' to go to
;; the right point.  There's no real checking of that, but for interactive
;; use you can easily see if it came out right.
;;

;;; Install
;;
;; Put perl-quote.el in one of your `load-path' directories and add to your
;; .emacs add
;;
;;     (autoload 'perl-quote-keybindings "perl-quote")
;;     (add-hook 'perl-mode-hook  'perl-quote-keybindings)
;;     (add-hook 'cperl-mode-hook 'perl-quote-keybindings)
;;
;; Or whichever perl-like modes you use, including maybe `pod-mode-hook' if
;; you write perl code samples in POD files.
;;
;; There's autoload cookies for the functions and likely hook custom
;; options, if you know how to use `update-file-autoloads' and friends.

;;; History:
;;
;; Version 2 - don't move point
;; Version 1 - the first version


;;; Code:

(defun perl-quote-backslashing (regexp alist)
  ;; explicit delete/insert to cope with incompatible `replace-match' in
  ;; xemacs
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (when (eq 0 (logand 1 (length (match-string 1)))) ;; or `evenp' in cl.el
      (let ((replacement (cdr (assoc (match-string 2) alist))))
        (goto-char (match-beginning 2))
        (delete-region (match-beginning 2) (match-end 2))
        (insert replacement)))))

;;;###autoload
(defun perl-quote-single (&optional force)
  "Convert a Perl \"\" double-quoted string to '' single-quotes.
The forms converted are

    \"\"     ->  ''
    qq{}   ->  q{}
    qq[]   ->  q[]
    qq()   ->  q()
    qq<>   ->  q<>
    __\"\"   ->  __('')
    N__\"\"  ->  N__('')

Point must be at the start of the string, ie. the \" or q, or the
start of the __ or N__.

Parens are added for __ and N__ because __'' doesn't work, a
quote there is the old-style package name separator, it must be
__('').

Single-quotes in the string are escaped as \\' for the new string
and the following backslash forms in the string are converted to
literal characters

    \\\"   double-quote (no longer needs backslashing)
    \\t   tab
    \\n   newline
    \\r   carriage-return
    \\f   form-feed
    \\b   backspace
    \\a   bell (alert)
    \\e   escape (0x1B)

Control characters in your sources probably aren't a great idea,
but at least it gives a conversion.  However remember Emacs will
in fact save a newline in the buffer as CRLF or CR under DOS or
Mac coding systems.

If there's other backslash forms or variable interpolations in
the string then the buffer is unchanged and an error is thrown
with point at the first offending form.  A \\[universal-argument] prefix forces
conversion.  Either way you'll have to edit to turn
interpolations etc into plain text or expression.

The intention is to use `perl-quote-single' mainly on
double-quoted strings without variable interpolations, meaning
ones which don't need to be double-quotes and you'd prefer to be
singles.

See `perl-quote-double' (\\[perl-quote-double]) for the converse
conversion to double."

  (interactive "P")
  (let ((orig-point (point))
        (add-parens (and (looking-at "N?__")
                         (goto-char (match-end 0))))
        (beg (point))
        (qq  nil))
    (or (looking-at "\"\\|\\(qq[{[{<]\\)")
        (error "Not a double-quoted string"))
    (setq qq (match-beginning 1))  ;; nil for "", number for qq{}

    (forward-sexp (if qq 2 1)) ;; to after string
    (let ((end (point)))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (unless force
          (when (re-search-forward "\\$\\|\\\\[^\"tnrfbae]" nil t)
            (goto-char (match-beginning 0))
            (error "Interpolation or backslash in string")))

        (if qq
            (progn
              (delete-region beg (1+ beg)) ;; qq{} becomes q{}
              (goto-char (1+ (point))))

          (when add-parens
            (insert "(")
            (goto-char (point-max))
            (insert ")")
            (narrow-to-region (1+ (point-min)) (1- (point))))

          ;; closing " becomes '
          (delete-region (1- (point-max)) (point-max))
          (goto-char (point-max))
          (insert "'")

          ;; opening " becomes '
          (delete-region (point-min) (1+ (point-min)))
          (goto-char (point-min))
          (insert "'"))

        ;; without surrounding quotes
        (narrow-to-region (point) (1- (point-max)))

        (perl-quote-backslashing "\\(\\\\*\\)\\('\\|\\\\[tnrfbae\"]\\)"
                                 '(("\\\"" . "\"")
                                   ("'"    . "\\'")
                                   ;; see "Quote and Quote-like Operators"
                                   ;; in the perlop man page
                                   ("\\t"  . "\t")
                                   ("\\n"  . "\n")
                                   ("\\r"  . "\r")
                                   ("\\f"  . "\f")
                                   ("\\b"  . "\b")
                                   ("\\a"  . "\a")
                                   ("\\e"  . "\e")))))
    ;; original point, but allowing errors to reposition
    (goto-char orig-point)))

;;;###autoload
(defun perl-quote-double ()
  "Convert a Perl '' single-quoted string to \"\" double-quotes.
The forms converted are

    ''   ->  \"\"
    q{}  ->  qq{}
    q[]  ->  qq[]
    q()  ->  qq()
    q<>  ->  qq<>

Point must be at the start of the string, ie. the ' or q.

Backslashing in the string is amended for the new double-quote
form

    \\' -> '   single-quote no longer needs backslashing
    \"  -> \\\"  double-quote must be backslashed

See `perl-quote-single' (\\[perl-quote-single]) for the converse
conversion to single."
  (interactive)
  (let ((beg (point))
        (q  nil))
    (or (looking-at "'\\|\\(q[{[{<]\\)")
        (error "Not a single-quoted string"))
    (setq q (match-beginning 1))  ;; nil for ' number for q{

    (save-excursion
      (forward-sexp)
      (let ((end (point)))
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))

          (if q
              (insert "q") ;; q{} becomes qq{}

            (goto-char (1- (point-max)))
            (delete-region (1- (point-max)) (point-max))
            (insert "\"")

            (delete-region (point-min) (1+ (point-min)))
            (goto-char (point-min))
            (insert "\"")

            (narrow-to-region (point) (1- (point-max))))

          ;; \' becomes '
          ;; "  becomes \"
          (perl-quote-backslashing "\\(\\\\*\\)\\(\"\\|\\\\'\\)"
                                   '(("\\\'" . "'")
                                     ("\""   . "\\\""))))))))

;;;###autoload
(defun perl-quote-keybindings ()
  "Bind keys C-c ' and C-c \" to toggle string quote types.
This is designed for use from a major mode hook.  It makes the
following key bindings in the major mode keymap
\(`current-local-map')

    C-c '     perl-quote-single
    C-c \"     perl-quote-single

These are the suggested perl-quote.el keys, but of course you can
use anything you prefer."

  (interactive)
  (define-key (current-local-map) [?\C-c ?']  'perl-quote-single)
  (define-key (current-local-map) [?\C-c ?\"] 'perl-quote-double))

;;;###autoload
(custom-add-option 'perl-mode-hook 'perl-quote-keybindings)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'perl-quote-keybindings)
;;;###autoload
(custom-add-option 'pod-mode-hook 'perl-quote-keybindings)

(provide 'perl-quote)

;;; perl-quote.el ends here
