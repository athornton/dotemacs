;;; Copyright (C) 1992  SAS Institute.
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 

;;;---------------------------------------------------------
;;; This file contains el code for PLI mode and C comment mode.
;;; If this file is too large for your mailer, it can be sent in
;;; two parts. The C comment file starts at about line 905.
;;;
;;;
;;; Electric PLI mode - takes care of indenting and flashing and filling
;;;   for PLI code.
;;;  file name: pli-mode.el
;;;  Author:   Mark Riggle / J4
;;;            AI Dept
;;;            SAS Institute Inc.
;;;            SAS Campus Dr.
;;;            Cary, NC 27513
;;;            plimsr@unx.pli.com
;;;
;;;   Inspired from the C-mode of GNU and the desperation of people
;;;     used to using a LISP environment needing to write SCL code.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, provided the people they give it to can and that this
;;; header remains with the file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Also see c-fill.el for where the fill mode came from.from.
;;;
;;;  ad the following lines to your .emacs file
;;;  (autoload  'pli-mode   "pli-mode")
;;;  (setq auto-mode-alist 
;;;     (append '(("\\.*pli" . pli-mode) ("\\.*scl" . pli-mode))
;;;             auto-mode-alist))

;;;---  Functions useful here are;
;;;
;;; pli-indent-command - bound to tab
;;; pli-indent-region
;;; pli-flash-open
;;; pli-labels-toggle-standard
;;; pli-big-pic
;;; pli-forward-function
;;; pli-backward-function
;;;
;;;  Bind the above functions to mode specific  keys
;;;   in the pli-mode-load-hook by something like this:
;;;  (but use function keys, change the ESC Control sequence)
;;;
;;;   (defun pli-mode-load-hook()
;;;     "set up PLI mode function keys"
;;;     (interactive)
;;;     (define-key pli-mode-map "\e\C-q" 'pli-indent-region)
;;;     (define-key pli-mode-map "\e\C-d" 'pli-forward-function)
;;;     (define-key pli-mode-map "\e\C-b" 'pli-backward-function)
;;;     (define-key pli-mode-map "\e\C-p" 'pli-big-pic)
;;;     (define-key pli-mode-map "\e\C-l" 'pli-flash-open)
;;;     (define-key pli-mode-map "\e\C-i" 'pli-indent-command)
;;;     (define-key pli-mode-map "\e\C-n" 'c-comment)
;;;     (define-key pli-mode-map "\e\C-c" 'exit-recursive-edit))
;;;
;;; If you are not sure what your function keys send as a string to
;;;  Emacs, see the function keyboard-read-key provided at the end of 
;;;  this file.
;;;
;;;
;;;---   important user variables are
;;; 
;;; pli-labels-standard
;;; pli-tab-always-indent
;;; pli-undent-end
;;; pli-continued-statement-offset
;;; pli-continue-to-indent
;;; pli-initial-indent
;;; pli-indent-level
;;; pli-flash-pause
;;; pli-flash-end
;;; pli-electric-semi
;;; pli-auto-newline
;;; pli-fast-scan


; put underscore as a word char
;;; ----------------- user vars -----------------

(defvar pli-auto-newline t
  "*Non-nil means TAB in PLI mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.")
(defvar pli-undent-end nil
  "*Controls where an end statement is indented to. T makes it indent at the
    do or select, nil indents the end as an inside statement.")

(defvar pli-tab-always-indent t "*make tab always indent")
(defvar pli-continued-statement-offset 3
  "*Indent for continued statements")
(defvar pli-initial-indent 3
  "*Indent for top level statements")
(defvar pli-preamble-indent 0
  "*Statements found to be in the preamble are indented this much")
(defvar pli-indent-level 3
  "*Indent after DO's and SELECTs.")
(defvar pli-labels-standard t
 "*Buffer-specific: If true then labels that are left justified are
   considered to be like subroutines, otherwise , use some indent.
   This will speed up the indenting process")
(make-variable-buffer-local 'pli-labels-standard)
(defvar pli-flash-pause 1 "*pause n seconds for flash")
(defvar pli-auto-newline t
  "*Insert new-line and indent after an electric ';'")
(defvar pli-electric-semi t
  "*Treat ';' as an indenter command also ans maybe add a newline,
    see pli-auto-newline")
(defvar pli-flash-end t
  "*Flash the opening DO or SELECT statement after an 'end;' is typed")
(defvar pli-labels-to-left nil
  "*Buffer-specific: if true then ALL labels are left justified, 
  if false then labels are not moved. 
  Both  pli-labels-to-left and pli-labels-standard may not be true.")
(defvar pli-fast-scan t
  "*If true then top level statements are set to pli-initial-indent,
  otherwise they are indented as the first indented statement is.")
(defvar pli-continue-to-indent nil
  "*For continued statements, indent each new line pli-continued-statement-offset more than the previous")
(defvar pli-minimize-indent nil
  "*If true then use real beginning of statement that contains 
    the DO or SELECT, otherwise use the line the DO or SELECT is on")
;;; --------- other vars ---------------

(defvar pli-indent-grps "" 
  "*statements that change indent level")

(setq pli-indent-grps
      "\\([% \t]do[ \n\t;]\\)\\|\\([ \t]+select[\(; \n\t]\\)\\|\\([% \t]end[ \t]*;\\)")

(defvar pli-string-delimiter "")
(setq pli-string-delimiter "\\([\'\"]\\)" )

(defvar pli-comment-end "" "end of a comment indicator")
(setq  pli-comment-end "\\(\\*/\\)" )
(setq pli-comment-begin "\\(/\\*\\)" )
(defvar pli-comments (concat pli-comment-begin "\\|" pli-comment-end))

(defvar pli-preamble-term ""
  "any label or data or proc statement terminates preamble, a macro 
    is only a temporary suspension")
(setq pli-preamble-term 
    "\\(^\\w+:\\|^proc[ \t\n]\\|^data[ \t\n]\\)\\|\\(^%macro[ \t\n]\\)")

(defvar macro-ender "^%mend[ \t\n;]"
  "this terminates a macro def")
(defvar pli-top-level-labels ""
  "*labels that are ALLWAYS at top level. \n
   Now it is possible that some people may be wierd and make some of them not
   at top level. They deserve NOT to have editing support.")

(setq pli-top-level-labels 
      "\\(^proc[ \t\n]\\|^data[ \t\n]\\|^getrow:\\|^putrow:\\|^init:\\|^main:\\|^term:\\)")

(defvar pli-stmt-left
  ""
  "these statements should be left justified")
(setq pli-stmt-left   
      "entry[ \t\n]\\|%macro[ \t\n]\\|%mend[ \t\n;]\\|^proc[ \t\n]\\|^data[ \n\t;]")
   
(defvar pli-back-scan-regexp ""
  "regions : do - 1; select - 2; end - 3; string - 4; comment-end - 5 ; comment-beg - 6; top-level-labels - 7; left-just-label - 8")

(defvar left-just-label "\\(^\\w+:\\)")

(setq pli-back-scan-regexp
  (concat pli-indent-grps "\\|" pli-string-delimiter  "\\|"
	  pli-comment-end "\\|" pli-comment-begin 
	  "\\|" pli-top-level-labels "\\|" left-just-label))


(defvar pli-comments (concat pli-comment-begin "\\|" pli-comment-end))

(defvar pli-first-skippers ""
  "skip over comments, %commands, entry and length and labels")
(setq pli-first-skippers
      (concat "/\\*\\|^%\\|\\(\\w*:\\)" pli-stmt-left))

(defvar pli-string-reg1  (concat pli-comments "\\|\\('\\)")
  "for string scanning")
(defvar pli-string-reg2 (concat pli-comments "\\|\\(""\\)"))

(defvar pli-cards "" "reg exp for see if we have a cards statement
     region 2 tells us if we need 1 or 4 semis")
(setq pli-cards
  "\\(cards\\|lines\\|datalines\\)\\(\\(4\\)\\|\\)[; \t]")
(defvar pli-cards-end1 "^;[ \t\n]")
(defvar pli-cards-end2 "^;;;;[ \t\n]")

(defvar pli-mode-abbrev-table nil
  "Abbrev table in use in pli-mode buffers.")
(define-abbrev-table 'pli-mode-abbrev-table ())

(defvar pli-mode-map ()
  "Keymap used in PLI mode.")
(if pli-mode-map
    ()
  (setq pli-mode-map (make-sparse-keymap))
  (define-key pli-mode-map ";" 'electric-pli-semi)
  (define-key pli-mode-map "\e\C-q" 'pli-indent-region)
  (define-key pli-mode-map "\177" 'backward-delete-char-untabify)
  (define-key pli-mode-map "\e(" 'pli-flash-open)
  (define-key pli-mode-map "\t" 'pli-indent-command))


(defvar pli-mode-syntax-table nil
  "Syntax table in use in PLI-mode buffers.")

(if pli-mode-syntax-table
    ()
  (setq pli-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" pli-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" pli-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" pli-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" pli-mode-syntax-table)
  (modify-syntax-entry ?+ "." pli-mode-syntax-table)
  (modify-syntax-entry ?- "." pli-mode-syntax-table)
  (modify-syntax-entry ?= "." pli-mode-syntax-table)
  (modify-syntax-entry ?% "." pli-mode-syntax-table)
  (modify-syntax-entry ?< "." pli-mode-syntax-table)
  (modify-syntax-entry ?> "." pli-mode-syntax-table)
  (modify-syntax-entry ?& "." pli-mode-syntax-table)
  (modify-syntax-entry ?| "." pli-mode-syntax-table)
;  (modify-syntax-entry ?\; "w" pli-mode-syntax-table) 
  ;; above is a hack to make forward-sexp do what I want
  (modify-syntax-entry ?\' "\"" pli-mode-syntax-table))

(defvar pli-moded-once nil 
  "for doing things to the syntax table and keymap only once")

(defun pli-mode ()
  "Major mode for editing PLI code.
  Tab indents for PLI code.
  Paragraphs are separated by blank lines only.
  Numeric-pad shift 5 - pli-indent-region
  PLIfile-header function inserts proper header

  Do not use the comment statement of '* comment ;' , 
  use instead the C style  /* comment */ type.

  It is recommended that the variable pli-labels-standard be true.
  This requires that left justified labels are treated as 'subroutines'.
  This speeds up indenting greatly.  Labels that are not marking a
  subroutine need to be indented by at least 1 space.

  Variables controlling indentation style and use:
 pli-labels-standard 
  Buffer-specific: If true then labels that are left justified are
   considered to be like subroutines, otherwise , use some indent.
   This will speed up the indenting process.  The command 
    pli-labels-toggle-standard will change the toggle the value
   and make sure that pli-labels-to-left is nil.
 pli-undent-end 
  Controls where an end statement is indented to. T makes it indent at the
    do or select, nil indents the end as an inside statement.
 pli-tab-always-indent
    Non-nil means TAB in PLI mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pli-electric-semi
    Treat ';' as an indenter command  and maybe add a newline,
    see pli-auto-newline
 pli-flash-end
    Flash the opening DO or SELECT statement after an 'end;' is typed
 pli-auto-newline
    Insert new-line and indent after an electric ';'
 pli-indent-level
    Indentation of PLI statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the DO or SELECT statement appears.
 pli-initial-indent
    Indent for top level statements
 pli-preamble-indent
    Statements found to be in the preamble are indented this much
 pli-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or other continued lines.
 pli-labels-to-left
    if true then ALL labels are left justified, if false then
    labels are not moved
 pli-continue-to-indent
    For continued statements, indent each new line
    pli-continued-statement-offset more than the previous
 pli-minimize-indent 
  If true then use real beginning of statement that contains 
    the DO or SELECT, otherwise use the line the DO or SELECT is on.
 pli-mode-hook
    hook to run on start of pli-mode.
 pli-mode-load-hook
    hook that is for key binding"
    
  (interactive)
  (kill-all-local-variables)
  (use-local-map pli-mode-map)
  (setq major-mode 'pli-mode)
  (setq mode-name "PLI")
  (setq local-abbrev-table pli-mode-abbrev-table)
  (set-syntax-table pli-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pli-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'pli-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (if (not pli-moded-once )
      (progn 
	(run-hooks 'pli-mode-load-hook)
	(setq pli-moded-once t)))
  (run-hooks 'pli-mode-hook))

(defun pli-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.


(defun electric-pli-semi (arg)
  "Insert character and correct line's indentation if not on column 1"
  (interactive "P")
  (if (and pli-electric-semi 
	   (not (= 0 (current-column)))
	   (not (eq (preceding-char) ?\;)))
      (electric-pli-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-pli-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos
	 in-string
	 (end (point)))
    (if (and (not arg) (eolp)
	     (= 1 (prefix-numeric-value arg))
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    ;; check if we are in a string
		    ;;  assume strings can only be on one line
		    (while (re-search-forward pli-string-delimiter end t)
			   (setq in-string t)
			   (if (re-search-forward (char-to-string (preceding-char))
				 end t)
			     (setq in-string nil)))
		    in-string)))
      (progn
	(insert last-command-char)
	(pli-indent-line)
	(if pli-flash-end
	  (save-excursion
	    (forward-word -1)
	    (if (looking-at "end;")
	      (pli-flash-open))))
	(and pli-auto-newline
	     (progn
	       (newline)
	       (pli-indent-line)))
	)
      (self-insert-command (prefix-numeric-value arg)))))

(defvar pli-grouper-helper t 
  "nil if not in a do select nest but pli-backward-balanced-grouper returns a number")

(defun pli-backward-balanced-grouper (start )
  "finds the containing PLI do or select statement, returns that pos or t if in a comment or nil if not contained"
  (save-excursion
    (let ((in-comment nil)
	  (l-num (1+ (count-lines 1 (point))))
	  (in-string nil)
	  (groupend-count 0)
	  (groupbeg-count 0))
      (catch 'niler
	(while t
          (if (re-search-backward pli-back-scan-regexp start t)
              (progn
                (cond ((match-beginning 1) ; a do statement
                       (setq groupend-count (1- groupend-count)))
                      ((match-beginning 2) ; a select
                       ;; need to determine if a statement or a function
                       ;; assume if an equal 
                       (save-excursion
                         ;; can speed up here by not using re-search and
                         ;; looking-at. Do not use tabs.
                         (skip-chars-backward " \t")
                         (if (not (or (char-equal (preceding-char) ?=)
                                      (char-equal (preceding-char) ?\()))
                             (setq groupend-count (1- groupend-count)))))
                      ((match-beginning 3)
                       (setq groupend-count (1+ groupend-count)))
                      ((match-beginning 4)
                       ;; assume we are not in a string to begin with
                       (back-over-pli-string start)
                       )
                      ((match-beginning 5)
                       ;; assume we are not in a comment to begin with.
                       (back-over-comment start))
                      ((match-beginning 6)
                       ;; uh-oh - we were in a comment!
                       (throw 'niler t))
                      ((or (match-beginning 7)
                           (and pli-labels-standard (match-beginning 8)))
                       ;; at a top level label!
                       ;; check if we are nested.
                       (if (> groupend-count 0)
                           ;; we have an end with no beginning
                           (error "Line: %d; A top level label \"%s\" at line %d found above an unmatched end statement"
                                  l-num
                                  (buffer-substring
                                    (or (match-beginning 7)
                                        (match-beginning 8))
                                    (or (match-end 7)
                                        (match-end 8)))
                                  (1+ (count-lines 1 (point))))
                           (setq  pli-grouper-helper nil)
                           (throw 'niler (point)))
                       )
                      ;; if not pli-labels-standard (match-beginning 8)
                      ;;  do nothing
                      ;; need if's done.
                      )
                (if (< groupend-count 0)
                    (progn
                      (setq  pli-grouper-helper t)
                      (throw 'niler (point)))))
              (throw 'niler nil)))))))

;; new for speed 	
(defun back-over-pli-string (start)
  "back over PLI strings, may start with single or double and have opposite embedded, unless bol or comment start is found"
  (if (not (search-backward
            (if (eq (following-char) ?\')
                "'"  "\"")
            nil t))
      (error "Error: possible unterminated string, Line: %d" 
             (1+ (count-lines 1 (point))))))
      


;; modified for faster operation. 
(defun back-over-comment (start)
  "if we are in a comment - back up to start of comment, if -not in comment , no back up"
  ;; can spped up here by string scearch for only comment start '/*'
  (search-backward "/*" start t))

(defun in-pli-comment-p (start)
  "are we in a comment"
  ;; later may want to make sure we are not in a string here
  (save-excursion
    (and (re-search-backward pli-comments start t)
         (match-beginning 1))))
          

(defun pli-current-indentation ()
  "get indentation of current line"
  (end-of-line)
  (let ((eol (point)))
    (beginning-of-line)
    (if (re-search-forward "^ *\\w*:[ \t]*" eol t)
      ;; we have a label, return current col
      (current-column)
      (current-indentation))))
      

(defun pli-calculate-indent (parse-start)
  "calculate the indent for a pli line, return a number or t if in a comment, later check if in a string, since strings can only be on one line"
  (save-excursion
    (let ((in-comment (in-pli-comment-p (point-min))))
      (if in-comment 
          (progn
            (goto-char (1+ in-comment))
            (current-column))
          (progn
            (beginning-of-line)
            ;; take care of the stupid /* not in column 1 thing.
            (if (and (not (eobp)) (char-equal (char-after (point)) 32) )
                (forward-char 1))
            (if  (looking-at "/\\*")  1 
                (skip-chars-forward " \t")
                (if (looking-at pli-stmt-left)
                    0
                    
                    (let* ((indent-point (point))
                           (case-fold-search nil)
                           state
                           (limitr  (or parse-start (point-min)))
                           (containing-open 
                            (if (and pli-undent-end (looking-at "end[ ;]"))
                                (save-excursion
                                  ;(debug)
                                  (end-of-line 1)
                                  (pli-backward-balanced-grouper limitr))
                                (pli-backward-balanced-grouper limitr)))
                           (limit (or containing-open parse-start (point-min)))
                           containing-sexp)
                      (if (eq containing-open t)
                          ;; we are in a comment
                          t
                          (goto-char limit)
                          (while (< (point) indent-point)
                            (setq containing-sexp
                                  (car (cdr (parse-partial-sexp
                                              (point) indent-point 0)))))
                          (cond 
                            ((eq containing-open t)
                             ;; we are in a comment
                             t)
                            ((not (null containing-sexp))
                             ;; we are in an expression
                             (goto-char (1+ containing-sexp))
                             (current-column))
                            (t
                             ;; Statement level.  Is it a continuation or a new statement?
                             ;; Find previous non-comment character.
                             (goto-char indent-point)
                             (pli-backward-to-noncomment limitr)
	      
                             (while (eq (preceding-char) ?\,)
                               (pli-backward-to-start-of-continued-exp limitr)
                               (beginning-of-line)
                               (pli-backward-to-noncomment limitr))
	      
                             ;; Now we get the answer.
                             (if (not (memq (preceding-char) '(nil ?\, ?\; ?\:)))
                                 ;; This line is continuation of preceding line's statement;
                                 ;; indent  pli-continued-statement-offset  more than the
                                 ;; previous line of the statement.
                                 (progn
                                   (if pli-continue-to-indent
                                       (pli-backward-to-start-of-continued-exp limit)
                                       (pli-backward-to-real-start-of-continued-exp
                                         (or containing-sexp (point))
                                         limit))
                                   (+ pli-continued-statement-offset (pli-current-indentation)
                                      ))
                                 ;; This line starts a new statement.
                                 ;; Position following last unclosed open.(DO or SELECT)
                                 (if (and containing-open pli-grouper-helper)
                                     (progn ; containing-open is position of opening 
                                        ;(debug)
                                       (goto-char containing-open)
                                       (if pli-minimize-indent
                                           (pli-backward-to-real-start-of-continued-exp
                                             (point)
                                             1)
                                           (beginning-of-line))
                                       ;;skip labels and white space
                                       (if (> containing-open (point))
                                           (re-search-forward "^ *\\w*:" 
                                                              containing-open t))
                                       (skip-chars-forward " \t\n")
                                       (+ (current-column) pli-indent-level))
                                     ;; we are top level, find if we are in the preamble
                                     (save-excursion
                                       (goto-char (point-min))
                                       (if (pli-in-preamble indent-point)
                                        ; in preamble proper
                                           pli-preamble-indent
                                        ; else in main part or macro def
                                           pli-initial-indent)))))))))))))))


(defun pli-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(setq stop t)
	(if stop (goto-char opoint)
	  (beginning-of-line))))))

(defun pli-backward-to-real-start-of-continued-exp (start lim)
  "go back to a real semi or full colon, start must not be embedded in a
  open paren"
  (goto-char start)
  (let (opoint stop)
    (while (not  (or (looking-at "\\(^\\w+\\:\\)\\|\\(\\w*\\;\\)")
		     (bobp)
		     (> lim (point))))
	   (forward-sexp -1))
    ; we are at a label beginning or a semi or at the limit.
    (forward-sexp (if  (> lim (point)) 1 2 ))
    (forward-word -1); we should be there!
    ))
	

(defun pli-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun pli-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))


(defun pli-in-preamble (place)
  "point is at beginning usally, if PLACE is still in preamble  but not in a macro def, return t, else return nil"
  (catch 'pre-amb-term
    (while (re-search-forward pli-preamble-term place t)
	   (cond  ((match-beginning 1) ; a real terminator
		   (throw 'pre-amb-term nil))
		  ((match-beginning 2) ; a macro def
		   (if (not (re-search-forward macro-ender place t))
		     ;; we are in a macro def
		     (throw 'pre-amb-term nil))))
	   )
    t))



    
(defun calculate-pli-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))

(defun pli-indent-line ()
  "Indent current line as pli/scl code. Return the amount the indentation changed by."
  (let ((indent (pli-calculate-indent nil))
	beg shift-amt label
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (pli-current-indentation)))
	  ((eq indent t)         
	   (setq indent (calculate-pli-indent-within-comment)))
	  ;; macro calls ??
	  ((looking-at "[ \t]*%macro;")
	   (setq indent 0))
	  ((looking-at "^%")
	   (setq indent 0))
	  ;; here we have skipped spaces and tabs now
	  ((and
	    (skip-chars-forward " \t")
	    (looking-at "else\\b"))
	   (setq indent (save-excursion
			  (pli-backward-to-start-of-if)
			  (pli-current-indentation))))
          ((looking-at "\\w*:")
           (setq indent (if pli-labels-to-left 
                            0  (current-indentation))))
          )
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
	(delete-horizontal-space)
	(indent-to indent)
	;; If initial point was within line's indentation,
	;; position after the indentation.  Else stay at same point in text.
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos))))
    shift-amt))



(defun pli-indent-command (&optional whole-exp)
  
  "Indent current line as PLI code, or in some cases insert a tab character.
If pli-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."

   (interactive "P")
   (if whole-exp
     ;; If arg, always indent this line as PLI
     ;; and shift remaining lines of expression the same amount.
     (let ((shift-amt (pli-indent-line))
	   beg end)
       (message "%d" shift-amt)
       (save-excursion
	 (if pli-tab-always-indent
	   (beginning-of-line))
	 (setq beg (point))
	 (forward-sexp 1)
	 (setq end (point))
	 (goto-char beg)
	 (forward-line 1)
	 (setq beg (point)))
       (if (> end beg)
	 (indent-code-rigidly beg end shift-amt "#")))
     (if (and (not pli-tab-always-indent)
	      (save-excursion
		(skip-chars-backward " \t")
		(not (bolp))))
       (insert-tab)
       (pli-indent-line))))

;
;



(defun pli-flash-open (&optional start)
  "Flash the opening grouper - Do or Select"
  (interactive) 
  (let ((opener (pli-backward-balanced-grouper (or start (point-min)))))
    (cond ((eq opener t)
	   (message "You are within a comment")
	   (beep 1)
	   )
	  ((eq opener nil)
	   (message "No matching Do or Select found")
	   (beep 1))
	  (t ; have a position, need to see if visable
	   (save-excursion
	     (goto-char opener)
	     (if (or (looking-at pli-top-level-labels)
                     (and pli-labels-standard 
                          (looking-at left-just-label)))
		 (error "A top level label \"%s\" at line %d found above this unmatched end statement"
				     (buffer-substring
				       (match-beginning 0)
				       (match-end 0))
				     (1+ (count-lines 1 (point)))))
	     (if (>= opener (window-start) )
	         ; yes visable
		 (sit-for pli-flash-pause)
		 (message
		   (concat
		     "LINE %d:"
		     (buffer-substring
			    (progn
			      (beginning-of-line)
			      (point))
			    (progn
			      (end-of-line)
			      (point))))
		   (1- (1+ (count-lines 1 (point)))))))))))



    
    


(defun pli-indent-region (parg)
  ;; This currently is a very poor way to do this. It really only needs to
  ;; do simple parsing as it goes forward. Other functions were made and this
  ;; is the easiest. Change later.
    "Indent the pli code from mark to point unless a prefix, then from buffer start to point if a positive prefix, and from point to end if negative"
  (interactive "P")
  (let*
    ((line-cnt 0)
     (arg (prefix-numeric-value parg))
     (start (if (> arg 1)
	      (point-min)
	      (min (point) (mark))))
     (start-line (save-excursion
		   (goto-char start)
		   (1+ (count-lines 1 (point)))))
     (end   (if (> 0 arg)
	      (point-max)
	      (max (point) (mark))))
     (end-pos (- (point-max) end))
     (end-line (save-excursion
		 (goto-char end)
		 (1+ (count-lines 1 (point)))))
     (num-lines (1+ (- end-line start-line))))
    (message "indenting PLI code region, ....")
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (catch 'at-eob
	(while  (> (- (point-max) (point))
		   end-pos)
	  (setq num-lines (1- num-lines))
	  (setq line-cnt (1+ line-cnt))
	  (if (> line-cnt 5)
	      (progn 
		(message "indenting PLI line %d" (1+ (count-lines 1 (point))))
		(setq line-cnt 1)))
	  (let ((bol (point)))
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (cond ((eolp) ; blank line, delete spaces
		   (delete-region bol (point))
		   (forward-line))
		  ((looking-at pli-cards)
		   ;; beginning of PLI cards or data lines
		   (if  (re-search-forward 
			      (if (match-beginning 3) 
				  pli-cards-end2
				  pli-cards-end1)
			      nil t)
			(forward-line)
			(looking-at pli-cards)
			(error 
			  "A %s statement at line %d not ended correctly"
			  (buffer-substring (match-beginning 1)
					    (match-end 1))
			  (1+ (count-lines 1 (point))))))
		  (t (pli-indent-line)
		     (forward-line)))
	    (if  (= (point) (point-max))
		(throw 'at-eob nil))
	    )))
      (beep)
      (message "Done indenting!"))))

(defun pli-labels-toggle-standard ()
 "toggle pli standard mode, ie the pli-labels-standard buffer specfic 
  variable"
  (interactive)
  (setq pli-labels-to-left nil)
  (setq pli-labels-standard (not pli-labels-standard))
  (message (if pli-labels-standard 
               "New PLI standard, indent non-subroutine labels"
               "Labels have no standard for indent"))
  )



(defun pli-big-pic (col)
  "Do a set-selctive-display at the current column, Column 1 resets,
   this hides all lines that are indented further that the column"
  (interactive (list (current-column)))
  (set-selective-display col))

(defun pli-forward-function()
  "PLI does not have strong markers. So go forward to a label, left justified"
  (interactive)
  (if (not (re-search-forward left-just-label nil t))
      (message "No more left justified labels found")))

(defun pli-backward-function()
  "PLI does not have strong markers. So go backward to a label, left justified"
   (interactive)
   (if (not (re-search-backward left-just-label nil t))
      (message "No more left justified labels found")))

(provide 'pli-mode)


;;====================================================================
;;====================================================================
;;====================================================================
;;====================================================================



;;; C comment mode - An auto-filled comment mode for gnu c-mode.
;;;
;;; Author:  	Robert Mecklenburg
;;;		Computer Science Dept.
;;;          	University of Utah
;;; From: mecklen@utah-gr.UUCP (Robert Mecklenburg)
;;;   Also hartzell@Boulder.Colorado.EDU
;;; (c) 1986, University of Utah
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, provided the people they give it to can.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I have written a "global comment" minor-mode which performs auto-fill,
;;; fill-paragraph, and auto-indentation functions.  This function only
;;; works for comments which occupy an entire line (not comments to the
;;; right of code).  The mode has several options set through variables.
;;; If the variable c-comment-starting-blank is non-nil multi-line
;;; comments come out like this:
;;; 
;;; 	/*
;;; 	 * Your favorite 
;;; 	 * multi-line comment.
;;; 	 */
;;; 
;;; otherwise they look like this:
;;; 
;;; 	/* Your Favorite
;;; 	 * multi-line comment.
;;; 	 */
;;; 
;;; If the variable c-comment-hanging-indent is non-nil K&R style comments
;;; are indented automatically like this:
;;; 
;;; 	/* my_func - For multi-line comments with hanging indent
;;; 	 *	     the text is lined up after the dash.
;;; 	 */
;;; 
;;; otherwise the text "the text" (!) is lined up under my_func.  If a
;;; comment fits (as typed) on a single line it remains a single line
;;; comment even if c-comment-starting-blank is set.  If
;;; c-comment-indenting is non-nil hitting carriage return resets the
;;; indentation for the next line to the current line's indentation
;;; (within the comment) like this:
;;; 
;;; 	/* Typing along merrily....
;;; 	 *     Now I indent with spaces, when I hit return
;;; 	 *     the indentation is automatically set to 
;;; 	 *     ^ here.
;;; 	 */
;;; 
;;; ----------------------------------------------------------------------
;;;

;;; modifications by Mark Riggle and John Maxwell of SAS Institute


(defvar c-comment-starting-blank t
  "*Controls whether global comments have an initial blank line.")
(defvar c-comment-indenting t
  "*If set global comments are indented to the level of the previous line.")
(defvar c-comment-hanging-indent t
  "*If true, comments will be automatically indented to the dash.")
(defvar c-hang-already-done t
  "If true we have performed the haning indent already for this comment.")


;;;
;;; c-comment-map - This is a sparse keymap for comment mode which
;;; 		    gets inserted when c-comment is called.
;;; 

(defvar c-comment-mode-map ()
  "Keymap used in C comment mode.")

(require 'cc-mode)	; SWO Hack of the day 12/16/94
(setq c-comment-mode-map (copy-keymap c-mode-map))
(define-key c-comment-mode-map "\e\r" 'newline)
(define-key c-comment-mode-map "\eq"  'set-fill-and-fill)
(define-key c-comment-mode-map "\ep"  'set-fill-and-fill)
(define-key c-comment-mode-map "\r"   'set-fill-and-return)

(save-window-excursion
  (switch-to-buffer "$C-comment")
  (bury-buffer (current-buffer))
  (use-local-map c-comment-mode-map)
  (local-set-key "\^J" 'set-fill-and-return)
  (local-set-key "\e*Nc4" 'set-fill-and-fill)
  ;; (local-set-key "\e*Fs5" 'comment-delimiter-line)
  )

;;; -------------------------------------------------------------------
;;; Comment-delimiter-line
;;; Use this to draw a line of dashes or some other character that you
;;; like across a comment, usually at the top, to delimit it from the
;;; rest of the text better.  Works best if you set
;;; c-comment-starting-blank to nil.

(defvar comment-delimiter-line-end-column 70)
(setq comment-delimiter-line-end-column 70)
(defvar comment-delimiter-line-char ?-)

(defun comment-delimiter-line ()
  "Draw a line of characters from point to the end of the line"
  (interactive)
  (insert-char comment-delimiter-line-char
               (let ((len (- comment-delimiter-line-end-column
                             (current-column))))
                 ;(message "Current column %d" (current-column))
                 (if (> len 0) len 0))))

(defvar c-comment-map-stack nil "for restoring local map")
;;; --------------------------------------------------------------
;;;
;;; c-comment - This is a filled comment mode which can format
;;; 		indented text, do hanging indents, and symetric
;;; 		placement of comment delimiters.
;;; 
(defun c-comment ()
  "Edit a C comment with filling and indentation.
This performs hanging indentation, symmetric placement of delimiters,
 and Indented-Text mode style indentation.  Type 'M-x apropos
c-comment' for information on options."
  (interactive)
  (let
      ;; Save old state.
      ((auto-fill-hook (if c-comment-indenting
			   'do-indented-auto-fill 'do-auto-fill))
;       (comment-start nil)
       (comment-multi-line t)
       (comment-start-skip "/*\\*+[ 	]*")
       (paragraph-start-ref paragraph-start)
       fill-prefix paragraph-start paragraph-separate opoint)

    ;; Determine if we are inside a comment.
    (setq in-comment
	  (save-excursion
	    (and (re-search-backward "/\\*\\|\\*/" 0 t)
		 (string= "/*" (buffer-substring (point) (+ (point) 2))))))

    ;; Indent the comment and set the fill prefix to comment continuation
    ;; string.  If we are already in a comment get the indentation on
    ;; the current line.
    (setq c-hang-already-done nil)

    ;; Set the beginning of the comment and insert the blank line if needed.
    (setq c-comment-map-stack (cons (current-local-map) c-comment-map-stack))
    (use-local-map c-comment-mode-map)
    (if (not in-comment)
	(progn (funcall indent-line-function)
	       (insert "/* ")
	       (setq fill-prefix (get-current-fill (point)))
	       (recursive-edit)

	       ;; If the comment fits on one line, place the close
	       ;; comment at the end of the line.  Otherwise, newline.
	       (setq opoint (point))
	       (if (and (save-excursion (beginning-of-line)
					(search-forward "/*" opoint t))
			(<= (+ (current-column) 3) 79))
		   (insert " */")
		 (insert "\n*/"))

	       (funcall indent-line-function))
      (progn (setq fill-prefix (get-current-fill (point)))
	     (recursive-edit)
	     (search-forward "*/" (buffer-size) t)
	     (next-line 1)))

    ;; If starting blank enabled, insert a newline, etc., but only if
    ;; this comment requires multiple lines.
    (if c-comment-starting-blank
	(save-excursion
	  (setq opoint (point))
	  (next-line -1)
	  (if (or (null (search-forward "/*" opoint t))
		  (null (search-forward "*/" opoint t)))
	      (progn
		(search-backward "/*")
		(re-search-forward comment-start-skip opoint t)
		(setq fill-prefix (get-current-fill (point)))
		(if (not (or (looking-at "\n")
                             ;; --------------------------------------
                             ;; HJM: 1Aug989
                             ;; Don't add top line if it has a
                             ;; comment delimiter line on it.
                             ;; --------------------------------------
                             (eql (char-after (point))
                                  comment-delimiter-line-char)))
		    (insert ?\n fill-prefix))))))
; 		    (indent-new-comment-line)

    ;; Move cursor to indentation.
    (funcall indent-line-function)
    (use-local-map (car c-comment-map-stack))
    (setq c-comment-map-stack (cdr c-comment-map-stack))
    )
  )

;;;
;;; set-fill-and-fill - Get the current fill for this line and fill
;;; 			the paragraph.
;;; Fri 26May89 hjm change to paragraph defs for * lines.
;;; Tue 30May89 changed to make /* and */ delimit paragraphs.

(defun set-fill-and-fill (arg)
  "Get the fill-prefix and fill the current paragraph."
  (interactive "P")
  (setq fill-prefix (get-current-fill (point)))
  (let ((ops paragraph-start)
	(opsep paragraph-separate))
   ;; paragraphs delimited by whatever is usual, plus blank comment
   ;; lines at start and finish of block comment, and blank starred
   ;; lines in the middle of a block comment.
    (setq paragraph-start
          (concat paragraph-start    "\\|^[ /*-]*$"))
    (setq paragraph-separate
          (concat paragraph-separate "\\|^[ /*-]*$"))
    (fill-paragraph arg)
    (setq paragraph-start ops)
    (setq paragraph-separate opsep)
    ))

;;;
;;; set-fill-and-return - Set the current fill prefix and
;;; 			  indent-new-comment-line.
;;; 

(defun set-fill-and-return ()
  "Set the current fill prefix and move to the next line."

  (interactive)
  (if c-comment-indenting
      (setq fill-prefix (get-current-fill (point))))
  (insert ?\n fill-prefix))

;;;
;;; do-indented-auto-fill - Perform the auto-fill function, but get
;;; 			    the fill-prefix first.
;;; 
(defun do-indented-auto-fill ()
  "Perform auto-fill, but get fill-prefix first."

  (let ((opoint (point)))
    (save-excursion
      (move-to-column (1+ fill-column))
      (skip-chars-backward "^ \t\n")
      (if (bolp)
	  (re-search-forward "[ \t]" opoint t))
      ;; If there is a space on the line before fill-point,
      ;; and nonspaces precede it, break the line there.
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))

	  ;; If we are wrapping to a new line, figure out the indentation on
	  ;; the current line first.
	  (progn
	    (setq fill-prefix (get-current-fill opoint))
	    (insert ?\n fill-prefix)))))
;	    (indent-new-comment-line)))))
  )


;;;
;;; get-current-fill - Get the fill-prefix for the current line.  This
;;; 		       assumes that the valid fill prefix is between
;;; 		       (beginning-of-line) and (point).
;;; 
(defun get-current-fill (pnt)
  "Get the current fill prefix.
A valid fill prefix must be between the beginning of the line and point."

  (let ((opoint pnt) fill last-char)
    (save-excursion
      (beginning-of-line)
      (setq fill
	    (buffer-substring (point)
			      (progn
				(re-search-forward comment-start-skip opoint t)
				(point))))

      ;; Be sure there is trailing white space.
      (setq last-char (substring fill (max 0 (1- (length fill)))
                                 (length fill)))
      (if (and (not (string= " " last-char))
	       (not (string= "	" last-char))
               )
	  (setq fill (concat fill " ")))

      (setq fill (replace-letter fill "/" " "))

      ;; Get the hanging indentation if we haven't already.
      (if (and c-comment-hanging-indent (not c-hang-already-done))
	  (let ((curr (point))
		(opnt (progn (end-of-line) (point))))
	    (beginning-of-line)
	    (if (re-search-forward " - \\|.*:[ ]*$" opnt t)
		(progn
		  (setq fill (concat fill (make-string (- (point) curr) 32)))
		  (setq c-hang-already-done t)))))

      ;; Set the paragraph delimiters.
      (setq paragraph-start (concat paragraph-start-ref
				    "\\|^"
				    (regexp-quote
				     (substring fill
						0 (1- (length fill))))
				    "$"))
      (setq paragraph-separate paragraph-start))
    fill)
  )
  

;;;
;;; replace-letter - Given a string, an old letter and a new letter,
;;; 		     perform the substitution.
;;; 
(defun replace-letter (str old-letter new-letter)
  (let (new-str c
	(sp 0)
	(size (length str)))
    (while (< sp size)
      (setq c (substring str sp (1+ sp)))
      (setq new-str (concat new-str (if (string= c old-letter) new-letter c)))
      (setq sp (1+ sp)))
    new-str))


(provide 'c-fill)
(provide 'ai-c-fill)



;;;----------------------------------------------
;;; Useful function for determining what a function key produces.
;;; Execute the function and then press the function key.
;;; From Randal L. Schwartz <merlyn@intelob.intel.com>
(defun keyboard-read-key (arg)
  "Displays characters typed, terminated by a 3-second timeout.
With ARG, also insert key description into buffer."
  (interactive "P")
  (let ((chars "")
        (inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
            quit-flag nil))             ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))
    (and arg (insert (key-description chars)))))
 




