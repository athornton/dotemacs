; Copyright (C) 1990 Mark B. Motl
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Publich License as published by
; the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; To obtain a copy of the GNU General Public License, write to:
;   Free Software Foundation, Inc.
;   675 Mass Ave.
;   Cambridge, MA  02139
;   USA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   AUTHOR:  Mark B. Motl  <motl@cssun.tamu.edu>
;            Department of Computer Science
;            Texas A&M University
;            College Station, TX  77843-3112
;
;   PURPOSE: The following functions are a set of GNU Emacs Lisp functions
;            to extend Emacs so that it is sensitive to the WEB style of
;            programming.
;
;   DATE:    The preliminary version of these functions was written in
;            Summer/Fall 1989.
;
;   ADVISOR: Dr. S. Bart Childs <bart@cssun.tamu.edu>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; <DISCLAIMER>
; This program is still under development.  The author accepts no
; responsibility to anyone for the consequences of using it, for whether
; it serves any particular purpose, or even works at all.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; Any bugs or comments should be mailed to the author at the above address.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; REVISION HISTORY
;
; Late November 1989
;
; Made a significant change to the type of list maintained for the module
; names.  It now looks like 
; 
; (("Module Name 1" (Defined in Module #'s) (Used in Module #'s))
;  ("Module Name 2" (Defined in Module #'s) (Used in Module #'s))
;  ...
;  ("Module Name n" (Defined in Module #'s) (Used in Module #'s))
; )
;
; As a consequence of this reorganization, the following functions had to
; be modified:  any-modules-undefined-initially, collect-module-names, 
; display-module-names, module-lookup, options-for-module-name-insertion, 
; and view-module-names.
;
; Two functions were deleted:  print-module-names and replace-nth-element.
; 
; The function "append-a-stub-module" was modified so that a stub module
; is now added at the end of the section in which it is first used
; instead of at the end of the buffer.
;
; Some new functions have been added:
;  1) "collect-section-names" -- constructs a list of the major sections
;     and the module in which each section starts
;  2) "view-section-names" -- prints the list of section names to another
;     buffer
;  3) "switch-to-module" -- this function is useful when one is switched
;     to another buffer (e.g., running "view-section-names") and wants to
;     return to the buffer where the WEB document is.  It prompts for a
;     module number to return to in the WEB; or, alternatively, one can 
;     provide a prefix argument.
;  4) "insert-new-module-name-in-list-of-module-names" -- this replaces
;     the old function "replace-nth-element".
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Early December 1989
;
; Added several functions so that traversal of the Defined In and Used In
; lists in the list of module names could be accomplished.  These are:
;
; first-use and first-define -- these can be executed from either the buffer
;   containing the WEB document or the buffer where the module names are
;   displayed. The only way to select a module is in the buffer containing
;   the list of module names (position the cursor on the line containing
;   the module name of interest). Once a module name has been selected,
;   first-use switches back to the buffer containing the WEB document at
;   the module where the selected module name was first used. If the
;   command is issued from the buffer containing the WEB document, point
;   is positioned at the beginning of the module where the most recently
;   selected module name was first used. The function first-define is analogous
;   to the operation of first-use except that it works on the Defined In
;   list instead of the Used In list.
;
; next-use, previous-use, next-define, and previous-define -- These functions
;   work as one would expect. These functions can only be used in the buffer
;   containing the WEB document.
;
; An internal support function, convert-list-to-string, was added so that
;   the lists Defined In and Used In (lists of integers) could be inserted
;   in the buffer where the module names are displayed.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Late December 1989
;
; Added some new functions to aid in communication with the index that
; Weave creates.  These are:  eliminate-control-sequences, first-index-use,
; next-index-use, previous-index-use, reformat-the-index, and view-index.
; The second, third, fourth, and last of these are interactive; the remaining
; ones are support functions.  When the command view-index is invoked, it
; first checks to see if the file INDEX.tex is present (we have modified the
; output generated by Weave so that in addition to the TeX file, it also 
; creates two other files: INDEX.tex which contains the index and
; MODULE_NAMES.tex which contains the list of section names).  If not present,
; it creates another process and Weaves the current buffer.  The file
; INDEX.tex is then read in and some reformatting is done so that it is more
; readable.  The user selects an index entry of interest by placing the
; cursor anywhere on the line(s) where the index entry is located.  An index
; entry is selected then by issuing the command first-index-use.  This command
; can be issued from the buffer where the index is displayed or it can be
; issued from the buffer containing the WEB document.  If invoked from the
; former, it switches back to the buffer containing the WEB document at the
; beginning of the module where the selected index entry was first referenced.
; If invoked from the latter, point is positioned at the beginning of the
; module where the most recently selected index entry was first referenced.
;
; Once an index entry has been selected, the functions next-index-use and
; previous-index-use can be used to traverse the list of references for the
; most recently selected index entry.
;
; Also added functions count-sections (provides a count of the total number 
; of sections in the WEB), goto-section (which goes to the beginning of the
; n-th section), and what-section (which informs the user of the section 
; that point is currently positioned in).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ACCOMMODATIONS FOR A CHANGE FILE
;
; In late December 1989, I added five new functions that deal with CHange
; files.  These are:
;
; 1) change-module --- Used in the buffer that contains the Web 
;    document.  The user positions point in the module that he wishes to
;    change and then invokes the function.  It copies the entire module
;    contents from the Web buffer to the buffer containing the CHange file
;    twice---once between an @x @y and once between an @y @z.  The text
;    is inserted in the proper position in the CHange file.
;
; 2) collect-list-of-changed-module-numbers --- collects and returns a list
;    of module numbers that have been changed in the CHange file.
;
; 3) escape-special-characters-in-regexp --- used when determining what
;    module in the Web a particular change corresponds to.  Because the
;    text between the @x and @y may contain special characters Emacs uses
;    in regular expression searches, it is necessary to turn them into
;    ordinary characters.
;
; 4) view-list-of-changed-modules --- available in both the buffer 
;    containing the Web document and the buffer containing the CHange file.
;    It displays a list of which modules in the Web have been changed in the
;    CHange file.
;
; 5) what-change --- available only in the buffer containing the CHange
;    file.  Based on the position of point, it displays a descriptive message
;    in the minibufer indicating which module in the Web is being changed.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 3, 1990
;
; Changed the names of some functions:
;   FROM Name:                                   TO Name:
;   module-count                                 count-modules
;   section-count                                count-sections
;   show-index                                   view-index
;   show-list-of-changed-modules                 view-list-of-changed-modules
;   show-module-names                            view-module-names
;   show-section-names                           view-section-names
;   statistics                                   determine-statistics
;   what-module-does-this-change-correspond-to   what-change
;
; Also added key-bindings which are accomplished at load time.  I used the
; convention:  Each command takes the form
;   ESC (or META)
;   CTRL first-letter-of-function-name 
;   CTRL first-letter-of-second-word-in-function-name
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 8, 1990
;
; Added two new functions change-file and web-file.  When invoked they
; set the current buffer to the CHange or Web file, respectively. 
; Eliminated the function switch-to-module.
;
; Changed some function names:
; FROM                                 TO
; change-this-module                   edit-module
; check-balance-of-module-delimiters   delimiter-match-check
; determine-statistics                 determine-characteristics
; first-index-use                      first-index
; next-index-use                       next-index
; previous-index-use                   previous-index
; view-list-of-changed-modules         view-changed-modules-list
; view-module-names                    view-module-names-list
; view-section-names                   view-section-names-list
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 10, 1990
;
; Added four new functions:
; 1) count-changes -- counts the number of changes in the CHange file.
; 2) goto-change -- goes to the n-th change in the CHange file.  The 
;    changes are numbered sequentially beginning with 1.
; 3) next-change -- positions point at the beginning of the next change.
; 4) previous-change -- positions point at the beginning of the previous 
;    change.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 13, 1990
;
; Added two new functions:
; 1) initialize-module-names-list -- this function is invoked by web-mode
;    if two conditions prevail:
;      a) the file .mods exists for the WEB beging edited and
;      b) the file .mods is newer than the WEB 
;    If both of these conditions are true, the information to initialize
;    the module-names list is read in from the file .mods.  This saves
;    a considerable amount of time when the WEB is quite large, such as
;    initex.web.  Thus, if the file initex.mods exists and it is newer
;    than initex.web, a lot of time can be saved in the initial startup.
; 2) write-module-names-to-a-file -- this is also an internal support function.
;    Every time the module-names must be collected, this function is invoked 
;    so that the new information can be written to the file .mods.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 16, 1990
;
; Completely changed the function collect-list-of-changed-module-numbers.
; The method the function now uses closely resembles the method used by
; Knuth in TANGLE and WEAVE.  See modules 128 through 137 in TANGLE.WEB.
; As a result of this change, I was able to delete the function escape-
; special-characters-in-regexp.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; January 27, 1990
;
; Reworked the function collect-module-names so that it works considerably
; faster.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; April 7, 1990
;
; Eliminated the functions first-index, first-define, and first-use.  Made
; the functions next-define, next-use, next-index, previous-define,
; previous-use, and previous-index more logical.  They work in either the
; buffer displaying the index or module names or in the buffer containing
; the Web document.  If in one of the special buffers and one of these
; commands is issued, the user is switched back to the Web document at the
; next or previous occurrence based on point's position in the Web document
; before the switch over to the special buffer.
;
; Added a global variable web-module-changed-then-goto-change.  Its purpose
; is to indicate where point is to be positioned when going to a module that
; has been changed.  If the variable's value is true, then point is positioned
; in the CHange file at the change that corresponds to the module that the
; user wants to go to.  If the variables value is not true, then point is
; positioned in the Web document with a message in the minibuffer that says
; that the module has been changed.
;
; Renamed all functions and global variables so that web- precedes their name.
; This was done in an effort to avoid any collisions when web-mode is used in
; conjunction with other modes.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; July 30, 1990
; 
; After looking at some journal files, I realized that when web-mode-save-
; buffers-kill-emacs is invoked, the web-journal function doesn't write out
; the proper count for web-mode-save-buffers-kill-emacs because the file
; is written before the vector containing the counts gets updated.
; 
; 1. Extend reformat or rewrite it to produce statistics.
;    These should include the usual things but for webs
;    it should also read a config file and produce things
;    like counts of @<*@>, @<sys*@>, and all other index
;    things, ...
; 
; 2. The environment of Mark does not need this but a WEB
;    user does.  Each library should have a corresponding
;    library whose elements are WEB sources for documenting
;    calls.  For example: calling LEQT1F from IMSL should
;    be complemented by emacs including /usr/local/doc/imsl/leqt1f.web
;    and editing for the local use.
; 
; 3. Major sections need a complementary command of the type
;    of @*subprogram-name@*.  This string could be passed
;    to the index for scoping variables and comments that
;    may be entered into the index.  Global vs local variables.
; 
; 4. C prototypes and Pascal forward references: display of
;    these kinds of items could be handy for a programmer.
;    This is somewhat related to 2. above.
; 
; 5. Integration with other tools such as Marcus' LPT or
;    Kevin Borden's work with X.  Two particular items that
;    would yield the most benefit would be a tex previewer
;    and source level debugger.  These may be possible now,
;    just not yet documented.
; 
; 6. Also need to set up a default directory for Emacs to
;    search when @i's are encountered.
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; GLOBAL variables and constants used in web-mode.
;
(defvar web-buffer-name "" "Name of Web document being edited.")

(defvar web-change-buffer-name nil
  "Name of the buffer containing the CHange file")

(defvar web-default-directory nil
  "The default-directory that the Web is located in.")

(defvar web-defined-in-occurrence -1 
  "Used as a pointer into the (Defined In) list of the most recently selected 
module name.")

(defvar web-defined-in-used-in-location -1 
  "Used as a pointer into the list of module names. Used to take the 
(web-defined-in-used-in-location)-th car of module-names.")

(defvar web-files nil
  "A list of the files used in the WEB.  The general form of the list is:
(\"CHange file name\" \"WEB file name\" \"include file 1\" \"include file 2\"
... \"include file n\")")

(defvar web-index-buffer-name "" 
  "Name of the buffer where the index is displayed.")

(defvar web-index-entry ""
  "The selected index entry.")

(defvar web-interactive-function-usage nil
  "A vector containing the number of times that the interactive functions
were executed.  The i-th element corresponds to the i-th car of 
web-interactive-functions")

(defvar web-interactive-functions nil
  "List of the interactive functions available in web-mode.")
; initialize web-interactive-functions
(setq web-interactive-functions 
  '("web-change-file"
    "web-collect-module-names"
    "web-count-changes"
    "web-count-modules"
    "web-count-sections"
    "web-delimiter-match-check"
    "web-determine-characteristics"
    "web-edit-module"
    "web-file"
    "web-goto-change-corresponding-to-module"
    "web-goto-module"
    "web-goto-section"
    "web-include-file"
    "web-insert-index-entry"
    "web-insert-module-name"
    "web-is-this-a-new-module-beginning"
    "web-mode"
    "web-mode-save-buffers-kill-emacs"
    "web-newline"
    "web-next-change"
    "web-next-define"
    "web-next-index"
    "web-next-module"
    "web-next-section"
    "web-next-use"
    "web-previous-change"
    "web-previous-define"
    "web-previous-index"
    "web-previous-module"
    "web-previous-section"
    "web-previous-use"
    "web-rename-module"
    "web-view-changed-modules-list"
    "web-view-index"
    "web-view-module-names-list"
    "web-view-section-names-list"
    "web-what-change"
    "web-what-module"
    "web-what-section"
   ) ; end list
) ; end setq

(defvar web-location-in-selected-index-entry-occurrences -1 
  "Used as a pointer into the list selected-web-index-entry-occurrences.")

(defvar web-location-of-module nil
  "A vector of vectors.  The i-th element contains information about the i-th
module.  Each element is a vector of 4 parts: the first part is an index into
the list web-files indicative of which file the i-th module is located; the
second part is the module number relative to the file that it is located in;
the third part tells the major section that the i-th mdoule is located in;
the fourth part indicates if the i-th module has been changed (0=no, 1=yes);
and the fifth indicates the position of the CHange in the CHange file.")

(defconst web-max-number-of-modules 2000
  "The maximum number of modules in the WEB and its associated include
files.")

(defvar web-mode-map nil "Keymap used in Web Mode.")

(defvar web-module-begins "^@\\( \\|\t\\|\n\\|\\*\\)" 
  "Regular expression used for determining the beginning of a module.")

(defvar web-module-changed-then-goto-change nil
  "Boolean variable.  When going to a module and it is true, point is 
positioned in the CHange buffer at the CHange corresponding to the module
number; otherwise, point is positioned at the module in the Web file.")

(defvar web-module-name "\\(@@\\)*@<\\(@'\\|@\"\\|@@\\|[^@]\\)+@>"
  "Regular expression used for isolating a valid module name.")

(defvar web-module-name-defined-in-used-in nil
  "A list of the Web's module names.  The list takes the form
  ((\"Module Name 1\" (Defined in Module #'s) (Used in Module #'s))
   (\"Module Name 2\" (Defined in Module #'s) (Used in Module #'s))
   ...
   (\"Module Name n\" (Defined in Module #'s) (Used in Module #'s))
  )")

(defvar web-module-names nil "List of Web document's module names.")

(defvar web-number-of-lines-in-window 0 
"Total number of lines in window where Emacs is used.")

(defvar web-number-of-module-names 0 
  "Total number of module names in the Web.")

(defvar web-number-of-modules-in-file nil
  "A vector that contains number of modules in each of the WEB files. The
elements are interpretted as follows:  0 - # of modules in the CHange file;
1 - # of modules in the WEB file; 2 - # of modules in include file 1; 
3 - # of modules in include file 2; etc.")

(defvar web-selected-index-entry-occurrences nil 
  "A list comprised of the module numbers where the most recently selected 
index entry was referenced.")

(defvar web-selected-module-name "" 
  "Represents the name of the most recently selected module name.")

(defvar web-used-in-occurrence -1 
  "Used as a pointer into the (Used In) list of the most recently selected 
module name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialize the key-map used in web-mode.  This is done at load-time.
;
(if web-mode-map
; then the user has defined one
  ()
; else
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\C-c\C-f" 'web-change-file)
    (define-key map "\e\C-c\C-c" 'web-count-changes)
    (define-key map "\e\C-c\C-m" 'web-count-modules)
    (define-key map "\e\C-c\C-s" 'web-count-sections)
    (define-key map "\e\C-d\C-m" 'web-delimiter-match-check)
    (define-key map "\e\C-d\C-c" 'web-determine-characteristics)
    (define-key map "\e\C-e\C-m" 'web-edit-module)
    (define-key map "\e\C-w\C-f" 'web-file)
    (define-key map "\e\C-g\C-c" 'web-goto-change-corresponding-to-module)
    (define-key map "\e\C-g\C-m" 'web-goto-module)
    (define-key map "\e\C-g\C-s" 'web-goto-section)
    (define-key map "\e\C-i\C-f" 'web-include-file)
    (define-key map "\e\C-i\C-i" 'web-insert-index-entry)
    (define-key map "<"          'web-insert-module-name)
    (define-key map "@"          'web-is-this-a-new-module-beginning)
    (define-key map "\015"       'web-newline)
    (define-key map "\e\C-n\C-c" 'web-next-change)
    (define-key map "\e\C-n\C-d" 'web-next-define)
    (define-key map "\e\C-n\C-i" 'web-next-index)
    (define-key map "\e\C-n\C-m" 'web-next-module)
    (define-key map "\e\C-n\C-s" 'web-next-section)
    (define-key map "\e\C-n\C-u" 'web-next-use)
    (define-key map "\e\C-p\C-c" 'web-previous-change)
    (define-key map "\e\C-p\C-d" 'web-previous-define)
    (define-key map "\e\C-p\C-i" 'web-previous-index)
    (define-key map "\e\C-p\C-m" 'web-previous-module)
    (define-key map "\e\C-p\C-s" 'web-previous-section)
    (define-key map "\e\C-p\C-u" 'web-previous-use)
    (define-key map "\e\C-r\C-m" 'web-rename-module)
    (define-key map "\e\C-v\C-c" 'web-view-changed-modules-list)
    (define-key map "\e\C-v\C-i" 'web-view-index)
    (define-key map "\e\C-v\C-m" 'web-view-module-names-list)
    (define-key map "\e\C-v\C-s" 'web-view-section-names-list)
    (define-key map "\e\C-w\C-c" 'web-what-change)
    (define-key map "\e\C-w\C-m" 'web-what-module)
    (define-key map "\e\C-w\C-s" 'web-what-section)
    (setq web-mode-map map)
  ) ; end let
) ; end if
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Here are the functions that comprise web-mode.el
;
(defun web-any-modules-undefined-initially ()
  "This function is invoked by \\[web-mode].  It checks to make sure that all
modules are defined.  If a module is undefined, a stub module is appended.
ARGUMENTS : None
GLOBAL Variables : web-module-name-defined-in-used-in and web-module-names
LOCAL Variable : ctr and ptr
RETURNS : Nothing
USES : web-append-a-stub-module and web-goto-module"
  (let ((ctr 0)
        (ptr 0)
       )
    (while (nth ctr web-module-names)
      (setq ptr (string-to-int (substring (nth ctr web-module-names)
        (+ (string-match "  " (nth ctr web-module-names)) 2))))
      (if (null (car (car (cdr (nth ptr web-module-name-defined-in-used-in)))))
      ; then
        (progn
          (ding)
          (message "Appending a module for %s"
            (substring (car (nth ptr web-module-name-defined-in-used-in)) 0 
              (length (car (nth ptr web-module-name-defined-in-used-in)))))
          (sit-for 2)
          (web-goto-module (car (car (cdr (cdr 
            (nth ptr web-module-name-defined-in-used-in))))))
          (setq module-number-of-stub (web-append-a-stub-module (substring 
            (car (nth ptr web-module-name-defined-in-used-in)) 0
            (length (car (nth ptr web-module-name-defined-in-used-in))))))
          (setcdr (nth ptr web-module-name-defined-in-used-in)
            (append (list (list module-number-of-stub))
            (list (car (cdr (cdr 
            (nth ptr web-module-name-defined-in-used-in)))))))
        ) ; end then
      ) ; end if
      (setq ctr (1+ ctr))
    ) ; end while
  ) ; end let
)

(defun web-append-a-stub-module (module-name)
  "This function is invoked when a new stub module needs to be inserted.  It
appends a module with no commentary, an index entry indicating that this module
is a stub, and the named module MODULE-NAME with no code.
ARGUMENT : module-name (required)
GLOBAL Variables : None
LOCAL Variables : module-number-of-stub
RETURNS : module-number-of-stub
USES : web-insert-index-entry, web-update-the-location-of-module-vector,
       web-update-the-module-name-defined-in-used-in-list, and web-what-module"
  (save-excursion
    (let ((module-number-of-stub))
      (if (re-search-forward "^@\\*" nil t)
      ; then position point at the end of the current section
        (forward-line -1)
      ; else position point at the end of the buffer 
      ;      (i.e., there is no next section)
        (goto-char (point-max))
        (newline)
      ) ; end if
      (newline)
      (insert-string "@ ")
      (web-update-the-location-of-module-vector ? )
      (web-insert-index-entry "R" "Stub")
      (insert-string (concat "@<" module-name "@>=\n"))
      (setq module-number-of-stub (web-what-module))
      (web-update-the-module-name-defined-in-used-in-list 
        module-number-of-stub)
      module-number-of-stub
    ) ; end let
  ) ; end excursion
)

(defun web-binary-search-of-names (names num-elements-in-names search-key len)
  "This is an internal support function that performs a binary search on the
list NAMES with NUM-ELEMENTS-IN-NAMES elements to find SEARCH-KEY of length 
LEN.
ARGUMENTS : names, num-elements-in-names, search-key, and len (all required)
LOCAL Variables : bottom, found, location, mid, and top
GLOBAL Variables : None
RETURNS : location
USES : Nothing"
  (let ((bottom (1- num-elements-in-names))
        found                               
        (location -1)                       
        (mid 0)
        (top 0)                             
       )
    (while (and (not (null names))
                (not found)
                (<= top bottom)
           )
      (setq mid (/ (+ top bottom) 2))
      (cond
        ((string= (substring search-key 0 len)
                  (substring (nth mid names) 0 
                    (min len (length (nth mid names)))))
          ; then we have a match
          (setq found t
                location mid)
        ) ; end case
        ((string< (substring search-key 0 len)
                  (substring (nth mid names) 0 
                    (min len (length (nth mid names)))))
          (setq bottom (1- mid))
        ) ; end case
        (t
          (setq top (1+ mid))
        ) ; end case
      ) ; end cond
    ) ; end while
    location ; return the location
  ) ; end let
)

(defun web-change-file ()
  "Makes the buffer that contains the CHange file the current buffer.
ARGUMENTS : None
LOCAL Variables : None
GLOBAL Variable : web-change-buffer-name
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
  (interactive)
  (web-journal "web-change-file")
  (web-check-if-buffer-is-one-of-the-web-files)
  (switch-to-buffer web-change-buffer-name)
)

(defun web-check-if-buffer-is-one-of-the-web-files ()
  "This function checks if the current buffer is one of the files included
in web-files.  If the current buffer is not one of the files include in
web-files, an error is signaled indicating that a user has attempted to use
one of the functions in web-mode in a buffer that \\[web-mode] was not used
on.
ARGUMENTS : None
GLOBAL Variables : web-default-directory and web-files
LOCAL Variable : ctr
RETURNS : pointer into the web-files list
USES : Nothing"
  (let ((ctr 0)
        current-buffer-is-one-of-the-web-files
       )
    (while (and (not current-buffer-is-one-of-the-web-files)
                (nth ctr web-files))
      (if (string-equal 
            (expand-file-name (nth ctr web-files) web-default-directory)
            (buffer-file-name))
      ; then
        (setq current-buffer-is-one-of-the-web-files t)
      ; else
        (setq ctr (1+ ctr))
      ) ; end if
    ) ; end while
    (if (not current-buffer-is-one-of-the-web-files)
    ; then
      (if (not (or (string= (buffer-name (current-buffer))
                     (concat "STATISTICS for " web-buffer-name "*"))
                   (string= (buffer-name (current-buffer))
                     "*Module Names*")
                   (string= (buffer-name (current-buffer))
                     web-index-buffer-name)
                   (string= (buffer-name (current-buffer))
                     "*CHanged Modules*")
                   (string= (buffer-name (current-buffer))
                     "*Module Name (Defined In) (Used In)*")
                   (string= (buffer-name (current-buffer))
                     "*Section Names*")
                   (string= (buffer-name (current-buffer))
                     "web-mode.jou")
               ) ; end or
          ) ; end not
      ; then
        (error "Attempting to use a function in a non-Web buffer.")
      ) ; end if
    ; else
      ctr ; return the location of the current-buffer in web-files
    ) ; end if
  ) ; end let
)

(defun web-collect-list-of-changed-module-numbers ()
  "This is an internal support function used by \\[web-edit-module],
\\[web-view-changed-modules-list], and \\[web-what-change].  It returns a list
of the module numbers in the Web being edited that have been changed in the
CHange file.
ARGUMENTS : None
GLOBAL Variables : web-change-buffer-name and web-buffer-name
LOCAL Variables : change-begins-at, changed-module, discrepancies,
                  discrepancies-begin-at, line-in-change-file, and
                  line-no-in-web
RETURNS : Nothing
USES : web-what-module"
  (let ((change-begins-at 0)       
        (changed-module 0)         
        (ctr 0)
        (discrepancies 0)          
       	(discrepancies-begin-at 0) 
        line-in-change-file        
        (line-no-in-web 1)          
      )
    (message "Determining which modules have been CHanged...")
    (save-excursion
      (set-buffer web-change-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^@\\(x\\|y\\|z\\)" nil t)
          (if (or (char-equal (downcase (string-to-char (buffer-substring
                  (match-beginning 1) (match-end 1)))) ?y)
                  (char-equal (downcase (string-to-char (buffer-substring
                  (match-beginning 1) (match-end 1)))) ?z)
              ) ; end condition
          ; then
            (error "CHange beginning on line %d in %s is missing an @x." 
              (count-lines (point-min) (point)) web-change-buffer-name)
          ; else
            (setq change-begins-at (count-lines (point-min) (point)))
            (forward-line 1)
            (if (eobp) (error "CHange file ended before @y."))
            (setq line-in-change-file (buffer-substring (point)
              (save-excursion (end-of-line) (point))))
            (save-excursion ; find this line's position in the WEB file
              (set-buffer web-buffer-name)
              (save-excursion
                (goto-line line-no-in-web)
                (if (not (search-forward line-in-change-file nil t))
                ; then couldn't find line from the change file
                  (error "WEB file ended during CHange.")
                ; else did find the line from the change file
                  (setq line-no-in-web (count-lines (point-min) (point))
                        changed-module (web-what-module))
                ) ; end if
              ) ; end excursion
            ) ; end excursion
            (setq line-no-in-web (1+ line-no-in-web)
                  discrepancies 0)
            (forward-line 1)
            (if (eobp) (error "CHange file ended before @y."))
            (while (not (looking-at "^@\\(x\\|y\\|z\\)"))
              (if (not 
                    (string-equal
                      (buffer-substring (point) (save-excursion (end-of-line)
                        (point)))
                      (save-excursion
                        (set-buffer web-buffer-name)
                        (save-excursion
                          (goto-line line-no-in-web)
                          (if (eobp) (error "WEB ended during CHange."))
                          (buffer-substring (point) 
                            (save-excursion (end-of-line)
                            (point)))
                        ) ; end excursion
                      ) ; end excursion
                    ) ; end string-equal
                  ) ; end not
              ; then
                (progn
                  (if (zerop discrepancies)
                  ; then
                    (setq discrepancies-begin-at line-no-in-web)
                  ) ; end if
                  (setq discrepancies (1+ discrepancies))
                ) ; end then
              ) ; end if
              (setq line-no-in-web (1+ line-no-in-web))
              (forward-line 1)
              (if (eobp) (error "CHange file ended during CHange."))
            ) ; end while
            (if (or (char-equal (downcase (string-to-char (buffer-substring
                      (match-beginning 1) (match-end 1)))) ?x)
                    (char-equal (downcase (string-to-char (buffer-substring
                      (match-beginning 1) (match-end 1)))) ?z)
                ) ; end condition
            ; then
              (error "The @x located on line %d in %s has no matching @y."
                change-begins-at web-change-buffer-name)
            ; else scan forward to the next @
              (if (not (zerop discrepancies))
              ; then
                (if (= discrepancies 1)
                ; then
                  (error 
                   "Hmm... %d line beginning at line %d in %s failed to match."
                    discrepancies discrepancies-begin-at web-buffer-name)
                ; else
                  (error 
                  "Hmm... %d lines beginning at line %d in %s failed to match."
                  discrepancies discrepancies-begin-at web-buffer-name)
                ) ; end if            
              ) ; end if
              (forward-line 1)
              (if (eobp) (error "CHange file ended before @z."))
              (while (not (looking-at "^@\\(x\\|y\\|z\\)"))
                (forward-line 1)
                (if (eobp) (error "CHange file ended before @z."))
              ) ; end while
              (if (or (char-equal (downcase (string-to-char (buffer-substring
                        (match-beginning 1) (match-end 1)))) ?x)
                      (char-equal (downcase (string-to-char (buffer-substring
                        (match-beginning 1) (match-end 1)))) ?y)
                  ) ; end condition
              ; then
                (error "The @x located on line %d in %s has no matching @z."
                  change-begins-at web-change-buffer-name)
              ; else
                (end-of-line)
              ) ; end if
              (aset (aref web-location-of-module changed-module) 3 1)
              (if (zerop (aref (aref web-location-of-module changed-module) 4))
              ; then
                (save-excursion
                  (goto-line change-begins-at)
                  (forward-char 2)
                  (setq change-begins-at (point))
                  (goto-char (point-min))
                  (setq ctr 0)
                  (while (re-search-forward "^@x" change-begins-at t)
                    (setq ctr (1+ ctr))
                  ) ; end while
                  (aset (aref web-location-of-module changed-module) 4 ctr)
                ) ; end excursion
              ) ; end if                                    
            ) ; end if
          ) ; end while
        ) ; end while
      ) ; end excursion
    ) ; end excursion
    (message "Determining which modules have been CHanged...done")
  ) ; end let
)

(defun web-collect-location-of-modules ()
  "This function actually initializes the vector web-location-of-module.  The
vector consists of vectors.  The i-th vector gives information about the i-th
module in the Web.  Each of these vectors has three elements: the first is a
index into the list web-files which is indicative of the file that the i-th
module is located; the secend the i-th module's position relative to the
beginning of the file that it is located in; and the third element is
indicative of the section that the i-th module is in.
ARGUMENTS : NONE
GLOBAL Variables : web-location-of-module, web-number-of-modules-in-file, 
                   web-files, and web-mode-map
LOCAL Variables : file-number, include-file-name, module-number, and 
                  section-number
RETURNS : Nothing
USES : web-collect-location-of-modules and web-eliminate-white-space"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@\\(i\\| \\|\n\\|\t\\|\\*\\)" nil t)
      (if (char-equal ?i
        (string-to-char (buffer-substring (match-beginning 1) (match-end 1))))
      ; then
        (progn
          (setq include-file-name (buffer-substring (point)
            (save-excursion (end-of-line) (point))))
          (setq include-file-name (substring include-file-name 0
            (string-match " " include-file-name 1)))
          (setq web-files (append web-files (list
            (web-eliminate-white-space include-file-name))))
          (setq file-number (cons (1- (length web-files)) file-number))
          (save-excursion
            (find-file-noselect (expand-file-name 
              (nth (car file-number) web-files) web-default-directory))
            (set-buffer (get-file-buffer (expand-file-name 
              (nth (car file-number) web-files) web-default-directory)))
            (setq major-mode 'Web-mode
                  mode-name "Web")
            (use-local-map web-mode-map)
            (web-collect-location-of-modules)
          ) ; end excursion
        ) ; end then
      ; else
        (if (char-equal ?* 
         (string-to-char (buffer-substring (match-beginning 1) (match-end 1))))
        ; then 
          (setq section-number (1+ section-number))
        ) ; end if
        (aset web-number-of-modules-in-file (car file-number) 
          (1+ (aref web-number-of-modules-in-file (car file-number))))
        (aset web-location-of-module module-number 
          (vector (car file-number) 
                  (aref web-number-of-modules-in-file (car file-number))
                  section-number
                  0  ; has it changed
                  0  ; if changed, the relative position in the CHange file
          ) ; end vector
        ) ; end aset
        (setq module-number (1+ module-number))      
      ) ; end if
    ) ; end while
    (setq file-number (cdr file-number))
  ) ; end excursion
)

(defun web-collect-module-names ()
  "This function is invoked by \\[web-mode].  It collects the list
module-name-defined-i9n-used-in list.  This function can be invoked by the user
if the list is in error (e.g., if a module name has been added but then the
user deletes it--\\[web-mode] has no way of knowing when a module name or a
module has been deleted
ARGUMENTS : None
GLOBAL Variables : web-module-name-defined-in-used-in, web-module-names,
                   web-number-of-module-names, web-default-directory,
                   and web-files
LOCAL Variable : module-number
RETURNS : Nothing
USES : web-collect-module-names-in-buffer, web-journal, and 
       web-write-module-names-to-a-file"
  (interactive)
  (web-journal "web-collect-module-names")
  (let ((module-number 1))
    (setq web-module-name-defined-in-used-in ()
          web-module-names () 
          web-number-of-module-names 0)
    (save-excursion
      (set-buffer (get-file-buffer (expand-file-name (nth 1 web-files)
        web-default-directory)))
      (web-collect-module-names-in-buffer)
    ) ; end excursion
    (web-write-module-names-to-a-file)
  ) ; end let
)

(defun web-collect-module-names-in-buffer ()
  "This function is used to collect a list of all valid module names.  The list
takes the form
  ((\"Module Name 1\" (Defined in Module #'s) (Used in Module #'s))
   (\"Module Name 2\" (Defined in Module #'s) (Used in Module #'s))
   ...
   (\"Module Name n\" (Defined in Module #'s) (Used in Module #'s))
  )
First it must determine the beginning and ending points of where a module name
begins and ends.  Once it has the name isolated, it checks the last three
characters.  If the last three characters are an ellipsis the length of the
module name is set to the number of characters between the starting and ending
positions less 3; otherwise the length is set to the number of characters
between the starting and ending positons of the module name.  The list as
mentioned above is not kept in alphabetical order.  The module names themselves
are maintained in a separate list in alphabetical order.  Each module name has
an integer appended to it which is used as a pointer into the list as mentioned
above.  This module name is then compared with the module names already in the
list that contains only module names.  If no match is found, the new module
name is inserted into both lists.  If a match is found, update either the
Defined In or Used In list that accompanies the module name in the big list.
ARGUMENTS : None
GLOBAL Variables : web-module-name-defined-in-used-in, web-module-names,
                   web-number-of-module-names, web-default-directory,
                   and web-module-name
LOCAL Variables : char-after-at-greater, include-file-name, len, location, 
                  module-name, module-number, next-module-begins, and 
                  number-of-modules
RETURNS : web-module-names
USES : web-binary-search-of-names, web-collect-module-names-in-buffer,
       and web-eliminate-white-space"
  (let (char-after-at-greater
        (len 0)
        include-file-name
        (location -1)
        module-name
        (next-module-begins (point))
       )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@\\(i\\| \\|\n\\|\t\\|\\*\\)" nil t)
        (if (char-equal ?i (string-to-char (buffer-substring 
               (match-beginning 1) (match-end 1))))
        ; then
          (save-excursion
            (setq include-file-name (buffer-substring (point)
              (save-excursion (end-of-line) (point))))
            (setq include-file-name 
              (substring include-file-name 0
              (string-match " " include-file-name 1)))
            (set-buffer (get-file-buffer (expand-file-name
              (web-eliminate-white-space include-file-name)
              web-default-directory)))
            (web-collect-module-names-in-buffer)
          ) ; end excursion
        ; else
          (save-excursion    
            (if (re-search-forward "^@\\(i\\| \\|\n\\|\t\\|\\*\\)" nil t)
            ; then
              (setq next-module-begins (point))
            ; else
              (setq next-module-begins (point-max))
            ) ; end if
          ) ; end excursion
          (message "Collecting module names in Module %d" module-number)
          (while (re-search-forward web-module-name next-module-begins t)
            (setq module-name (web-eliminate-white-space
              (buffer-substring (match-beginning 2) (match-end 2))))
            (setq len (length module-name))
            (if (and (> len 3) (string-equal "..."
                  (substring module-name (- len 3) len)))
            ; then
              (setq len (- len 3))
            ) ; end if
            (if (looking-at "=")
            ; then
              (setq char-after-at-greater ?=)
            ; else
              (setq char-after-at-greater ? )
            ) ; end if
            (setq location 
              (web-binary-search-of-names web-module-names 
              web-number-of-module-names module-name len))
            (if (= location -1)
            ; then a new module name has been encountered
              (progn
                (setq web-module-names (sort (append web-module-names 
                        (list (concat module-name
                        "  " (int-to-string web-number-of-module-names))))
                        'string<)
                      web-number-of-module-names 
                        (1+ web-number-of-module-names))
                (if (char-equal ?= char-after-at-greater)
                ; then this new module was defined (i.e., @<...@>=)
                  (setq web-module-name-defined-in-used-in 
                    (append web-module-name-defined-in-used-in
                    (list (list module-name (list module-number)
                    (list ())))))
                ; else this new module was used before defined
                  (setq web-module-name-defined-in-used-in 
                    (append web-module-name-defined-in-used-in
                    (list (list module-name (list ()) 
                    (list module-number)))))
                ) ; end if
              ) ; end then
            ; else the module name is already in list
              (setq location (string-to-int 
                (substring (nth location web-module-names) 
                (+ (string-match "  " (nth location web-module-names)) 2))))
              (if (char-equal ?= char-after-at-greater)
              ; then an existing module name has been redefined
                (if (null (car (car (cdr 
                      (nth location web-module-name-defined-in-used-in)))))
                ; then
                  (setcdr (nth location web-module-name-defined-in-used-in) 
                    (append (list (list module-number))
                    (cdr (cdr (nth 
                    location web-module-name-defined-in-used-in)))))
                ; else
                  (setcdr (nth location web-module-name-defined-in-used-in) 
                    (append (list (append 
                    (car (cdr (nth 
                    location web-module-name-defined-in-used-in)))
                    (list module-number)))
                    (cdr (cdr (nth 
                    location web-module-name-defined-in-used-in)))))
                ) ; end if
              ; else an existing module has been used
                (if (null (car (car (cdr (cdr 
                      (nth location web-module-name-defined-in-used-in))))))
                ; then
                  (setcdr (nth location web-module-name-defined-in-used-in) 
                    (append (list (car (cdr 
                    (nth location web-module-name-defined-in-used-in))))
                    (list (list module-number))))
                ; else
                  (setcdr (nth location web-module-name-defined-in-used-in) 
                    (append (list (car (cdr 
                    (nth location web-module-name-defined-in-used-in))))
                    (list (append (car (cdr (cdr 
                    (nth location web-module-name-defined-in-used-in))))
                    (list module-number)))))
                ) ; end if
              ) ; end if
            ) ; end if
          ) ; end while
          (setq module-number (1+ module-number))
        ) ; end if
      ) ; end while
    ) ; end excursion
  ) ; end let
)

(defun web-collect-section-names ()
  "This function collects a list of the major section names used in the Web.
It also keeps track of the module number where each major section begins.
ARGUMENTS: None
GLOBAL Variables : None
LOCAL Variables : ctr and section-names
RETURNS : section-names
USES : web-count-sections, web-eliminate-white-space, web-goto-section, 
       and web-what-module" 
  (let ((ctr 1)
        (num-of-sections (web-count-sections))
        section-names
       )
    (save-excursion
      (while (<= ctr num-of-sections)
        (web-goto-section ctr)
        (re-search-forward "^@\\*\\(.*\\)\\.\\( \\|\n\\)" nil t)
        (setq section-names (append section-names (list
          (list (web-eliminate-white-space (buffer-substring 
          (match-beginning 1) (match-end 1)))
          (web-what-module)))))
        (setq ctr (1+ ctr))
      ) ; end while
    ) ; end excursion
    section-names ; return the list
  ) ; end let
)

(defun web-convert-list-to-string (list-of-numbers)
  "This is an internal support function that accepts a LIST-OF-NUMBERS and
converts it into a string.
ARGUMENT : list-of-numbers (required)
GLOBAL Variables : None
LOCAL Variables : ctr and list-as-string
RETURNS : list-as-string
USES : Nothing"
  (let ((ctr 0)
        list-as-string)
    (while (not (null (nth ctr list-of-numbers)))
      (setq list-as-string (concat list-as-string 
        (int-to-string (nth ctr list-of-numbers)) " "))
      (setq ctr (1+ ctr))
    ) ; end while
    (if (> (length list-as-string) 1)
    ; then eliminate the trailing space
      (setq list-as-string
        (substring list-as-string 0 (1- (length list-as-string))))
    ) ; end if
    list-as-string ; return the list of numbers as a string
  ) ; end let
)

(defun web-count-changes ()
  "Count the number of changes in the CHange file.
ARGUMENTS : None
GLOBAL Variable : web-location-of-module
LOCAL Variables : count, index, and number-of-modules
RETURNS : count
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-modules, 
       and web-journal"
  (interactive)
  (web-journal "web-count-changes")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((count 0)
        (index 0)
        (number-of-modules (web-count-modules))
       )
    (while (< index number-of-modules)
      (if (not (zerop (aref (aref web-location-of-module index) 3)))
      ; then
        (setq count (1+ count))
      ) ; end if
      (setq index (1+ index))
    ) ; end while
    (message "%d modules have been CHanged." count)
    count ; return the number of changes in the web
  ) ; end let
)

(defun web-count-modules ()
  "Count the number of modules in the Web file.
ARGUMENTS : None
GLOBAL Variables : web-number-of-modules-in-file and web-files
LOCAL Variable : ctr and module-count
RETURNS : module-count
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
  (interactive)
  (web-journal "web-count-modules")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((module-count 0)
        (ctr 1)
       )
    (while (<= ctr (length web-files))
      (setq module-count (+ module-count 
              (aref web-number-of-modules-in-file ctr))
            ctr (1+ ctr))
    ) ; end while      
    (message "The number of modules in %s is %d"
       (upcase (nth 1 web-files)) module-count)
    module-count ; return the number of modules in the web
  ) ; end let
)

(defun web-count-sections ()
  "Count the number of sections in the Web file.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : module-count
RETURNS : the number of sections
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-modules, and
       web-journal"
  (interactive)
  (web-journal "web-count-sections")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((module-count (web-count-modules)))
    (message "The number of sections in %s is %d"
       (upcase (nth 1 web-files)) 
       (aref (aref web-location-of-module module-count) 2))
    (aref (aref web-location-of-module module-count) 2) ; return # of sections
  ) ; end let
)

(defun web-count-the-matches (search-string)
  "This is an internal support function that counts the number of occurrences
of SEARCH-STRING in the Web files.
ARGUMENT : search-string (required)
GLOBAL Variables : done, file-number, module-count, stopping-point, and
                    web-default-directory
LOCAL Variable : file-no and include-file-name
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-the-matches, and
        web-eliminate-white-space"
  (save-excursion
    (let ((file-no (web-check-if-buffer-is-one-of-the-web-files))
          include-file-name
         )
      (goto-char (point-min))
      (while (and (not done)
                  (re-search-forward search-string
                    (aref stopping-point file-no) t)
             ) ; end condition
        (if (char-equal ?i 
              (string-to-char (buffer-substring (match-beginning 1)
          (match-end 1))))
        ; then
          (save-excursion
            (setq include-file-name (buffer-substring (point)
              (save-excursion (end-of-line) (point))))
            (setq include-file-name (substring include-file-name 0
              (string-match " " include-file-name 1)))
            (set-buffer (get-file-buffer (expand-file-name 
              (web-eliminate-white-space include-file-name)
              web-default-directory)))
            (web-count-the-matches "^@\\(i\\| \\|\t\\|\n\\|\\*\\)")
          ) ; end excursion
        ; else
          (setq module-count (1+ module-count))
        ) ; end if
      ) ; end while
      (if (= file-no file-number) 
      ; then we are done
        (setq done t)
      ) ; end if
    ) ; end let
  ) ; end excursion
)
 
(defun web-delimiter-match-check ()
  "This function checks if all module delimiters (namely, @<...@>) are matched.
It records any missing delimiter pairs, whether they be opening or closing
pairs that are missing.  If any missing delimiters are discovered, a window
pops up with the pertinent information.  The user then needs to rectify the
situation and re-invoke \\[web-mode].
ARGUMENTS : None
GLOBAL Variable : web-module-begins
LOCAL Variables : balance, bound-for-search, closing-delimiters,
                  next-module-begins, opening-char, opening-delimiters,
                  and valid-delimiter
RETURNS : balance (boolean indicating if module delimiters are balanced)
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
  (interactive)
  (web-journal "web-delimiter-match-check")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((balance t)
        (bound-for-search 1)
        closing-delimiters
        (module-number 1)
        (next-module-begins 0)
        opening-char
        opening-delimiters
        valid-delimiter
       )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward web-module-begins nil t)
        (message "Checking balance in Module %d" module-number)
        (setq bound-for-searc (point))
        (save-excursion
          (if (not (re-search-forward web-module-begins nil t))
          ; then
            (setq next-module-begins (point-max))
          ; else
            (goto-char (match-beginning 0))
            (setq next-module-begins (point))
          ) ; end if
        ) ; end excursion
        (while (re-search-forward "\\(@@\\)*@\\(<\\|>\\)" 
                next-module-begins t)
          (save-excursion
            (goto-char (match-beginning 0))
            (if (char-equal (preceding-char) ?@)
            ; then
              (setq valid-delimiter nil)
            ; else
              (setq valid-delimiter t)
            ) ; end if
          ) ; end excursion
          (if valid-delimiter
          ; then
            (progn
              (cond
                ((char-equal (preceding-char) ?<)
                  (setq opening-delimiters (cons 
                    (count-lines (point-min) (point)) opening-delimiters))
                ) ; end case
                ((char-equal (preceding-char) ?>)
                  (save-excursion
                    (setq opening-char nil)
                    (while (and (null opening-char)
                           (re-search-backward 
                           "\\(@@\\)*@\\(\\^\\|\\.\\|:\\|t\\|=\\|<\\)" 
                           bound-for-search t))
                      (if (char-equal (preceding-char) ?@)
                      ; then keep searching backward
                        ()
                      ; else
                        (setq opening-char (string-to-char (buffer-substring
                          (match-beginning 2) (match-end 2))))
                      ) ; end if
                    ) ; end while
                  ) ; end excursion
                  (cond
                    ((null opening-char) ; then missing an opening delimiter
                      (setq closing-delimiters (append closing-delimiters
                        (list (count-lines (point-min) (point)))))
                    ) ; end case
                    ((char-equal opening-char ?<) ; valid module name
                      (setq opening-delimiters (cdr opening-delimiters))
                    ) ; end case
                  ) ; end cond
                  (setq bound-for-search (point))
                ) ; end case
              ) ; end cond
            ) ; end then
          ) ; end if
        ) ; end while
        (setq module-number (1+ module-number))
      ) ; end while
    ) ; end excursion
    (if (and (null opening-delimiters) (null closing-delimiters))
    ; then
      (message "All module delimiters match")
    ; else
      (setq balance nil)
      (with-output-to-temp-buffer "*Missing Delimiters*"
        (princ 
          "After these errors are fixed, reissue the command M-x web-mode.")
        (if (not (null opening-delimiters))
        ; then
          (if (nth 1 opening-delimiters)
          ; then
            (princ (format 
              "\nThe @<'s located on lines %s are missing an @>."
              (sort opening-delimiters '<)))
          ; else
            (princ (format "\nThe @< located on line %d is missing an @>."
              (car opening-delimiters)))
          ) ; end if
        ) ; end if
        (if (not (null closing-delimiters))
        ; then
          (if (nth 1 closing-delimiters)
          ; then
            (princ (format 
             "\nThe @>'s located on lines %s are missing an @<."
              closing-delimiters))
          ; else
            (princ (format "\nThe @> located on line %d is missing an @<."
              (car closing-delimiters)))
          ) ; end if
        ) ; end if
      ) ; end output to temp buffer
    ) ; end if
    balance ; return the boolean indicating if module delimiters balance
  ) ; end let
)

(defun web-determine-characteristics ()
  "This function is used to give the user an idea of how many lines of text
exist in each module's commentary, definition/macro, and code parts.
ARGUMENTS : None
GLOBAL Variables : web-default-directory and web-files
LOCAL Variables : code, code-begins, ctr, current-position, divider,
                  done, file-no, header, header1, header2, header3, header4,
                  line-in-table, lines-in-code, lines-in-commentary,
                  lines-in-macros, macros, macros-begin, max-module,
                  module-no, next-module-begins, number-of-columns,
                  space-to-left-of-table, spacer, spanner, stat-buffer-name,
                  title, and width-of-table
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-modules,
       web-determine-characteristics-for-buffer, and web-journal"
  (interactive)
  (web-journal "web-determine-characteristics")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let* ((module-no 1)
         (max-module (web-count-modules))
         (number-of-columns 4)
         (header1 "Module #")
         (header2 "Commentary")
         (header3 "Macros")
         (header4 "Code")
         (spanner "Number of Lines in")
         (title (concat "STATISTICS FOR " 
           (upcase (buffer-name (current-buffer)))))
         (spacer 2)
         (width-of-table (+ (length header1) (length header2) 
           (length header3) (length header4) (* number-of-columns spacer 2)
           (1+ number-of-columns)))
         (space-to-left-of-table (/ (- (screen-width) width-of-table) 2))
         (divider (concat
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ?_) "|"
           (make-string (+ (length header2) (* spacer 2)) ?_) "|"
           (make-string (+ (length header3) (* spacer 2)) ?_) "|"
           (make-string (+ (length header4) (* spacer 2)) ?_) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ? ) "|"
           (make-string (+ (length header2) (* spacer 2)) ? ) "|"
           (make-string (+ (length header3) (* spacer 2)) ? ) "|"
           (make-string (+ (length header4) (* spacer 2)) ? ) "|\n"))
         (header (concat 
           (make-string (/ (- (screen-width) (length title)) 2) ? )
             title "\n"
           (make-string (1+ (/ (- (screen-width) width-of-table) 2)) ? )
           (make-string (- width-of-table 2) ?_) "\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2))? ) "|"
           (make-string (+ (length header2) (length header3)
             (length header4) (* 3 spacer 2) 2) ? ) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ? ) "|"
           (make-string (/ (- (+ (length header2) (length header3)
             (length header4) (* 3 spacer 2) 2) (length spanner)) 2) ? )
           spanner
           (make-string (1- (- (- width-of-table (+ (length header1)
             (* spacer 2) 2)) (+ (/ (- (+ (length header2) (length header3)
             (length header4) (* 3 spacer 2) 2) (length spanner)) 2)
             (length spanner)))) ? ) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ? ) "|"
           (make-string (1- (- width-of-table (+ (length header1)
             (* spacer 2) 2))) ?_) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ? ) "|"
           (make-string (+ (length header2) (* spacer 2)) ? ) "|"
           (make-string (+ (length header3) (* spacer 2)) ? ) "|"
           (make-string (+ (length header4) (* spacer 2)) ? ) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string spacer ? ) header1
           (make-string spacer ? ) "|"
           (make-string spacer ? ) header2
           (make-string spacer ? ) "|"
           (make-string spacer ? ) header3
           (make-string spacer ? ) "|"
           (make-string spacer ? ) header4
           (make-string spacer ? ) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ?_) "|"
           (make-string (+ (length header2) (* spacer 2)) ?_) "|"
           (make-string (+ (length header3) (* spacer 2)) ?_) "|"
           (make-string (+ (length header4) (* spacer 2)) ?_) "|\n"
           (make-string space-to-left-of-table ? ) "|"
           (make-string (+ (length header1) (* spacer 2)) ? ) "|"
           (make-string (+ (length header2) (* spacer 2)) ? ) "|"
           (make-string (+ (length header3) (* spacer 2)) ? ) "|"
           (make-string (+ (length header4) (* spacer 2)) ? ) "|\n"))
         code
         (code-begins)
         (ctr 0)
         (current-position 0)
         done
         file-no
         line-in-table
         (lines-in-code 0)
         (lines-in-commentary 0)
         (lines-in-macros 0)
         macros
         (macros-begin 0)
         (next-module-begins 0)
         (stat-buffer-name (concat "*STATISTICS for " 
          (buffer-name (current-buffer)) "*"))
        )
    (generate-new-buffer stat-buffer-name)
    (save-excursion
      (set-buffer stat-buffer-name)
      (use-local-map web-mode-map)
      (setq major-mode 'Web-mode
            mode-name "Web")
      (goto-char (point-min))
      (kill-line (count-lines (point-min) (point-max)))
    ) ; end excursion
    (save-excursion
      (set-buffer (get-file-buffer (expand-file-name (nth 1 web-files)
        web-default-directory)))
      (setq file-no (cons (web-check-if-buffer-is-one-of-the-web-files) 
        file-no))
      (web-determine-characteristics-for-buffer)
    ) ; end excursion
    (switch-to-buffer stat-buffer-name)
    (goto-char (point-min))
  ) ; end let
) 

(defun web-determine-characteristics-for-buffer ()
  "This is an internal support function used by
\\[web-determine-characteristics] to write the characteristics of each
individual buffer to the buffer where they are being recorded.
ARGUMENTS : None
GLOBAL Variables : web-default-directory and web-files
LOCAL Variables : code, code-begins, ctr, current-position, divider,
                  done, file-no, header, header1, header2, header3, header4,
                  include-file-name,
                  line-in-table, lines-in-code, lines-in-commentary,
                  lines-in-macros, macros, macros-begin, max-module,
                  module-no, next-module-begins, number-of-columns,
                  space-to-left-of-table, spacer, spanner, stat-buffer-name,
                  title, and width-of-table
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files and 
       web-eliminate-white-space"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@\\(i\\| \\|\t\\|\n\\|\\*\\)" nil t)
      (if (char-equal ?i 
                      (string-to-char 
                        (buffer-substring (match-beginning 1) (match-end 1))))
      ; then switch to include file and collect its characteristics
        (save-excursion
          (setq include-file-name (buffer-substring (point)
            (save-excursion (end-of-line) (point))))
          (setq include-file-name (substring include-file-name 0
            (string-match " " include-file-name 1)))
          (set-buffer (get-file-buffer (expand-file-name 
            (web-eliminate-white-space include-file-name)
            web-default-directory)))
          (setq file-no (cons (web-check-if-buffer-is-one-of-the-web-files)
              file-no))
          (web-determine-characteristics-for-buffer)
        ) ; end excursion
      ; else continue collecting in the same buffer
        (setq current-position (count-lines (point-min) (point)))
        (save-excursion
          (if (re-search-forward "^@\\(i\\| \\|\t\\|\n\\|\\*\\)" nil t)
          ; then
            (setq next-module-begins (point))
          ; else
            (setq next-module-begins (point-max))
          ) ; end if
        ) ; end excursion
        (save-excursion
          (setq macros nil)
          (if (re-search-forward "@\\(d\\|f\\)" next-module-begins t)
          ; then
            (progn
              (backward-char 2)
              (setq done nil 
                    ctr 0)
              (while (null done)
                (while (looking-at "@")
                  (backward-char 1)
                  (setq ctr (1+ ctr))
                )
                (forward-char (+ ctr 2))
                (if (not (zerop (% ctr 2)))
                ; then
                  (setq macros t
                        done t
                        macros-begin (count-lines (point-min) (point)))
                ; else
                  (if (re-search-forward "@\\(d\\|f\\)" next-module-begins t)
                  ; then
                    (backward-char 2)
                  ; else
                    (setq done t)
                  ) ; end if
                ) ; end if
              ) ; end while
            ) ; end then
          ) ; end if
        ) ; end excursion
        (save-excursion
          (setq code nil)
          (if (re-search-forward "@\\(p\\|c\\|<\\)" next-module-begins t) 
          ; then
            (progn
              (backward-char 2)
              (setq done nil 
                    ctr 0)
              (while (null done)
                (while (looking-at "@")
                  (backward-char 1)
                  (setq ctr (1+ ctr))
                )
                (forward-char (+ ctr 2))
                (if (not (zerop (% ctr 2)))
                ; then
                  (setq code t
                        done t
                        code-begins (count-lines (point-min) (point)))
                ; else
                  (if (re-search-forward "@\\(<\\|p\\|c\\)" 
                    next-module-begins t)
                  ; then
                    (backward-char 2)
                  ; else
                    (setq done t)
                  ) ; end if
                ) ; end if
              ) ; end while
            ) ; end then
          ) ; end if
        ) ; end excursion
        (setq next-module-begins (count-lines 
          (point-min) next-module-begins))
        (cond
          ((and (null macros) (null code))
             (setq lines-in-commentary 
               (- next-module-begins current-position))
             (setq lines-in-macros 0)
             (setq lines-in-code 0)
          )
          ((and (null macros) (not (null code)))
             (setq lines-in-commentary (- code-begins current-position))
             (setq lines-in-macros 0)
             (setq lines-in-code (- next-module-begins code-begins))
          )
          ((and (not (null macros)) (null code))
             (setq lines-in-commentary (- macros-begin current-position))
             (setq lines-in-macros (- next-module-begins macros-begin))
             (setq lines-in-code 0)
          )
          ((and (not (null macros)) (not (null code)))
             (setq lines-in-commentary (- macros-begin current-position))
             (setq lines-in-macros (- code-begins macros-begin))
             (setq lines-in-code (- next-module-begins code-begins))
          )
        ) ; end cond
        (setq line-in-table (concat 
          (make-string space-to-left-of-table ? ) "|"
          (make-string spacer ? )
          (make-string (- (length header1)
            (length (int-to-string module-no))) ? )
          (int-to-string module-no)
          (make-string spacer ? ) "|"
          (make-string spacer ? )
          (make-string (- (length header2)
            (length (int-to-string lines-in-commentary))) ? )
          (int-to-string lines-in-commentary)
          (make-string spacer ? ) "|"
          (make-string spacer ? )
          (make-string (- (length header3)
            (length (int-to-string lines-in-macros))) ? )
          (int-to-string lines-in-macros)
          (make-string spacer ? ) "|"
          (make-string spacer ? )
          (make-string (- (length header4)
            (length (int-to-string lines-in-code))) ? )
          (int-to-string lines-in-code)
          (make-string spacer ? ) "|\n"
          ) ; ends concat
        ) ; ends setq
        (message "Module %d" module-no)
        (set-buffer stat-buffer-name)
        (if (= module-no 1) 
        ; then
          (progn
            (goto-char (point-min))
            (kill-line (count-lines (point-min) (point-max)))
            (insert-string header)
          ) ; end then
        ) ; end if
        (insert-string line-in-table)
        (cond
          ((= module-no max-module)
            (setq divider (concat 
              (make-string space-to-left-of-table ? ) "|"
              (make-string (+ (length header1) (* spacer 2)) ?_) "|"
              (make-string (+ (length header2) (* spacer 2)) ?_) "|"
              (make-string (+ (length header3) (* spacer 2)) ?_) "|"
              (make-string (+ (length header4) (* spacer 2)) ?_) "|\n"
              ) ; end concat
            ) ; end setq
            (insert-string divider)
          ) ; end case
          ((zerop (% module-no 3))
            (insert-string divider)
          ) ; end case
        ) ; end cond
        (setq module-no (1+ module-no))
        (set-buffer (get-file-buffer (expand-file-name 
          (nth (car file-no) web-files)  web-default-directory)))
      ) ; end if
    ) ; end while
  (setq file-no (cdr file-no))
  ) ; end excursion
)

(defun web-determine-module-name-ending ()
  "This function determines if the appropriate ending to a module name is @> or
@>=.
ARGUMENTS : None
GLOBAL Variable : web-module-begins
LOCAL Variables : at-greater-than-equal, at-p, module-name-ending,
                  next-module-begins, and this-module-begins
RETURNS : module-name-ending
USES : Nothing"
  (let (at-greater-than-equal
        at-p
        module-name-ending
        (next-module-begins 0)
        (this-module-begins 0)
       )
    (save-excursion
      (re-search-backward web-module-begins nil t)
      (setq this-module-begins (point))
    ) ; end excursion
    (save-excursion
      (if (re-search-forward web-module-begins nil t)
      ; then
        (progn
          (goto-char (match-beginning 0))
          (setq next-module-begins (point))
        ) ; end then
      ; else
        (setq next-module-begins (point-max))
      ) ; end if
    ) ; end excursion
    (save-excursion
      (goto-char this-module-begins) 
      (while (and (re-search-forward "\\(@@\\)*@\\(p\\|c\\|r\\|a\\|n\\)" 
                   next-module-begins t)
                  (null at-p))
        (if (not (char-equal ?@
            (string-to-char (buffer-substring 
            (1- (match-beginning 0)) (match-beginning 0)))))
        ; then it's valid
          (setq at-p t)
        ) ; end if
      ) ; end while
      (if at-p
      ; then do nothing
        ()
      ; else check for a valid @>=
        (while (and (re-search-forward "\\(@@\\)*@>=" 
                     next-module-begins t)
                    (null at-greater-than-equal))
          (if (not (char-equal ?@
              (string-to-char (buffer-substring 
              (1- (match-beginning 0)) (match-beginning 0)))))
          ; then it's valid
            (setq at-greater-than-equal t)
          ) ; end if
        ) ; end while
      ) ; end if
    ) ; end excursion
    (if (and (null at-greater-than-equal)
             (null at-p))
    ; then
      (setq module-name-ending "@>=")
    ; else
      (setq module-name-ending "@>")
    ) ; end if
    module-name-ending ; return the proper ending for the module name
  ) ; end let
)

(defun web-display-module-names (top bottom &optional arg)
  "This function is used to display a list of module names.  This function is
invoked by \\[web-options-for-module-name-insertion].  It displays the module
name of the TOP-th car of web-module-names through the BOTTOM-th car of
web-module-names.  An optional argument ARG is used to choose between one of
two messages displayed at the bottom of the window.
ARGUMENTS : top (required), bottom (required), and arg (optional)
GLOBAL Variables : web-module-names, web-number-of-lines-in-window, and 
                   web-number-of-module-names
LOCAL Variables : available-lines, ctr, module-name, phrase, and space-on-left
RETURNS : Nothing
USES : Nothing"
  (let ((available-lines (- web-number-of-lines-in-window 11))
        (ctr top)
        module-name
        phrase
        (space-on-left 
          (/ (- 6 (length (int-to-string web-number-of-module-names))) 2))
       )
    (if (null arg)
    ; then
      (setq phrase (format "Displaying %d Module Names of %d" 
        (1+ (- bottom top)) web-number-of-module-names))
    ; else
      (setq phrase (format "Displaying %d of %d Matches" 
        (1+ (- bottom top)) arg))
    )
    (goto-char (point-min))
    (forward-line 9)
    (kill-line (count-lines (point) (point-max)))
    (insert-string "Number  Name\n")
    (while (<= ctr bottom)
      (setq module-name (substring (nth (1- ctr) web-module-names) 0
        (string-match "  " (nth (1- ctr) web-module-names))))
      (if (> (length module-name) 71)
      ; then
        (setq module-name (concat (substring module-name 0 68) "..."))
      )
      (insert-string (concat (make-string (+ space-on-left
        (- (length (int-to-string web-number-of-module-names))
        (length (int-to-string ctr)))) ? ) (int-to-string ctr)
        (make-string (- 8 (+ space-on-left 
        (length (int-to-string web-number-of-module-names)))) ? )
        module-name "\n"))
      (setq ctr (1+ ctr))
    ) ; end while
    (insert-string (make-string (- available-lines (1+ (- bottom top))) ?\012))
    (insert-string (concat (make-string (/ (- 79 (length phrase)) 2) ? )
      phrase))
  ) ; end let
)

(defun web-edit-module ()
  "This function can only be invoked from the buffer containing the Web
document.  Whatever module point is positioned in is the module that is to be
changed.  If the module to be changed already appears in the CHange file, the
user is warned but is switched to the buffer containing the CHange file so that
further changes can be made.  If the module to be changed does not appear in
the CHange file, the module to be changed from the Web buffer is copied
verbatim at its proper position in the CHange file twice--once between @x and
@y and once between @y and @z.  Point is then positioned at the beginning of
the line following @y so that changes can be made.
ARGUMENTS : None
GLOBAL Variables : web-change-buffer-name and web-buffer-name
LOCAL Variables : comment-string, ctr, module-to-be-changed, 
                  module-to-be-changed-begins-at,
                  and module-to-be-changed-ends-at
RETURNS : Nothing
USES : web-count-changes, web-count-modules, 
       web-goto-change-corresponding-to-module, web-goto-module, web-journal,
       web-what-change, and web-what-module"
  (interactive)
  (web-journal "web-edit-module")
  (if (not (string-equal web-buffer-name (buffer-name (current-buffer))))
  ; then
    (error "Not in the Web buffer that web-mode was used on most recently.")
  ; else
    (let* ((ctr 0)
           done
           (module-to-be-changed (web-what-module))
           (module-to-be-changed-begins-at 0)
           (module-to-be-changed-ends-at 0)
           (comment-string (concat "  Module  " 
             (int-to-string module-to-be-changed) "  " 
             (current-time-string) "\n"))
           (total-number-of-modules (web-count-modules))
          )
      (if (= (aref (aref web-location-of-module module-to-be-changed) 3) 1)
      ; then the module has already been changed
        (progn
          (web-goto-change-corresponding-to-module module-to-be-changed)
          (message "Module %d has already been CHanged." module-to-be-changed)
        ) ; end then
      ; else
        (if (zerop module-to-be-changed)
        ; then the limbo portion of the Web is to be changed
          (setq module-to-be-changed-begins-at 1)
        ; else
          (save-excursion
            (web-goto-module module-to-be-changed)
            (setq module-to-be-changed-begins-at (point))
          ) ; end excursion
        ) ; end if
        (if (= module-to-be-changed (web-count-modules))
        ; then the last module is being changed
          (setq module-to-be-changed-ends-at (1- (point-max)))
        ; else
          (save-excursion
            (web-goto-module (1+ module-to-be-changed))
            (backward-char 1)
            (while (looking-at "\n") (backward-char 1))
            (setq module-to-be-changed-ends-at (1+ (point)))
          ) ; end excursion
        ) ; end if
        (setq ctr (1+ module-to-be-changed))
        (while (not done)
          (cond
            ((or (> ctr total-number-of-modules)
                 (= (aref (aref web-location-of-module ctr) 3) 1))
              (setq done t)
            ) ; end case
            (t
             (setq ctr (1+ ctr)
                   done nil)
            ) ; end case
          ) ; end cond
        ) ; end while
        (switch-to-buffer web-change-buffer-name)
        (goto-char (point-min))
        ; position point in the CHange file for the insertion of the
        ; contents of the module that is to be changed.  Update the
        ; web-location-of-module vector.
        (cond
          ((zerop (web-count-changes))
            (goto-char (point-max))
            (aset (aref web-location-of-module module-to-be-changed) 3 1)
            (aset (aref web-location-of-module module-to-be-changed) 4 1)
          ) ; end case
          ((= ctr (1+ total-number-of-modules))
            ; then the new change goes at the bottom of the CHange file
            (goto-char (point-max))
            ; update the web-location-of-module vector
            (aset (aref web-location-of-module module-to-be-changed) 3 1)
            (save-excursion
              (re-search-backward "^@z" nil t)
              (aset (aref web-location-of-module module-to-be-changed) 4
                (1+ (aref (aref web-location-of-module (web-what-change)) 4)))
            ) ; end excursion
          ) ; end case
          (t
            (re-search-forward "^@x" nil t ctr)
            (goto-char (match-beginning 0))
            (recenter 0)
            (aset (aref web-location-of-module module-to-be-changed) 3 1)
            (aset (aref web-location-of-module module-to-be-changed) 4
              (aref (aref web-location-of-module ctr) 4))
            (while (<= ctr total-number-of-modules)
              (if (not (zerop (aref (aref web-location-of-module ctr) 3)))
              ; then increment the relative position in the CHange file
                (aset (aref web-location-of-module ctr) 4 
                  (1+ (aref (aref web-location-of-module ctr) 4)))
              ) ; end if
              (setq ctr (1+ ctr))
            ) ; end while
          ) ; end case
        ) ; end cond
        (if (re-search-backward "^@z" nil t)
        ; then
          (progn
            (forward-line 1)
            (if (not (looking-at "\n"))
            ; then
              (newline)
            ; else
              (forward-line)
            ) ; end if
          ) ; end then
        ) ; end if
        (insert "@x" comment-string)
        (insert-buffer-substring web-buffer-name 
          module-to-be-changed-begins-at (1+ module-to-be-changed-ends-at))
        (insert "@y" comment-string)
        (insert-buffer-substring web-buffer-name 
          module-to-be-changed-begins-at (1+ module-to-be-changed-ends-at))
        (insert "@z" comment-string "\n")
        (re-search-backward "^@y" nil t)
        (recenter 0)
        (message "Module %d is ready to be changed." module-to-be-changed)
      ) ; end if
    ) ; end let
  ) ; end if
)

(defun web-eliminate-control-sequences (search-string 
  &optional replacement-string)
  "This is an internal support function used to eliminate delimited control
sequences.  The function searches for SEARCH-STRING replaces these delimited
control sequences with their argument.  An optional REPLACEMENT-STRING will be
appended to the argument of the control sequence if present.  This function is
used when the index is being reformatted.  For example, the index contains
entries such as \[#] (where # represents a module number where the index entry
was referenced) which are to be underlined in the final output.  This function
replaces the sequence with #.
ARGUMENTS : search-string (required) and replacement-string (optional)
GLOBAL Variables : None
LOCAL Variables : None
RETURNS : Nothing
USES : Nothing"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward search-string nil t)
      (backward-char (length (buffer-substring (match-beginning 0)
         (match-end 0))))
      (insert-string (concat (buffer-substring (match-beginning 1)
         (match-end 1)) replacement-string))
      (delete-char (length (buffer-substring (match-beginning 0)
         (match-end 0))))
    ) ; end while
  ) ; end excursion
)

(defun web-eliminate-white-space (string)
  "This is an internal function used to eliminate all leading, trailing, and
multiple inner occurrences of white space (tabs, carriage returns, and spaces)
in STRING.  The resulting string will always have a leading space, and hence,
must be deleted before returning.  If the last character in the input string is
white space, a trailing space will also be present, and it, too, must be
deleted before returning.  This code adapted from Knuth (see Module 153 in
tangle.web).
ARGUMENT : string (required)
GLOBAL Variables : None
LOCAL Variables : d, k, loc, new-string
RETURNS : new-string
USES : Nothing"
  (let (d
        (k 1)
        (loc 1)
        (new-string " ")
       )
    (while (<= loc (length string))
      (setq d (substring string (1- loc) loc)
            loc (1+ loc) 
            k (1+ k))
      (if (or (string-equal d " ")
              (string-equal d "\n")
              (string-equal d "\t"))
      ; then
        (progn
          (setq d " ")
          (if (string-equal (substring new-string (- k 2) (1- k)) " ")
          ; then
            (setq k (1- k))
          ) ; end if
        ) ; end then
      ) ; end if
      (setq new-string (concat (substring new-string 0 (1- k)) d))
    ); end while
    (if (string-equal (substring new-string (1- k) k) " ")
    ; then
      (setq new-string (substring new-string 0 (1- k)))
    ) ; end if
    (substring new-string 1 (length new-string)) ; return the new string
  ) ; end let
)

(defun web-file ()
  "Makes the buffer that contains the Web document the current buffer.
ARGUMENTS : None
LOCAL Variables : None
GLOBAL Variables : web-buffer-name
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
  (interactive)
  (web-journal "web-file")
  (web-check-if-buffer-is-one-of-the-web-files)
  (switch-to-buffer web-buffer-name)
)

(defun web-goto-change-corresponding-to-module (goto-change-number)
  "This function positions point at the beginning of the change that
corresponds to module GOTO-CHANGE-NUMBER.  If no prefix argument is provided,
the function prompts for one.
ARGUMENT : goto-change-number (required)
GLOBAL Variables : None
LOCAL Variables : None
RETURNS : Nothing
USES : web-change-file, web-count-modules, and web-journal"
  (interactive "NGoto change corresponding to module: ")
  (web-journal "web-goto-change-corresponding-to-module")
  (cond
    ((or (< goto-change-number 0)
         (> goto-change-number (web-count-modules)))
      (error "There is no change that corresponds to %d." goto-change-number)
    ) ; end case
    ((zerop (aref (aref web-location-of-module goto-change-number) 3))
      (error "Module %d has not been CHanged." goto-change-number)
    ) ; end case
    (t
      (web-change-file)
      (goto-char (point-min))
      (re-search-forward "^@y" nil t
        (aref (aref web-location-of-module goto-change-number) 4))
      (goto-char (match-beginning 0))
      (recenter 0)
      (message "CHange for module %d" goto-change-number)
    )  ; end case
  )  ; end cond
)

(defun web-goto-module (goto-module-number)
  "This function positions point at the beginning of the GOTO-MODULE-NUMBER
module in the Web document.  If no argument is provided, the user is prompted
for one.  The function will not permit a user to go to a module labelled 0 or
less; nor will it permit a user to go to a module that exceeds the total number
of modules.
ARGUMENT : goto-module-number (required)
GLOBAL Variables : web-location-of-module, web-files, web-default-directory,
                   web-module-begins, and web-module-changed-then-goto-change
LOCAL Variable : tot-num-modules
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-modules, 
       and web-journal"
  (interactive "NGoto module: ")
  (web-journal "web-goto-module")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((tot-num-modules (web-count-modules)))
    (cond
      ((< goto-module-number 1)
        (error "There is no module %d." goto-module-number)
      ) ; end case
      ((> goto-module-number tot-num-modules)
        (error "Can't go to module %d. There are only %d modules."
          goto-module-number tot-num-modules)
      ) ; end case
      (t
        (if web-module-changed-then-goto-change
        ; then
          (progn
            (web-change-file)
            (goto-char (point-min))
            (re-search-forward "^@y" nil t
              (aref (aref web-location-of-module goto-module-number) 4))
            (goto-char (match-beginning 0))
            (recenter 0)
          ) ; end then
        ; else
          (switch-to-buffer (get-file-buffer (expand-file-name
            (nth (aref (aref web-location-of-module goto-module-number) 0) 
            web-files) web-default-directory)))
          (goto-char (point-min))
          (re-search-forward web-module-begins nil t 
            (aref (aref web-location-of-module goto-module-number) 1))
          (goto-char (match-beginning 0))
          (recenter 0)
        ) ; end if
        (message "Module %d in %s. %s" goto-module-number (nth 1 web-files)
          (if (= (aref (aref web-location-of-module goto-module-number) 3) 1)
          "This module has been CHanged." " "))
      )  ; end case
    )  ; end cond
  ) ; end let
)

(defun web-goto-section (goto-section-number)
  "This function positions point at the beginning of the GOTO-SECTION-NUMBER
section in the Web document.  If no argument is provided, the user is prompted
for one.  The function will not permit a user to go to a section labelled 0 or
less; nor will it permit a user to go to a section that exceeds the total
number of sections.  If the current buffer is not the buffer containing the Web
document, the buffer containing the Web document is made the current buffer.
ARGUMENT : goto-section-number (required)
GLOBAL Variables : web-location-of-module and web-files
LOCAL Variable : ctr and tot-num-sections
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-sections, 
       web-goto-module, and web-journal"
  (interactive "NGoto Section Number:  ")
  (web-journal "web-goto-section")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((tot-num-sections (web-count-sections))
        (ctr 1)
       )
    (cond
      ((< goto-section-number 1)
        (error "There is no section %d." goto-section-number)
      )
      ((> goto-section-number tot-num-sections)
        (error "Can't go to section %d. There are only %d sections."
          goto-section-number tot-num-sections)
      )
      (t
        (while (/= (aref (aref web-location-of-module ctr) 2) 
                   goto-section-number)
          (setq ctr (1+ ctr))
        ) ; end while
        (web-goto-module ctr)
        (message "Section %d in %s" goto-section-number
          (upcase (nth 1 web-files)))
      )  ; end case
    )  ; end cond
  ) ; end let
)

(defun web-include-file (include-file-number)
  "This function switches to the buffer containing the contents of include file
INCLUDE-FILE-NUMBER.
ARGUMENTS include-file-number (required)
GLOBAL Variables : web-default-directory and web-files
LOCAL Variables : None
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
  (interactive "NInclude File to be switch to:  ")
  (web-journal "web-include-file")
  (web-check-if-buffer-is-one-of-the-web-files)
  (if (or (< include-file-number 1)
          (> include-file-number (- (length web-files) 2)))
  ; then
    (error "There is no include file numbered %d" include-file-number)
  ; else
    (switch-to-buffer (get-file-buffer (expand-file-name 
      (nth (1+ include-file-number) web-files) web-default-directory)))
    (goto-char (point-min))
  ) ; end if
)

(defun web-initialize-location-of-module-vector ()
  "This is an internal support function used to initialize the 
web-location-of-module vector.
ARGUMENTS : NONE
GLOBAL Variables : web-change-buffer-name, web-location-of-module, 
                   web-number-of-modules-in-file, web-buffer-name
                   web-files, and web-max-number-of-modules
LOCAL Variables : file-number, module-number, and section-number
RETURNS : Nothing
USES : web-collect-location-of-modules"
  (let ((file-number '(1))
        (module-number 1)
        (section-number 0)
       )
    (setq web-location-of-module (make-vector (1+ web-max-number-of-modules) 
            (make-vector 5 0))
          web-number-of-modules-in-file (make-vector 20 0)
          web-files ()
    ) ; end setq
    (aset web-location-of-module 0 (vector 1 0 0 0 0))
    (setq web-files (append web-files (list web-change-buffer-name)
      (list web-buffer-name)))
    (set-buffer (nth (car file-number) web-files))
    (message "Collecting the vector web-location-of-module...")
    (web-collect-location-of-modules)
    (message "Collecting the vector web-location-of-module...done")
  ) ; end let
)

(defun web-initialize-module-names-list ()
  "This function is invoked by \\[web-mode] if 1) filename.mods exists and
2) filename.mods is newer than filename.web.  If these two conditions are
true, then \\[web-mode] does not need to collect the web-module-names list from
scratch.  It can read this information from the file filename.mods.
ARGUMENTS : None
GLOBAL Variable : web-module-name-defined-in-used-in, web-module-names, 
                  web-number-of-module-names, web-buffer-name,
                  and web-default-directory
LOCAL Variables : defined-in-list, module-name, module-name-begins, 
                  and used-in-list
RETURNS : Nothing
USES : web-eliminate-white-space"
  (let (defined-in-list
        module-name
        (module-name-begins 0)
        used-in-list
       )
    (find-file-noselect (expand-file-name
      (concat (substring web-buffer-name 0 
      (string-match "\\." web-buffer-name)) ".mods") web-default-directory))
    (set-buffer (concat (substring web-buffer-name 0 
      (string-match "\\." web-buffer-name)) ".mods"))
    (goto-char (point-min))
    (while (not (eobp))
      (setq module-name-begins (point))
      (forward-line 1)
      (while (and (not (eobp))
                  (char-equal 
                    (string-to-char (buffer-substring (point) (1+ (point))))
                    ? )
             ) ; end condition
        (forward-line 1)
      ) ; end while
      (save-excursion
        (re-search-backward "(" nil t 2)
        (setq module-name (web-eliminate-white-space 
          (buffer-substring module-name-begins (point))))
      ) ; end excursion
      (save-excursion
        (setq defined-in-list ())
        (re-search-backward "(" nil t 2)
        (while (not (looking-at ")"))
          (re-search-forward "[0-9]+" nil t)
          (setq defined-in-list (append defined-in-list (list (string-to-int
            (buffer-substring (match-beginning 0) (match-end 0))))))
        ) ; end while
      ) ; end excursion
      (save-excursion
        (setq used-in-list ())
        (re-search-backward "(" nil t)
        (while (not (looking-at ")"))
          (re-search-forward "[0-9]+" nil t)
          (setq used-in-list (append used-in-list (list (string-to-int
            (buffer-substring (match-beginning 0) (match-end 0))))))
        ) ; end while
      ) ; end excursion
      (setq web-module-name-defined-in-used-in 
        (append web-module-name-defined-in-used-in (list 
          (list module-name defined-in-list used-in-list))))
      (setq web-module-names (append web-module-names (list
        (concat module-name "  " (int-to-string web-number-of-module-names)))))
      (setq web-number-of-module-names (1+ web-number-of-module-names))
    ) ; end while
  ) ; end let
)

(defun web-insert-index-entry (&optional type-style phrase)
  "This function inserts the necessary constructs so that an entry is placed in
the index by Weave.  Web supports three types of index entries.  They differ in
the type of type style used in the index itself.  This function asks the user
which TYPE-STYLE he prefers.  Once a valid selection has been made, it asks for
the PHRASE to be inserted in the index.
ARGUMENTS : type-style and phrase (both required) 
GLOBAL Variables : None 
LOCAL Variables : first-part and prompt 
RETURNS : Nothing 
USES : web-check-if-buffer-is-one-of-the-web-files and web-journal"
   (interactive)
   (web-journal "web-insert-index-entry")
   (web-check-if-buffer-is-one-of-the-web-files)
   (let (first-part
         (prompt "rm(R/r), tt(T/t), or User-Defined(U/u): ")
        )
     (if type-style
     ; then an explicit argument has been provided
       ()
     ; else prompt for the type-style
       (setq type-style (read-string (concat "Type Style: " prompt)))
       (while (and (not (string-equal (upcase type-style) "R"))
                   (not (string-equal (upcase type-style) "U"))
                   (not (string-equal (upcase type-style) "T"))
              )
         (ding)
         (setq type-style (read-string (concat "Try again. " prompt)))
      ) ; end while
    ) ; end if
    (cond
      ((string-equal (upcase type-style) "U")
        (setq first-part "@:")
      ) ; end case
      ((string-equal (upcase type-style) "T")
        (setq first-part "@.") 
      ) ; end case
      (t ; default
        (setq first-part "@^")
      ) ; end case
    ) ; end cond
    (if (null phrase)
     (setq phrase (read-string "Phrase to be placed in index: "))
    )
    (end-of-line)
    (newline)
    (insert first-part phrase "@>")
    (newline)
  ) ; end let
)

(defun web-insert-limbo-material ()
  "This function is only invoked by \\[web-mode] when the edit buffer is empty,
i.e., one is beginning a new Web document.  The user is prompted for the title
of the Web.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variables : limbo-file-name, line1, line2, line3, position-of-colon,
                  title, and width
RETURNS : Nothing
USES : Nothing"
  (let (limbo-file-name
        (line1 (concat "PROGRAM  :  " (buffer-name (current-buffer))))
        (line2 (concat "CREATOR  :  " (user-full-name) " [" (user-login-name)
          "@" (system-name) "]"))
        (line3 (concat "CREATION DATE  :  " (current-time-string)))
        (position-of-colon 0)
        title
        (width 0)
       )
    (cond
      ((and (>= (length line1) (length line2)) 
            (>= (length line1) (length line3)))
        (setq width (length line1)
              position-of-colon (+ (/ (- 77 width) 2) 9))
      ) ; end case
      ((and (>= (length line2) (length line1)) 
            (>= (length line2) (length line3)))
        (setq width (length line2)
              position-of-colon (+ (/ (- 77 width) 2) 9))
      ) ; end case
      ((and (>= (length line3) (length line1)) 
            (>= (length line3) (length line2)))
        (setq width (length line3)
              position-of-colon (+ (/ (- 77 width) 2) 15))
      ) ; end case
      (t
        (error 
          "In web-insert-limbo-material. Shouldn't have reached this code.")
      ) ; end case
    ) ; end cond
    ; insert a nice header
    (insert (make-string 79 ?%) "\n"
      ?% (make-string 77 ? ) ?% "\n"
      ?% (make-string (- position-of-colon 9) ? ) line1
      (make-string (- 77 (+ (length line1) (- position-of-colon 9))) ? )
      "%\n"
      ?% (make-string (- position-of-colon 9) ? ) line2
      (make-string (- 77 (+ (length line2) (- position-of-colon 9))) ? )
      "%\n"
      ?% (make-string (- position-of-colon 15) ? ) line3
      (make-string (- 77 (+ (length line3) (- position-of-colon 15))) ? )
      "%\n"
      ?% (make-string 77 ? ) "%\n"
      (make-string 79 ?%) "\n")
  
    (let ((insert-default-directory t))
      (while (null (file-readable-p (setq limbo-file-name 
        (read-file-name "File containing limbo material " 
          (expand-file-name "limbo.material")
          (expand-file-name "limbo.material")))))
        (ding)
        (message "%s does not exist or is non-readable" limbo-file-name)
        (sit-for 2)
      ) ; end while
    ) ; end let
    (insert-file-contents limbo-file-name)
  
    (setq title (read-string "Title of Web:  "))
    (goto-char (point-min))
    (if (re-search-forward "\\\\def\\\\title{{\\\\tt " nil t)
    ; then
      (insert-string title)
    ) ; end if
    (goto-char (point-max))
  ) ; end let
)

(defun web-insert-module-name ()
  "This function is invoked when the user presses <.  This function must look
at the preceding characters to determine if this represents the beginning of a
module name or not.  If the < is preceded by an odd number of @'s, then it
represents the beginning of a module name; otherwise, it doesn't.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : ctr
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, 
       web-options-for-module-name-insertion, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-insert-module-name")
  (let ((ctr 0)
        (file-no (web-check-if-buffer-is-one-of-the-web-files))
       )
    (backward-char 1)
    (while (and (not (bobp)) (looking-at "@"))
      (backward-char 1)
      (setq ctr (1+ ctr))
    ) ; end while
    (forward-char (1+ ctr))
    (insert-char ?< 1)
    (cond
      ((and (not (zerop (% ctr 2))) (>= (web-what-module) 1))
      ; then the preceding @< marks the beginning of a module name
        (web-options-for-module-name-insertion)
      ) ; end case
      ((and (not (zerop (% ctr 2))) (< (web-what-module) 1))
      ; then we have a valid module beginning but are in the limbo portion
        (delete-backward-char (1+ ctr))
        (error "You can't insert a module name in the limbo portion")
      ) ; end case
    ) ; end cond
  ) ; end let
)

(defun web-is-this-a-new-module-beginning ()
  "This function is invoked whenever the user enters the character @.  The
function then reads the next character.  If this next character is in the set
{ (space),*,\\t(tab),\\n(newline)}, then it begins a new module and the vector
web-location-of-module must be updated to reflect this addition.  The @ must be
at the beginning of a new line to be a valid module beginning.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : next-char
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-insert-module-name,
       web-update-the-location-of-module-vector, and web-journal"
  (interactive)
  (web-journal "web-is-this-a-new-module-beginning")
  (web-check-if-buffer-is-one-of-the-web-files)
  (if (not (bolp))
  ; then insert an @ -- it's not the beginning of a new module
    (insert-char ?@ 1)
  ; else get the next character
    (let ((next-char (read-char)))
      (if (not (or (char-equal next-char ? )
                   (char-equal next-char ?<)
                   (char-equal next-char ?*)
                   (char-equal next-char ?\t)
                   (char-equal next-char ?\015))) ; RET = ^M = \015 (octal)
      ; then insert an @ and the last input char -- it's not the beginning
      ; of a new module
        (progn
          (insert-char ?@ 1)
          (insert-char next-char 1)
        ) ; end then
      ; else this does begin a new module, major section, or code section
        (insert-char ?@ 1)
        (if (char-equal next-char ?<)
        ; then
          (web-insert-module-name)    
        ; else
          (if (char-equal next-char ?\015)
          ; then
            (insert-char ?\012 1)
          ; else
            (insert-char next-char 1)
          ) ; end if
          (web-update-the-location-of-module-vector next-char)
        ) ; end if
      ) ; end if
    ) ; end let
  ) ; end if
)
             
(defun web-journal (function-name)
  "This function keeps track of the number of times FUNCTION-NAME has been
invoked.  Only the interactive functions in web-mode are kept track of.
ARGUMENTS : function-name (required)
GLOBAL Variables : web-interactive-function-usage, web-buffer-name, and
                   web-interactive-functions
LOCAL Variables : ctr, start-time-in-seconds, stop-time, 
                  stop-time-in-seconds, time-in-web-mode, and 
                  time-in-web-mode-as-string
RETURNS : Nothing
USES : web-binary-search-of-names"
  (let ((ctr 0)
        (start-time-in-seconds 0)
        stop-time
        (stop-time-in-seconds 0)
        (time-in-web-mode 0)
        time-in-web-mode-as-string
       )
    (cond
      ((string-equal function-name "web-mode")
        ; web-mode is the first function invoked
        ; thus, do initialization
        (get-buffer-create "web-mode.jou")
        (setq web-interactive-function-usage 
            (make-vector (length web-interactive-functions) 0)
              start-time (current-time-string))
        (save-excursion
          (set-buffer "web-mode.jou")
          (goto-char (point-min))
          (kill-line (count-lines (point-min) (point-max)))
          (insert (make-string 79 ?\045) 
            "\nUSER : " (user-full-name) " [" (user-login-name)
            "@" (system-name) "]\nFILE : " web-buffer-name "\nSTART TIME : "
            start-time "\n")
        ) ; end excursion
      ) ; end case
      ((string-equal function-name "web-mode-save-buffers-kill-emacs")
        ; this is the last function invoked by the user when he wants to quit
        ; get the stop time, calculate the total time in web-mode, and
        ; write the usage of the functions to the file web-mode.jou
        (setq ctr (web-binary-search-of-names web-interactive-functions
                                             (length web-interactive-functions)
                                              function-name
                                             (length function-name)))
        (aset web-interactive-function-usage ctr 
          (1+ (aref web-interactive-function-usage ctr)))
        (save-excursion
          (set-buffer "web-mode.jou")
          (setq stop-time (current-time-string))
          (setq start-time-in-seconds (+ 
            (* 3600 (string-to-int (substring start-time 11 13)))
            (*   60 (string-to-int (substring start-time 14 16)))
                    (string-to-int (substring start-time 17 19))))
          (setq stop-time-in-seconds (+ 
            (* 3600 (string-to-int (substring stop-time 11 13)))
            (*   60 (string-to-int (substring stop-time 14 16)))
                    (string-to-int (substring stop-time 17 19))))
          (if (not (natnump (- stop-time-in-seconds start-time-in-seconds)))
          ; then add the equivalent of a day to the stop time
            (setq stop-time-in-seconds (+ stop-time-in-seconds 86400))
          ) ; end if
          (setq time-in-web-mode 
            (- stop-time-in-seconds start-time-in-seconds))
          (if (< (/ time-in-web-mode 3600) 10)
          ; then
            (setq time-in-web-mode-as-string (concat "0"
              (int-to-string (/ time-in-web-mode 3600)) ":"))
          ; else
            (setq time-in-web-mode-as-string (concat 
              (int-to-string (/ time-in-web-mode 3600)) ":"))
          ) ; end if
          (if (< (% (/ time-in-web-mode 60) 60) 10)
          ; then
            (setq time-in-web-mode-as-string 
              (concat time-in-web-mode-as-string "0"
              (int-to-string (% (/ time-in-web-mode 60) 60)) ":"))
          ; else
            (setq time-in-web-mode-as-string 
              (concat time-in-web-mode-as-string
              (int-to-string (% (/ time-in-web-mode 60) 60)) ":"))
          ) ; end if
          (if (< (% (% time-in-web-mode 3600) 60) 10)
          ; then
            (setq time-in-web-mode-as-string 
              (concat time-in-web-mode-as-string "0"
              (int-to-string (% (% time-in-web-mode 3600) 60))))
          ; else
            (setq time-in-web-mode-as-string 
              (concat time-in-web-mode-as-string
              (int-to-string (% (% time-in-web-mode 3600) 60))))
          ) ; end if
          (insert "FINISH TIME : " stop-time "\n" 
            "TOTAL TIME IN web-mode : " time-in-web-mode-as-string "\n"
            (make-string 79 ?\055) 
            "\nINTERACTIVE FUNCTION USAGE\n")
          (while (< ctr (length web-interactive-functions))
            (if (not (zerop (aref web-interactive-function-usage ctr)))
              (insert (make-string (- 4 (length (int-to-string 
                (aref web-interactive-function-usage ctr)))) ? )
                (int-to-string (aref web-interactive-function-usage ctr))
                "  " (nth ctr web-interactive-functions) "\n")
            ) ; end if
            (setq ctr (1+ ctr))
          ) ; end while
          (write-region (point-min) (point-max) "web-mode.jou" t)
        ) ; end excursion
      ) ; end case
    ) ; end cond
    (setq ctr (web-binary-search-of-names web-interactive-functions
                                      (length web-interactive-functions)
                                      function-name
                                      (length function-name)))
    (if (>= ctr 0)
    ; then the function-name was interactive
      (aset web-interactive-function-usage ctr 
        (1+ (aref web-interactive-function-usage ctr)))
    ; else the function was not found in the web-interactive-functions list
      (error "Function %s wasn't found" function-name)
    ) ; end if
  ) ; end let
)

(defun web-mode ()
  "This is the definition of the mode for manipulating Web documents.  When
invoked the function checks to see if the file that is being edited is new.  If
it is, limbo material is inserted.  If the file is not new, the program checks
for any undefined module names.  If any modules are found to be undefined,
stubs are appended.
ARGUMENTS : None
GLOBAL Variables : web-change-buffer-name, web-defined-in-used-in-location,
                   web-location-of-module, web-module-names, 
                   web-number-of-lines-in-window, web-number-of-module-names,
                   web-selected-index-entry-occurrences, web-buffer-name,
                   web-default-directory, web-files, web-mode-map, and
                   web-module-changed-then-goto-change
LOCAL Variables : None
RETURNS : Nothing
USES : web-any-modules-undefined-initially, 
       web-collect-list-of-changed-module-numbers, web-collect-module-names,
       web-initialize-location-of-module-vector,
       web-initialize-module-names-list, web-insert-limbo-material,
       and web-journal"
  (interactive)
  (kill-all-local-variables)
  (use-local-map web-mode-map)
  (setq major-mode 'Web-mode
        mode-name "Web")
  (auto-fill-mode 1)
  (setq web-change-buffer-name 
          (concat (substring (buffer-name (current-buffer))
          0 (string-match "\\." (buffer-name (current-buffer)))) ".ch")
        web-defined-in-used-in-location -1                 
        fill-column 70                                 
        web-location-of-module nil
        web-module-names nil                               
        web-number-of-lines-in-window (- (screen-height) 2)
        web-number-of-module-names 0                       
        web-selected-index-entry-occurrences nil       
        web-buffer-name (buffer-name (current-buffer))
        web-default-directory default-directory
        web-files nil
        web-module-changed-then-goto-change nil
  ) ; end setq
  (web-journal "web-mode")
  (if (= (length (buffer-substring (point-min) (point-max))) 0)
  ; then
    (web-insert-limbo-material)
  ; else initialize the web-module-names list and 
  ; check to see if any modules are undefined
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\f" nil t)
        (replace-match "\n" t t)
      ) ; end while
    ) ; end excursion
    (web-initialize-location-of-module-vector)
    (if (and (file-readable-p 
               (expand-file-name (concat (substring web-buffer-name 0
               (string-match "\\." web-buffer-name)) ".mods")))
             (file-newer-than-file-p 
               (expand-file-name (concat (substring web-buffer-name 0
               (string-match "\\." web-buffer-name)) ".mods"))
               (expand-file-name web-buffer-name))
        ) ; end condition
    ; then
      (progn
        (message "Initializing the Module Names List...")
        (web-initialize-module-names-list)
        (message "Initializing the Module Names List...done")
        (sit-for 1)
      ) ; end then
    ; else
      (web-collect-module-names)
    ) ; end if
    (message "Checking to see if any modules are undefined...")
    (web-any-modules-undefined-initially)
    (message "Checking to see if any modules are undefined...done")
    (find-file-noselect web-change-buffer-name)
    (save-excursion
      (set-buffer web-change-buffer-name)
      (web-collect-list-of-changed-module-numbers)
      (setq major-mode 'Web-mode
            mode-name "Web")
      (use-local-map web-mode-map)
    ) ; end excursion
  ) ; end if
)

(define-key (current-global-map) "\C-x\C-c" 'web-mode-save-buffers-kill-emacs)

(defun web-mode-save-buffers-kill-emacs ()
  "This function is used to replace Emacs' save-buffers-kill-emacs so that
the journal file can be written.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variables : None
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-journal, and 
       web-write-module-names-to-a-file"
  (interactive)
  (web-check-if-buffer-is-one-of-the-web-files)
  (web-journal "web-mode-save-buffers-kill-emacs")
  (web-write-module-names-to-a-file)  
  (save-buffers-kill-emacs)
)

(defun web-newline ()
  "This function redefines the newline character so that the following line
is indented like the previous one.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : ctr
RETURNS : Nothing
USES : web-journal"
  (interactive)
  (web-journal "web-newline")
  (let ((ctr 0))
    (save-excursion
      (beginning-of-line)
      (while (looking-at " ")
        (setq ctr (1+ ctr))
        (forward-char 1)
      ) ; end while
    ) ; end excursion
    (newline)
    (insert (make-string ctr ?\040))
  ) ; end let
)

(defun web-next-change ()
  "Advances point to the beginning of the next change.
ARGUMENTS : None
GLOBAL Variable : web-change-buffer-name
LOCAL Variable : start-position
RETURNS : Nothing
USES : goto-change and web-journal"
  (interactive)
  (web-journal "web-next-change")
  (if (not (string= web-change-buffer-name (buffer-name (current-buffer))))
  ; then
    (error "Not in the CHange buffer.")
  ; else
    (let ((start-position (point)))
      (if (looking-at "^@x") (forward-char 2))
      (if (re-search-forward "^@x" nil t)
      ; then
        (progn
          (re-search-forward "^@y" nil t)
          (goto-char (match-beginning 0))
          (recenter 0)
        ) ; end then
      ; else there is no next change
        (goto-char start-position)
        (error "No next CHange.")
      ) ; end if
    ) ; end let
  ) ; end if
)

(defun web-next-define ()
  "This function is used to traverse the list of Defined In occurrences of the
most recently selected module name in the forward direction.  A module name is
selected by issuing the command \\[web-view-module-names-list] and positioning
the cursor on the line that contains the module name of interest.
ARGUMENTS : None
GLOBAL Variables : web-defined-in-occurrence, web-defined-in-used-in-location,
                   and web-module-name-defined-in-used-in
LOCAL Variable : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-next-or-previous-define-or-use, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-next-define")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string= (buffer-name (current-buffer))
              "*Module Name (Defined In) (Used In)*")
      ; then the user needs to select an entry
      (let ((buffer-to-be-returned-to 
              (web-next-or-previous-define-or-use "next" "defined-in")))
        (if (>= web-defined-in-occurrence
                (length (car (cdr (nth web-defined-in-used-in-location
                  web-module-name-defined-in-used-in)))))
        ; then there is no next define
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No next define of %s" (car (nth 
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
          ) ; end then
        ; else
          (web-goto-module (nth web-defined-in-occurrence (car (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in)))))
          (message "Define %d of %d of %s" (1+ web-defined-in-occurrence)
            (length (car (cdr (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in))))
            (car (nth web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; we are in a valid Web buffer
      (if (= web-defined-in-used-in-location -1)
          (error (concat "No selected module. "
            "Execute M-x web-view-module-names-list to select."))
      ; else
        (let ((current-module (web-what-module)))
          (setq web-defined-in-occurrence 0)
          (while (and (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                      (<= (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                        current-module)
                 ) ; end and
            (setq web-defined-in-occurrence (1+ web-defined-in-occurrence))
          ) ; end while
          (if (>= web-defined-in-occurrence
                  (length (car (cdr (nth web-defined-in-used-in-location
                    web-module-name-defined-in-used-in)))))
          ; then there is no next define
            (error "No next define of %s" (car (nth 
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
          ; else
            (web-goto-module (nth web-defined-in-occurrence (car (cdr (nth
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))))
            (message "Define %d of %d of %s" (1+ web-defined-in-occurrence)
              (length (car (cdr (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in))))
              (car (nth web-defined-in-used-in-location 
                web-module-name-defined-in-used-in)))
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case  
  ) ; end cond
)

(defun web-next-index ()
  "This function is used to traverse the list of occurrences of the most
recently selected index entry in the forward direction.  An index entry is
selected by issuing the command \\[web-view-index] and positioning the cursor
on the line that contains the index entry of interest.
ARGUMENTS : None
GLOBAL Variables : web-location-in-selected-index-entry-occurrences,
                   web-selected-index-entry-occurrences,
                   web-index-buffer-name, and web-index-entry
LOCAL Variables : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-journal, web-next-or-previous-index, and web-what-module"
  (interactive)
  (web-journal "web-next-index")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string-equal web-index-buffer-name (buffer-name (current-buffer)))
      ; then we are in the index
      (let ((buffer-to-be-returned-to (web-next-or-previous-index "next")))
        (if (>= web-location-in-selected-index-entry-occurrences
                (length web-selected-index-entry-occurrences))
        ; then there is no next use
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No next index reference for %s" web-index-entry)
          ) ; end then
        ; else
          (web-goto-module (nth 
            web-location-in-selected-index-entry-occurrences 
            web-selected-index-entry-occurrences))
          (message "Index Reference %d of %d for entry %s"
            (1+ web-location-in-selected-index-entry-occurrences)
            (length web-selected-index-entry-occurrences)
            web-index-entry)
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; else we are in a Web buffer
      (if (null web-selected-index-entry-occurrences)
      ; then
        (error 
          "Issue the command M-x web-view-index to select an index entry.")
      ; else
        (let ((current-module (web-what-module)))
          (setq web-location-in-selected-index-entry-occurrences 0)
          (while (and (nth web-location-in-selected-index-entry-occurrences
                        web-selected-index-entry-occurrences)
                      (<= (nth web-location-in-selected-index-entry-occurrences
                        web-selected-index-entry-occurrences)
                        current-module)
                 ) ; end and
            (setq web-location-in-selected-index-entry-occurrences 
              (1+ web-location-in-selected-index-entry-occurrences))
          ) ; end while
          (if (>= web-location-in-selected-index-entry-occurrences
                  (length web-selected-index-entry-occurrences))
          ; then
            (error "No next index reference for %s" web-index-entry)
          ; else
            (web-goto-module (nth 
              web-location-in-selected-index-entry-occurrences
              web-selected-index-entry-occurrences))
            (message "Index Reference %d of %d for entry %s"
              (1+ web-location-in-selected-index-entry-occurrences)
              (length web-selected-index-entry-occurrences)
              web-index-entry)
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case
  ) ; end cond
)

(defun web-next-module ()
  "This function moves point to the beginning of the next module.  If point is
currently in the last module of the Web, an appropriate message is sent to the
minibuffer; otherwise, point is positioned at the first line of the next
module.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, 
       web-count-modules, web-goto-module, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-next-module")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((current-module (web-what-module)))
    (if (>= current-module (web-count-modules))
    ; then
      (error "No next module")
    ; else
      (web-goto-module (1+ current-module))
    ) ; end if
  ) ; end let
)

(defun web-next-or-previous-define-or-use (direction list)
  "This is an internal support function that is called by both \\[web-next-use]
and \\[web-next-define].  It determines the module name that the cursor is
currently located at.  After the name has been isolated, it is looked up in the
list of module names so that the traversal of the module name's Defined In and
Used In lists can be done.
ARGUMENTS : direction and list (both required)
GLOBAL Variables : web-defined-in-occurrence, web-defined-in-used-in-location, 
                   web-module-names, web-number-of-module-names, 
                   web-selected-module-name, and web-used-in-occurrence
LOCAL Variables : beginning-of-module-name, ctr, current-module, 
                  current-position, end-of-module-name, ptr, and 
                  web-buffer-list
RETURNS : buffer to be returned to on error
USES : web-binary-search-of-names, web-eliminate-white-space, 
       and web-what-module"
  (let ((beginning-of-module-name 0)
        (ctr 1)
        (current-module 0)
        (current-position (point))
        (end-of-module-name 0)
        (ptr 0)
        web-buffer-list
       )
    (if (eobp)
    ; then
      (error "No module name can be selected from point's current position.")
    ) ; end if
    (save-excursion
      (setq current-position (point))
      (beginning-of-line) 
      (while (char-equal 
        (string-to-char (buffer-substring (point) (1+ (point)))) ? )
        (forward-line -1)
      ) ; end while
      (setq beginning-of-module-name (point))
      (goto-char current-position)
      (forward-line 1)
      (while (and (not (eobp)) (char-equal 
        (string-to-char (buffer-substring (point) (1+ (point)))) ? ))
        (forward-line 1)
      ) ; end while
      (backward-char 1)
      (setq end-of-module-name (point))
      (goto-char end-of-module-name)
      (re-search-backward "(" beginning-of-module-name t 2)
      (setq web-selected-module-name (web-eliminate-white-space 
        (buffer-substring beginning-of-module-name (1- (point)))))
      ; Set the global variable search-last-string so that the user can
      ; do a C-s C-s to go immediately to the selected module name.
      (setq search-last-string web-selected-module-name)
      (setq ptr (web-binary-search-of-names web-module-names 
        web-number-of-module-names 
        web-selected-module-name (length web-selected-module-name)))
      (setq web-defined-in-used-in-location 
        (string-to-int (substring (nth ptr web-module-names)
        (+ (string-match "  " (nth ptr web-module-names)) 2))))
      (setq web-buffer-list (mapcar (function buffer-name) (buffer-list)))
      (setq ctr 1)
      (while (char-equal (string-to-char 
             (substring (nth ctr web-buffer-list) 0 1)) ? )
        (setq ctr (1+ ctr))
      ) ; end while
      (save-excursion
        (set-buffer (nth ctr web-buffer-list))
        (setq current-module (web-what-module))
      ) ; end excursion
      (cond
        ((and (string= direction "next")
              (string= list "defined-in"))
          (setq web-defined-in-occurrence 0)
          (while (and (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                      (<= (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                        current-module)
                 ) ; end and
            (setq web-defined-in-occurrence (1+ web-defined-in-occurrence))
          ) ; end while
        ) ; end case
        ((and (string= direction "next")
              (string= list "used-in"))
          (setq web-used-in-occurrence 0)
          (while (and (nth web-used-in-occurrence (car (cdr (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in)))))
                      (<= (nth web-used-in-occurrence (car (cdr (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in)))))
                        current-module)
                 ) ; end and
            (setq web-used-in-occurrence (1+ web-used-in-occurrence))
          ) ; end while
        ) ; end case
        ((and (string= direction "previous")
              (string= list "defined-in"))
          (setq web-defined-in-occurrence (1- (length 
            (car (cdr (nth web-defined-in-used-in-location 
            web-module-name-defined-in-used-in))))))
          (while (and (>= web-defined-in-occurrence 0)
                      (>= (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                        current-module)
                 ) ; end and
            (setq web-defined-in-occurrence (1- web-defined-in-occurrence))
          ) ; end while
        ) ; end case
        ((and (string= direction "previous")
              (string= list "used-in"))
          (setq web-used-in-occurrence (1- (length (car (cdr (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in)))))))
          (while (and (>= web-used-in-occurrence 0)
                      (>= (nth web-used-in-occurrence (car (cdr (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in)))))
                        current-module)
                 ) ; end and
            (setq web-used-in-occurrence (1- web-used-in-occurrence))
          ) ; end while
        ) ; end case
        (t
          (error "This case should not be reached.")
        ) ; end case
      ) ; end cond
      ; return the name of the buffer to be returned to on error
      (nth ctr web-buffer-list) 
    ) ; end excursion
  ) ; end let
)

(defun web-next-or-previous-index (direction)
  "This is an internal support function used by \\[web-next-index] and
\\[web-previous-index].  This is common code and is only invoked when the 
user is viewing the index.  This code determines the entry the user has
selected and builds a list of the referenced module numbers for the selected
index entry.
ARGUMENT : direction (required)
GLOBAL Variables : web-location-in-selected-index-entry-occurrences,
                   web-selected-index-entry-occurrences, and web-index-entry
LOCAL Variables : ctr, current-module, end-of-web-index-entry, and
                  web-buffer-list
RETURNS : the name of the buffer to return on an error
USES : web-what-module"
  (let ((ctr 1)
        (current-module 0)            
        (end-of-web-index-entry 0)
        web-buffer-list
       )
    (if (eobp)
    ; then
      (error "No index entry can be selected from this position.")
    ) ; end if
    (save-excursion
      (beginning-of-line)
      (while (looking-at " ") (forward-line -1))
      (setq web-index-entry (buffer-substring (point)
        (save-excursion (re-search-forward ": ") (- (point) 2))))
      ; Set the global Emacs variable search-last-string to the index
      ; entry selected by the user so that the user can use C-s C-s to
      ; get to the entry much easier.
      (setq search-last-string web-index-entry)
      (save-excursion
        (re-search-forward "\\.\n" nil t)
        (setq end-of-web-index-entry (point))
      ) ; end excursion
      (while (re-search-forward 
        "\\([0-9]+\\)\\*?\\(, \\|,\n\\|\\.\n\\)" 
        end-of-web-index-entry t)
        (setq web-selected-index-entry-occurrences (append 
          web-selected-index-entry-occurrences
          (list (string-to-int 
          (buffer-substring (match-beginning 1) (match-end 1))))))
      ) ; end while
    ) ; end excursion
    (setq web-buffer-list (mapcar (function buffer-name) (buffer-list)))
    (setq ctr 1)
    (while (char-equal (string-to-char 
           (substring (nth ctr web-buffer-list) 0 1)) ? )
      (setq ctr (1+ ctr))
    ) ; end while
    (save-excursion
      (set-buffer (nth ctr web-buffer-list))
      (setq current-module (web-what-module))
    ) ; end excursion
    (cond
      ((string= direction "next")
        (setq web-location-in-selected-index-entry-occurrences 0)
        (while (and (nth web-location-in-selected-index-entry-occurrences
                      web-selected-index-entry-occurrences)
                    (<= (nth web-location-in-selected-index-entry-occurrences
                      web-selected-index-entry-occurrences)
                      current-module)
               ) ; end and
          (setq web-location-in-selected-index-entry-occurrences 
            (1+ web-location-in-selected-index-entry-occurrences))
        ) ; end while
      ) ; end case
      ((string= direction "previous")
        (setq web-location-in-selected-index-entry-occurrences 
          (1- (length web-selected-index-entry-occurrences)))
        (while (and (>= web-location-in-selected-index-entry-occurrences 0)
                    (>= (nth web-location-in-selected-index-entry-occurrences
                      web-selected-index-entry-occurrences)
                      current-module)
               ) ; end and
          (setq web-location-in-selected-index-entry-occurrences 
            (1- web-location-in-selected-index-entry-occurrences))
        ) ; end while
      ) ; end case
      (t
        (error "This case should not be reached.")
      ) ; end case
    ) ; end cond
    ; return the name of the buffer to return to on an error
    (nth ctr web-buffer-list) 
  ) ; end let 
)

(defun web-next-section ()
  "This function moves point to the beginning of the next section.  If point is
currently in the last section of the Web, an appropriate message is sent to the
minibuffer; otherwise, point is positioned at the first line of the next
section.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : current-section
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files,
       web-count-sections, web-goto-section, web-journal, and web-what-section"
  (interactive)
  (web-journal "web-next-section")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((current-section (web-what-section)))
    (if (>= current-section (web-count-sections))
    ; then
      (error "No next section")
    ; else
      (web-goto-section (1+ current-section))
    ) ; end if
  ) ; end let
)

(defun web-next-use ()
  "This function is used to traverse the list of Used In occurrences of the
most recently selected module name in the forward direction.  A module name is
selected by issuing the command \\[web-view-module-names-list] and positioning
the cursor on the line that contains the module name of interest.
ARGUMENTS : None
GLOBAL Variables : web-defined-in-used-in-location, 
                   web-module-name-defined-in-used-in, and 
                   web-used-in-occurrence
LOCAL Variables : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-next-or-previous-define-or-use, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-next-use")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string= (buffer-name (current-buffer))
              "*Module Name (Defined In) (Used In)*")
      ; then the user needs to select an entry
      (let ((buffer-to-be-returned-to 
              (web-next-or-previous-define-or-use "next" "used-in")))
        (if (>= web-used-in-occurrence
                (length (car (cdr (cdr (nth web-defined-in-used-in-location
                  web-module-name-defined-in-used-in))))))
        ; then there is no next use
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No next use of %s" 
              (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
          ) ; end then
        ; else
          (web-goto-module (nth web-used-in-occurrence (car (cdr (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in))))))
          (message "Use %d of %d of %s" (1+ web-used-in-occurrence)
            (length (car (cdr (cdr (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in))))) 
            (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; we are in a valid Web buffer
      (if (= web-defined-in-used-in-location -1)
          (error (concat "No selected module. "
            "Execute M-x web-view-module-names-list to select."))
      ; else
        (let ((current-module (web-what-module)))
          (setq web-used-in-occurrence 0)
          (while (and (nth web-used-in-occurrence (car (cdr (cdr (nth 
                  web-defined-in-used-in-location 
                  web-module-name-defined-in-used-in)))))
                (<= (nth web-used-in-occurrence (car (cdr (cdr (nth 
                  web-defined-in-used-in-location 
                  web-module-name-defined-in-used-in)))))
                  current-module)
           ) ; end and
            (setq web-used-in-occurrence (1+ web-used-in-occurrence))
          ) ; end while
          (if (>= web-used-in-occurrence
                  (length (car (cdr (cdr (nth web-defined-in-used-in-location
                    web-module-name-defined-in-used-in))))))
          ; then there is no next define
            (error "No next use of %s" 
              (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
          ; else
            (web-goto-module (nth web-used-in-occurrence (car (cdr (cdr (nth
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in))))))
            (message "Use %d of %d of %s" (1+ web-used-in-occurrence)
              (length (car (cdr (cdr (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in))))) 
              (car (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in)))
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case  
  ) ; end cond
)

(defun web-options-for-module-name-insertion ()
  "This function is invoked by \\[web-insert-module-name] if the @< just
encountered represents a valid module beginning.  This function then presents
the user with various options for choosing or creating a module name.
ARGUMENTS : None
GLOBAL Variables : web-module-names, web-number-of-lines-in-window, and 
                   web-number-of-module-names
LOCAL Variables : available-lines, char-after-at-greater, ctr, defined-in-list,
                  done, element-to-be-inserted, found, len, location, menu,
                  menu-choice, module-name, module-number, screen-bottom,
                  screen-top, search-letter, sub-menu, and used-in-list
RETURNS : Nothing
USES : web-append-a-stub-module, web-binary-search-of-names, 
       web-determine-module-name-ending, web-display-module-names, 
       web-eliminate-white-space, and web-what-module"
  (let ((available-lines (- web-number-of-lines-in-window 11))
        char-after-at-greater,
        (ctr 0)
        done
        element-to-be-inserted
        found
        (len 0)
        (location 0)
        (menu (concat "Option  Action\n------  " (make-string 43 ?-)    
          "\n   A    Abort\n"
            "   C    Create a New Module Name\n"
            "   L    List all Module Names Beginning with Letter\n"
            "   N    Next Screen\n"
            "   P    Previous Screen\n"
            "   S    Select Existing Module Name\n"
          (make-string 79 ?-) "\n"))
        (menu-choice 0)
        module-name
        (module-number 0)
        (screen-bottom 0)
        (screen-top 0)
        search-letter
        used-in-list    
       )
    (switch-to-buffer "*Module Names*")
    (use-local-map web-mode-map)
    (setq major-mode 'Web-mode
          mode-name "Web")
    (goto-char (point-min))
    (kill-line (count-lines (point-min) (point-max)))
    (insert-string menu)
    (setq screen-top 1)
    (if (>= web-number-of-module-names available-lines)
    ; then
      (setq screen-bottom available-lines)
    ; else
      (setq screen-bottom web-number-of-module-names)
    ) ; end if
    (web-display-module-names screen-top screen-bottom)
    (setq menu-choice (read-string 
      "(A)bort, (C)reate, (L)ist, (N)ext, (P)revious, or (S)elect: "))
    (while (not (string-match (upcase (substring menu-choice 0 1)) "ACLNPS"))
      (ding)
      (setq menu-choice (read-string
        "(A)bort, (C)reate, (L)ist, (N)ext, (P)revious, or (S)elect: "))
    ) ; end while
    (setq done nil)
    (while (not done)
      (cond
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?A)
          ; this is the Abort option
          (switch-to-buffer (get-file-buffer (expand-file-name
            (nth file-no web-files) web-default-directory)))
          (delete-backward-char 2)
          (setq done t)
        ) ; end case
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?N)
          ; show the Next screen of module names
          (setq screen-bottom (+ screen-bottom available-lines))
          (if (> screen-bottom web-number-of-module-names)
          ; then
            (setq screen-bottom web-number-of-module-names)
          ) ; end if
          (setq screen-top (1+ (- screen-bottom available-lines)))
          (if (< screen-top 0)
          ; then
            (setq screen-top 1)
          ) ; end if
          (web-display-module-names screen-top screen-bottom)
          (setq done nil)
        ) ; end case
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?P)
          ; show the Previous screen of module names
          (setq screen-top (- screen-top available-lines))
          (if (<= screen-top 0)
          ; then
            (setq screen-top 1)
          ) ; end if
          (setq screen-bottom (1- (+ screen-top available-lines)))
          (if (>= screen-bottom web-number-of-module-names)
          ; then
            (setq screen-bottom web-number-of-module-names)
          ) ; end if
          (web-display-module-names screen-top screen-bottom)
          (setq done nil)
        ) ; end case
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?L)
          ; list all module names beginning with a particular letter
          (setq search-letter (read-string "Letter: ")
                found nil 
                ctr 1)
          (while (and (<= ctr web-number-of-module-names)
            (<= (string-to-char 
                  (substring (nth (1- ctr) web-module-names) 0 1))
                (string-to-char search-letter)))
            (if (and (null found) (string-equal 
              (substring (nth (1- ctr) web-module-names) 0 1)
              (substring search-letter 0 1)))
            ; then
              (setq found t 
                    screen-top ctr)
            ) ; end if
            (setq ctr (1+ ctr))
          ) ; end while
          (if (null found)
          ; then
            (progn
              (ding)
              (message "No module names begin with %s" 
                (substring search-letter 0 1))
              (sit-for 3)
            ) ; end then
          ; else
            (setq screen-bottom (min (1- ctr) 
              (1- (+ screen-top available-lines))))
            (web-display-module-names screen-top screen-bottom 
              (- ctr screen-top))
          ) ; end if
          (setq done nil)
        ) ; end case
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?S)
          ; choose module name
          (if (zerop web-number-of-module-names)
          ; then this is an invalid choice
            (progn
              (ding)
              (message 
                "Invalid since there are no module names to select from.")
              (setq done nil)
            ) ; end then
          ; else select one of the existing names
            (setq module-number (read-string 
              (format "Module Number [1..%d]: " web-number-of-module-names))) 
            (while (or (< (string-to-int module-number) 1)
                   (> (string-to-int module-number) 
                      web-number-of-module-names))
              (ding)
              (setq module-number (read-string 
                (format "Module Number [1..%d]: " web-number-of-module-names)))
            ) ; end while
            (setq module-name (substring 
              (nth (1- (string-to-int module-number)) web-module-names)
              0 (string-match "  " 
              (nth (1- (string-to-int module-number)) web-module-names))))
            (switch-to-buffer (get-file-buffer (expand-file-name
              (nth file-no web-files) web-default-directory)))
            (insert-string module-name)
            (insert-string (web-determine-module-name-ending))
            (setq location (string-to-int (substring 
              (nth (1- (string-to-int module-number)) web-module-names)
              (+ (string-match "  " (nth (1- (string-to-int module-number))
               web-module-names)) 2))))
            (if (char-equal (preceding-char) ?=)
            ; then an existing module name has been defined again
              (progn
                (setq new-module-number (web-what-module))
                (web-update-the-module-name-defined-in-used-in-list 
                  new-module-number)
                (setq defined-in-list (sort (append (car (cdr (nth
                  location web-module-name-defined-in-used-in)))
                  (list new-module-number)) '<))
                (setcdr (nth location web-module-name-defined-in-used-in)
                  (append (list defined-in-list)
                  (list (car (cdr (cdr 
                  (nth location web-module-name-defined-in-used-in)))))))
              ) ; end then
            ; else update the used in list in the module names
              (if (null (car (car (cdr (cdr 
                    (nth location web-module-name-defined-in-used-in))))))
              ; then
                (setq used-in-list (list (web-what-module)))
              ; else
                (setq used-in-list (sort (append (car (cdr (cdr 
                  (nth location web-module-name-defined-in-used-in))))
                  (list (web-what-module))) '<))
              ) ; end if
              (setcdr (nth location web-module-name-defined-in-used-in)
                (append (list (car (cdr (nth location 
                web-module-name-defined-in-used-in))))
                (list used-in-list)))
            ) ; end if
            (if (char-equal (preceding-char) ?=) (insert-char ?\012 1))
            (setq done t)           
          ) ; end if
        ) ; end case
        ((char-equal (string-to-char (upcase (substring menu-choice 0 1))) ?C)
          ; Create a new module name
          (setq module-name (read-string "New Module Name: "))
          (switch-to-buffer (get-file-buffer (expand-file-name
            (nth file-no web-files) web-default-directory)))
          (setq module-name (web-eliminate-white-space module-name))
          (insert-string module-name)
          (insert-string (web-determine-module-name-ending))
          (if (char-equal (preceding-char) ?=)
          ; then
            (setq char-after-at-greater ?=)
          ; else
            (setq char-after-at-greater ? )
          ) ; end if
          (setq len (length module-name))
          (if (and (> len 3) (string-equal "..."
                (substring module-name (- len 3) len)))
          ; then
            (setq len (- len 3))
          ) ; end if
          (setq location (web-binary-search-of-names web-module-names 
            web-number-of-module-names module-name len))
          (if (= location -1)
          ; then it is indeed a new module name
            (progn
              (setq web-module-names (sort 
                (append web-module-names (list (concat
                module-name "  " (int-to-string web-number-of-module-names))))
                'string<))
              (setq web-number-of-module-names (1+ web-number-of-module-names))
              (setq new-module-number (web-what-module))
              (if (char-equal ?= char-after-at-greater)
              ; then this new module was defined (i.e., @<...@>=)
                (progn
                  (web-update-the-module-name-defined-in-used-in-list 
                    new-module-number)
                  (setq web-module-name-defined-in-used-in (append
                    web-module-name-defined-in-used-in (list (list module-name
                    (list new-module-number) (list ())))))
                ) ; end then
              ; else this new module was used before defined (insert a stub)
                (ding)
                (message "Appending a module for %s" module-name)
                (sit-for 2)
                (setq module-number-of-stub 
                  (web-append-a-stub-module module-name))
                (setq web-module-name-defined-in-used-in (append
                  web-module-name-defined-in-used-in (list (list module-name
                  (list module-number-of-stub) (list new-module-number)))))
              ) ; end if
            ) ; end then
          ; else the user entered a supposedly new name that already exists
            (setq location (string-to-int (substring 
              (nth location web-module-names)
              (+ (string-match "  " (nth location web-module-names)) 2))))
            (if (char-equal (preceding-char) ?=)
            ; then an existing module name has been defined again
              (progn
                (setq new-module-number (web-what-module))
                (web-update-the-module-name-defined-in-used-in-list 
                  new-module-number)
                (setq defined-in-list (sort (append (car (cdr (nth
                  location web-module-name-defined-in-used-in)))
                  (list new-module-number)) '<))
                (setcdr (nth location web-module-name-defined-in-used-in)
                  (append (list defined-in-list)
                  (list (car (cdr (cdr 
                  (nth location web-module-name-defined-in-used-in)))))))
              ) ; end then
            ; else update the used in list in the module names
              (if (null (car (car (cdr (cdr 
                    (nth location web-module-name-defined-in-used-in))))))
              ; then
                (setq used-in-list (list (web-what-module)))
              ; else
                (setq used-in-list (sort (append (car (cdr (cdr 
                  (nth location web-module-name-defined-in-used-in))))
                  (list (web-what-module))) '<))
              ) ; end if
              (setcdr (nth location web-module-name-defined-in-used-in)
                (append (list (car (cdr (nth location 
                web-module-name-defined-in-used-in))))
                (list used-in-list)))
            ) ; end if
            (if (char-equal (preceding-char) ?=) (insert-char ?\012 1))
          ) ; end if
          (setq done t)
        ) ; end case
      ) ; end cond
      (if (not done)
      ; then
        (progn
	  (setq menu-choice (read-string 
	    "(A)bort, (C)reate, (L)ist, (N)ext, (P)revious, or (S)elect: "))
	  (while (not 
            (string-match (upcase (substring menu-choice 0 1)) "ACLNPS"))
	    (ding)
	    (setq menu-choice (read-string 
	      "(A)bort, (C)reate, (L)ist, (N)ext, (P)revious, or (S)elect: "))
	  ) ; end while
        ) ; end then
      ) ; end if
    ) ; end while
  ) ; end let
)

(defun web-previous-change ()
  "Advances point to the beginning of the previous change.
ARGUMENTS : None
GLOBAL Variable : web-change-buffer-name
LOCAL Variable : start-position
RETURNS : Nothing
USES :  web-journal"
  (interactive)
  (web-journal "web-previous-change")
  (if (not (string= web-change-buffer-name (buffer-name (current-buffer))))
  ; then
    (error "Not in the CHange buffer.")
  ; else
    (let ((start-position (point)))
      (end-of-line)
      (if (not (re-search-backward "^@x" nil t 2))
      ; then
        (progn
          (goto-char start-position)
          (error "No previous CHange.")
        ) ; end then
      ; else
        (re-search-forward "^@y" nil t)
        (goto-char (match-beginning 0))        
        (recenter 0)
      ) ; end if
    ) ; end let
  ) ; end if
)

(defun web-previous-define ()
  "This function can only be invoked in the buffer containing the Web document.
It is used to traverse the list of Defined In occurrences of the most recently
selected module name in the backward direction.  A module name is selected by
issuing the command \\[web-view-module-names-list] and positioning the cursor
on the line that contains the module name of interest.
ARGUMENTS : None
GLOBAL Variables : web-defined-in-occurrence, web-defined-in-used-in-location,
                   and web-module-name-defined-in-used-in
LOCAL Variable : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-next-or-previous-define-or-use, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-previous-define")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string= (buffer-name (current-buffer))
              "*Module Name (Defined In) (Used In)*")
      ; then the user needs to select an entry
      (let ((buffer-to-be-returned-to 
              (web-next-or-previous-define-or-use "previous" "defined-in")))
        (if (< web-defined-in-occurrence 0)
        ; then there is no previous define
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No previous define of %s" (car (nth 
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
          ) ; end then
        ; else
          (web-goto-module (nth web-defined-in-occurrence (car (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in)))))
          (message "Define %d of %d of %s" (1+ web-defined-in-occurrence)
            (length (car (cdr (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in))))
            (car (nth web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; we are in a valid Web buffer
      (if (= web-defined-in-used-in-location -1)
          (error (concat "No selected module. "
            "Execute M-x web-view-module-names-list to select."))
      ; else
        (let ((current-module (web-what-module)))
          (setq web-defined-in-occurrence (1- (length 
            (car (cdr (nth web-defined-in-used-in-location 
            web-module-name-defined-in-used-in))))))
          (while (and (>= web-defined-in-occurrence 0)
                      (>= (nth web-defined-in-occurrence (car (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in))))
                        current-module)
                 ) ; end and
            (setq web-defined-in-occurrence (1- web-defined-in-occurrence))
          ) ; end while
          (if (< web-defined-in-occurrence 0)
          ; then there is no next define
            (error "No previous define of %s" (car (nth 
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))
          ; else
            (web-goto-module (nth web-defined-in-occurrence (car (cdr (nth
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in)))))
            (message "Define %d of %d of %s" (1+ web-defined-in-occurrence)
              (length (car (cdr (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in))))
              (car (nth web-defined-in-used-in-location 
                web-module-name-defined-in-used-in)))
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case  
  ) ; end cond
)

(defun web-previous-index ()
  "This function is used to traverse the list of occurrences of the most
recently selected index entry in the backward direction. An index entry is
selected by issuing the command \\[web-view-index] and positioning the cursor
on the line that contains the index entry of interest.
ARGUMENTS : None
GLOBAL Variables : web-location-in-selected-index-entry-occurrences,
                   web-selected-index-entry-occurrences,
                   web-index-buffer-name, and web-index-entry
LOCAL Variables : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-journal, web-next-or-previous-index, and web-what-module"
  (interactive)
  (web-journal "web-previous-index")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string-equal web-index-buffer-name (buffer-name (current-buffer)))
      ; then we are in the index
      (let ((buffer-to-be-returned-to (web-next-or-previous-index "previous")))
        (if (< web-location-in-selected-index-entry-occurrences 0)
        ; then there is no previous use
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No previous index reference for %s" web-index-entry)
          ) ; end then
        ; else
          (web-goto-module (nth 
            web-location-in-selected-index-entry-occurrences 
            web-selected-index-entry-occurrences))
          (message "Index Reference %d of %d for entry %s"
            (1+ web-location-in-selected-index-entry-occurrences)
            (length web-selected-index-entry-occurrences)
            web-index-entry)
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; else we are in a Web buffer
      (if (null web-selected-index-entry-occurrences)
      ; then
        (error 
          "Issue the command M-x web-view-index to select an index entry.")
      ; else
        (let ((current-module (web-what-module)))
          (setq web-location-in-selected-index-entry-occurrences 
            (1- (length web-selected-index-entry-occurrences)))
          (while (and (>= web-location-in-selected-index-entry-occurrences 0)
                      (>= (nth web-location-in-selected-index-entry-occurrences
                        web-selected-index-entry-occurrences)
                        current-module)
                 ) ; end and
            (setq web-location-in-selected-index-entry-occurrences 
              (1- web-location-in-selected-index-entry-occurrences))
          ) ; end while
          (if (< web-location-in-selected-index-entry-occurrences 0)
          ; then
            (error "No previous index reference for %s" web-index-entry)
          ; else
            (web-goto-module (nth 
              web-location-in-selected-index-entry-occurrences
              web-selected-index-entry-occurrences))
            (message "Index Reference %d of %d for entry %s"
              (1+ web-location-in-selected-index-entry-occurrences)
              (length web-selected-index-entry-occurrences)
              web-index-entry)
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case
  ) ; end cond
)

(defun web-previous-module ()
  "This function moves point to the beginning of the previous module.  If point
is currently in the limbo section of the Web or in module 1, an appropriate
message is sent to the minibuffer; otherwise, point is positioned at the first
line of the previous module.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, 
       web-goto-module, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-previous-module")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((current-module (web-what-module)))
    (if (<= current-module 1)
    ; then
      (error "No previous module")
    ; else
      (web-goto-module (1- current-module))
    ) ; end if
  ) ; end let
)

(defun web-previous-section ()
  "This function moves point to the beginning of the previous section.  If
point is currently in the limbo section of the Web or in the first section, an
appropriate message is sent to the minibuffer; otherwise, point is positioned
at the first line of the previous section.
ARGUMENTS : None
GLOBAL Variables : None
LOCAL Variable : current-section
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files,
       web-goto-section, web-journal, and web-what-section"
  (interactive)
  (web-journal "web-previous-section")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((current-section (web-what-section)))
    (if (<= current-section 1)
    ; then
      (error "No previous section")
    ; else
      (web-goto-section (1- current-section))
    ) ; end if
  ) ; end let
)

(defun web-previous-use ()
  "This function can only be invoked in the buffer containing the Web document.
It is used to traverse the list of Used In occurrences of the most recently
selected module name in the backward direction. A module name is selected by
issuing the command \\[web-view-module-names-list] and positioning the cursor
on the line that contains the module name of interest.
ARGUMENTS : None
GLOBAL Variables : web-defined-in-used-in-location, 
                   web-module-name-defined-in-used-in, and 
                   web-used-in-occurrence
LOCAL Variables : buffer-to-be-returned-to and current-module
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-goto-module, 
       web-next-or-previous-define-or-use, web-journal, and web-what-module"
  (interactive)
  (web-journal "web-previous-use")
  (web-check-if-buffer-is-one-of-the-web-files)
  (cond
    ((string= (buffer-name (current-buffer))
              "*Module Name (Defined In) (Used In)*")
      ; then the user needs to select an entry
      (let ((buffer-to-be-returned-to 
              (web-next-or-previous-define-or-use "previous" "used-in")))
        (if (< web-used-in-occurrence 0)
        ; then there is no previous use
          (progn
            (switch-to-buffer buffer-to-be-returned-to)
            (error "No previous use of %s" 
              (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
          ) ; end then
        ; else
          (web-goto-module (nth web-used-in-occurrence (car (cdr (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in))))))
          (message "Use %d of %d of %s" (1+ web-used-in-occurrence)
            (length (car (cdr (cdr (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in))))) 
            (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
        ) ; end if
      ) ; end let
    ) ; end case
    (t ; we are in a valid Web buffer
      (if (= web-defined-in-used-in-location -1)
          (error (concat "No selected module. "
            "Execute M-x web-view-module-names-list to select."))
      ; else
        (let ((current-module (web-what-module)))
          (setq web-used-in-occurrence (1- (length (car (cdr (cdr (nth
            web-defined-in-used-in-location 
            web-module-name-defined-in-used-in)))))))
          (while (and (>= web-used-in-occurrence 0)
                      (>= (nth web-used-in-occurrence (car (cdr (cdr (nth 
                        web-defined-in-used-in-location 
                        web-module-name-defined-in-used-in)))))
                        current-module)
                 ) ; end and
            (setq web-used-in-occurrence (1- web-used-in-occurrence))
          ) ; end while
          (if (< web-used-in-occurrence 0)
          ; then there is no next define
            (error "No previous use of %s" 
              (car (nth web-defined-in-used-in-location
              web-module-name-defined-in-used-in)))
          ; else
            (web-goto-module (nth web-used-in-occurrence (car (cdr (cdr (nth
              web-defined-in-used-in-location 
              web-module-name-defined-in-used-in))))))
            (message "Use %d of %d of %s" (1+ web-used-in-occurrence)
              (length (car (cdr (cdr (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in))))) 
              (car (nth web-defined-in-used-in-location
                web-module-name-defined-in-used-in)))
          ) ; end if
        ) ; end let
      ) ; end if
    ) ; end case  
  ) ; end cond
)

(defun web-reformat-the-index ()
  "This is an internal support function used to reformat the raw index produced
by Weave.  Control sequences must be removed and some reformatting is necessary
to clearly distinguish index entries (e.g., if an index entry occupies multiple
lines, the second and succeeding lines are indented two spaces).
ARGUMENTS : None
GLOBAL Variables : web-default-directory, web-index-buffer-name, and
                   web-mode-map
LOCAL Variables : ctr and line-to-be-inserted
RETURNS : Nothing
USES : web-eliminate-control-sequences and web-eliminate-white-space"
  (let ((ctr 0)
        line-to-be-inserted
       )
    (message "Reformatting the index...")
    (switch-to-buffer web-index-buffer-name)
    (use-local-map web-mode-map)
    (goto-char (point-min))
    (kill-line (count-lines (point-min) (point-max)))
    (insert-file (expand-file-name "INDEX.tex" web-default-directory))
    (save-excursion
      (re-search-forward "\\\\W?inx" nil t)
      (setq ctr (count-lines (point-min) (point)))
      (goto-char (point-min))
      (kill-line ctr)
    ) ; end excursion
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\\\\W?fin" nil t)
      (beginning-of-line)
      (kill-line (count-lines (point) (point-max)))
    ) ; end excursion
    (goto-char (point-min))
    ; indent index entries that occupy multiple lines
    ; index entries look like \:entry, occurrence-1, occurrence-2, ...,
    ; occurrence-n.
    ;
    ; WEAVE sometimes puts a %\n at the end of lines in the index.  See
    ; module 67 in the source for CWEAVE or module 122 in the source for
    ; the Pascal WEAVE.  Temporarily, make one long line out of these.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "%\n" nil t)
        (delete-char -2)
      ) ; end while
    ) ; end excursion
    ; Eliminate \: and the , followoing the index entry.
    (web-eliminate-control-sequences "\\\\:\\(.*\\)}," "}:")
    (web-eliminate-control-sequences "\\\\:\\\\|\\(.\\)," ":")
    ; FWEAVE sets built-in functions in the index as \:\@{function},
    ; Two lines above changes this to \@{function}:
    ; Now get rid of the \@{ and }
    (web-eliminate-control-sequences "\\\\@{\\(.*\\)}:" ":")
    ; Now entries to be set in roman type look like {...}: ... Now eliminate
    ; the braces.
    (web-eliminate-control-sequences "^{\\(.*\\)}:" ":")
    ; Eliminate \\{ and } from \\{...} (used for italicizing index entries
    ; of length greater than one
    (web-eliminate-control-sequences "\\\\\\\\{\\(.*\\)}")
    ; Eliminate \|{ and } from \|{...} (used for italicizing index entries
    ; of length one
    (web-eliminate-control-sequences "\\\\|{\\(.*\\)}")
    ; Eliminate \&{ and } from \&{...} (used for boldfacing index entries
    (web-eliminate-control-sequences "\\\\&{\\(.*\\)}")
    ; Eliminate \9{, }, and {...} from \9{...}{...} (used for placing entry 
    ; in a special way.  See p. 204 #11 in WEB User Manual.
    (web-eliminate-control-sequences "\\\\9{\\(.*\\)}{.*}")
    ; eliminate \.{ and } from \.{...} (used for placing index entries in a
    ; typewriter font
    (web-eliminate-control-sequences "\\\\\\.{\\(.*\\)}")
    ; Eliminate \[ and ] from \[...] which is used to underline an index
    ; occurrence entry.  The only characters that can be inside the brackets
    ; are digits and \* (the latter meaning that the module was changed).
    (web-eliminate-control-sequences "\\\\\\[\\([0-9*\\]*\\)\\]")
    (web-eliminate-control-sequences 
     "\\\\\\(\\\\\\|'\\|`\\|{\\|}\\|~\\| \\|_\\|&\\|#\\|\\$\\|%\\|\\^\\|\\*\\)"
    )
    ; FWEAVE places a control sequence \Windexspace before every group of
    ; index entries beginning with a particular letter. Eliminate these lines.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\\Windexspace" nil t)
        (beginning-of-line)
        (kill-line 1)
      ) ; end while
    ) ; end excursion
    (save-excursion (goto-char (point-min)) (replace-regexp "{\\\\AT}" "@"))
    ; Some index entries occupy multiple lines.  Indent the second and
    ; succeeding lines.
    (save-excursion
      (while (re-search-forward ".+: \\(.+,\n\\)*.+\\.\n" nil t)
        (if (/= (count-lines (point-min) (match-beginning 0))
                (count-lines (point-min) (match-end 0)))
        ; then
          (progn
            (setq line-to-be-inserted (web-eliminate-white-space 
              (buffer-substring
              (match-beginning 0) (match-end 0))))
            (delete-char (- 0 (length (buffer-substring (match-beginning 0)
              (match-end 0)))))
            (while (> (length line-to-be-inserted) 79)
              (setq ctr 79)
              (while (not (char-equal ? 
                (string-to-char 
                  (substring line-to-be-inserted (1- ctr) ctr))))
                (setq ctr (1- ctr))
              ) ; end while
              (insert-string (concat 
                (substring line-to-be-inserted 0 (1- ctr)) "\n"))
              (setq line-to-be-inserted (concat "  " 
                (substring line-to-be-inserted ctr)))
            ) ; end while
            (insert-string (concat line-to-be-inserted "\n"))
          ) ; end then
        ) ; end if
      ) ; end while
    ) ; end excursion
  ) ; end let
  (message "Reformatting the index...done")
)

(defun web-rename-module ()
  "This function allows a user to rename a module.  The user positions point
within the module that is to be changed.  The user can then change all, one,
or some occurrences of this module name.
ARGUMENTS : None
GLOBAL Variables : web-module-name-defined-in-used-in, web-module-names, and 
                   web-module-name
LOCAL Variables : case, ctr, current-position, defined, data, done, len,
                  len-old-module-name, module-name, module-name-begins-at,
		  module-name-ends-at, module-number-of-replacement,
		  modules-where-name-is-to-be-replaced, new-define,
		  new-module-name, new-use, next-module-begins, old-define,
		  old-module-name, old-use,
		  position-in-web-module-name-defined-in-used-in-list,
		  position-in-module-names, replace-module-name, and
		  temp-list
RETURNS : Nothing
USES : web-append-a-stub-module, web-binary-search-of-names, 
       web-check-if-buffer-is-one-of-the-web-files, web-eliminate-white-space,
       web-goto-module, web-view-module-names-list, web-journal and 
       web-what-module"
  (interactive)
  (web-journal "web-rename-module")
  (web-check-if-buffer-is-one-of-the-web-files)
  ; isolate the module name that point is in
  (let (case
        (ctr 0)
        (current-position (point))
        defined
        done
        (len 0)
        (len-old-module-name 0)
        module-name
        (module-name-begins-at 0)
        (module-name-ends-at 0)
        (module-number-of-replacement 0)
        modules-where-name-is-to-be-replaced
        new-define
        new-module-name
        new-use
        (next-module-begins 0)
        old-define
        old-module-name
        old-use
        (position-in-web-module-name-defined-in-used-in-list 0)
        (position-in-module-names 0)
        replace-module-name
        temp-list
       )
    (save-excursion
      (cond
        ((looking-at "<") 
          (forward-char 1)
        ) ; end case    
        ((looking-at "@<")
          (forward-char 2)
        ) ; end case
      ) ; end cond
      ; find the beginning of the module name, if possible
      (while (not done)
        (if (not (re-search-backward "\\(@@\\)*\\(@<\\)" nil t))
        ; then
          (progn
            (goto-char current-position)
            (setq done nil)
            (error "Point not positioned within a module name.")
          ) ; end then
        ; else
          (goto-char (match-beginning 0))
          (if (and (> (point) 1) 
                   (char-equal (preceding-char) ?\@))
          ; then point is not positioned at the beginning of a module name
            (setq done nil)
          ; else point is positioned at a valid module name beginning
            (goto-char (match-beginning 2))
            (setq done t)
          ) ; end if
        ) ; end if
      ) ; end while
      (setq module-name-begins-at (point)
            done nil)
      ; find the ending of the module name, if possible
      (while (not done)
        (if (not (re-search-forward "\\(@@\\)*\\(@>\\)" nil t))
        ; then
          (progn
            (goto-char current-position)
            (setq done nil)
            (error "Point not positioned within a module name.")
          ) ; end then
        ; else
          (goto-char (match-beginning 0))
          (if (char-equal (preceding-char) ?\@)
          ; then point is not positioned at the ending of a module name
            (progn
              (setq done nil)
              (goto-char (match-end 2))
            ) ; end then
          ; else point is positioned at a valid module name ending
            (goto-char (match-end 2))
            (setq done t)
          ) ; end if
        ) ; end if
      ) ; end while
      (setq module-name-ends-at (point))
      (if (< (point) current-position)
      ; then
        (error "Point not positioned within a module name.")
      ) ; end if
      (if (looking-at "=") (setq defined t))
      (setq old-module-name (web-eliminate-white-space 
        (buffer-substring (+ module-name-begins-at 2)
        (- module-name-ends-at 2))))
      (setq len-old-module-name (length old-module-name))
      (if (and (> len-old-module-name 3) 
                (string-equal "..."
                              (substring old-module-name 
                              (- len-old-module-name 3) len-old-module-name)))
      ; then
        (setq len-old-module-name (- len-old-module-name 3))
      ) ; end if
      (setq position-in-module-names (web-binary-search-of-names 
        web-module-names 
        (length web-module-names) old-module-name len-old-module-name))
      (setq position-in-web-module-name-defined-in-used-in-list
        (string-to-int (substring 
        (nth position-in-module-names web-module-names) 
        (+ (string-match "  "
        (nth position-in-module-names web-module-names)) 2))))
      (setq replace-module-name (web-eliminate-white-space
        (read-string (format "Replacement Module Name: "))))
      (if (> (web-binary-search-of-names web-module-names 
        (length web-module-names) 
        replace-module-name (length replace-module-name)) -1)
      ; then the replace-module-name is already in list of web-module-names
        (error "Replace module name has already been used.")
      ) ; end if
      ; ask the user if this is a global replacement, only one replacement,
      ; or replace some
      (setq case (read-string "Replace (a)ll, (o)ne, or (s)ome: "))
      (while (not (string-match (downcase (substring case 0 1)) "aos"))
        (ding)
        (setq case (read-string "Replace (a)ll, (o)ne, or (s)ome: "))
      ) ; end while
      (cond
        ((char-equal (string-to-char (downcase case)) ?a)
          (while (nth ctr (car (cdr (nth 
            position-in-web-module-name-defined-in-used-in-list 
            web-module-name-defined-in-used-in))))
            (setq modules-where-name-is-to-be-replaced (append
              modules-where-name-is-to-be-replaced
              (list (nth ctr (car (cdr 
              (nth position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in)))))))
            (setq ctr (1+ ctr))
          ) ; end while
          (setq ctr 0)
          (while (nth ctr (car (cdr (cdr (nth 
            position-in-web-module-name-defined-in-used-in-list 
            web-module-name-defined-in-used-in)))))
            (setq modules-where-name-is-to-be-replaced (append
              modules-where-name-is-to-be-replaced
              (list (nth ctr (car (cdr (cdr 
              (nth position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in))))))))
            (setq ctr (1+ ctr))
          ) ; end while
          (setq modules-where-name-is-to-be-replaced (sort
            modules-where-name-is-to-be-replaced '<))
          (while (car modules-where-name-is-to-be-replaced)
            (web-goto-module (car modules-where-name-is-to-be-replaced))
            (forward-char 2) ; get past the module beginning
            (save-excursion    
              (if (re-search-forward "^@\\(i\\| \\|\n\\|\t\\|\\*\\)" nil t)
              ; then
                (setq next-module-begins (point))
              ; else
                (setq next-module-begins (point-max))
              ) ; end if
            ) ; end excursion
            (message "Replacing name in Module %d" 
              (car modules-where-name-is-to-be-replaced))
            (while (re-search-forward web-module-name next-module-begins t)
              (setq module-name (web-eliminate-white-space
                (buffer-substring (match-beginning 2) (match-end 2))))
              (setq len (length module-name))
              (if (and (> len 3) (string-equal "..."
                    (substring module-name (- len 3) len)))
              ; then
                (setq len (- len 3))
              ) ; end if
              (if (string-equal (substring module-name 0 
                                  (min len len-old-module-name))
                                (substring old-module-name 0 
                                  (min len len-old-module-name)))
              ; then do the replacement
                (progn
                  (backward-char (+ (length (buffer-substring 
                    (match-beginning 2) (match-end 2))) 2))
                  (delete-char (length (buffer-substring 
                    (match-beginning 2) (match-end 2))))
                  (insert replace-module-name)
                ) ; end then
              ) ; end if
            ) ; end while
            (setq modules-where-name-is-to-be-replaced
              (cdr modules-where-name-is-to-be-replaced))
          ) ; end while
          ; update the web-module-names and 
          ; web-module-name-defined-in-used-in lists
          (setcar (nth position-in-web-module-name-defined-in-used-in-list
            web-module-name-defined-in-used-in) replace-module-name)
          (setq ctr 0
                temp-list nil)
          (while (< ctr (length web-module-names))
            (if (/= ctr position-in-module-names)
            ; then
              (setq temp-list (append temp-list 
                (list (nth ctr web-module-names))))
            ) ; end if
            (setq ctr (1+ ctr))
          ) ; end while
          (setq temp-list (append temp-list (list (concat
            replace-module-name "  " 
            (int-to-string 
            position-in-web-module-name-defined-in-used-in-list)))))
          (setq web-module-names (sort temp-list 'string<))
          (web-view-module-names-list)
        ) ; end case
        ((char-equal (string-to-char (downcase case)) ?o)
          ; rename only one occurrence
          (setq module-number-of-replacement (web-what-module)
                temp-list nil
                ctr 0)
          (if defined
          ; then
            (progn
              (if (= 1 (length (car (cdr 
                (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in)))))
              ; then
                (error (concat "Use the A option. "
                  "Attempt to rename module that was defined only once."))
              ; else
                (setq web-number-of-module-names 
                  (1+ web-number-of-module-names))
                (while (nth ctr (car (cdr 
                  (nth position-in-web-module-name-defined-in-used-in-list
                  web-module-name-defined-in-used-in))))
                  (if (/= module-number-of-replacement
                          (nth ctr (car (cdr 
                          (nth 
                          position-in-web-module-name-defined-in-used-in-list
                          web-module-name-defined-in-used-in)))))
                  ; then
                    (setq temp-list (append temp-list (list (nth ctr (car (cdr 
                      (nth position-in-web-module-name-defined-in-used-in-list
                      web-module-name-defined-in-used-in)))))))
                  ) ; end if
                  (setq ctr (1+ ctr))              
                ) ; end while                        
                (setcdr (nth 
                   position-in-web-module-name-defined-in-used-in-list
                   web-module-name-defined-in-used-in)
                  (append (list temp-list)
                  (list (car (cdr (cdr 
                  (nth position-in-web-module-name-defined-in-used-in-list
                  web-module-name-defined-in-used-in)))))))
                (setq web-module-name-defined-in-used-in (append
                  web-module-name-defined-in-used-in (list 
                  (list replace-module-name
                  (list module-number-of-replacement) (list ())))))
                (setq web-module-names (sort 
                  (append web-module-names (list (concat
                  replace-module-name "  " 
                  (int-to-string (1- web-number-of-module-names)))))
                  'string<))
              ) ; end if
            ) ; end then
          ; else
            (setq web-number-of-module-names (1+ web-number-of-module-names))
            (while (nth ctr (car (cdr (cdr 
              (nth position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in)))))
              (if (/= module-number-of-replacement
                      (nth ctr (car (cdr (cdr
                      (nth position-in-web-module-name-defined-in-used-in-list
                      web-module-name-defined-in-used-in))))))
              ; then
                (setq temp-list (append temp-list (list (nth ctr (car (cdr (cdr
                  (nth position-in-web-module-name-defined-in-used-in-list
                  web-module-name-defined-in-used-in))))))))
              ) ; end if
              (setq ctr (1+ ctr))              
            ) ; end while                        
            (setcdr (nth position-in-web-module-name-defined-in-used-in-list 
              web-module-name-defined-in-used-in)
              (append (list (car (cdr (nth 
              position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in))))
              (list temp-list)))
            (setq web-module-name-defined-in-used-in (append
              web-module-name-defined-in-used-in (list 
              (list replace-module-name
              (list (web-append-a-stub-module replace-module-name))
              (list ())))))
            (setq web-module-names (sort (append web-module-names (list (concat
              replace-module-name "  " 
              (int-to-string (1- web-number-of-module-names)))))
              'string<))
          ) ; end if
          (goto-char (+ module-name-begins-at 2))
          (delete-char (1- (- module-name-ends-at (point))))
          (insert replace-module-name)
          (web-view-module-names-list)
        ) ; end case
        ((char-equal (string-to-char (downcase case)) ?s)
          (setq old-define nil
                new-define nil
                old-use nil
                new-use nil
                defined nil)
          (while (nth ctr (car (cdr (nth 
            position-in-web-module-name-defined-in-used-in-list 
            web-module-name-defined-in-used-in))))
            (setq modules-where-name-is-to-be-replaced (append
              modules-where-name-is-to-be-replaced
              (list (nth ctr (car (cdr 
              (nth position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in)))))))
            (setq ctr (1+ ctr))
          ) ; end while
          (setq ctr 0)
          (while (nth ctr (car (cdr (cdr (nth 
            position-in-web-module-name-defined-in-used-in-list 
            web-module-name-defined-in-used-in)))))
            (setq modules-where-name-is-to-be-replaced (append
              modules-where-name-is-to-be-replaced
              (list (nth ctr (car (cdr (cdr 
              (nth position-in-web-module-name-defined-in-used-in-list
              web-module-name-defined-in-used-in))))))))
            (setq ctr (1+ ctr))
          ) ; end while
          (setq modules-where-name-is-to-be-replaced (sort
            modules-where-name-is-to-be-replaced '<))
          (while (car modules-where-name-is-to-be-replaced)
            (web-goto-module (car modules-where-name-is-to-be-replaced))
            (forward-char 2) ; get past the module beginning
            (save-excursion    
              (if (re-search-forward "^@\\(i\\| \\|\n\\|\t\\|\\*\\)" nil t)
              ; then
                (setq next-module-begins (point))
              ; else
                (setq next-module-begins (point-max))
              ) ; end if
            ) ; end excursion
            (while (re-search-forward web-module-name next-module-begins t)
              (setq module-name (web-eliminate-white-space
                (buffer-substring (match-beginning 2) (match-end 2))))
              (setq len (length module-name))
              (if (and (> len 3) (string-equal "..."
                    (substring module-name (- len 3) len)))
              ; then
                (setq len (- len 3))
              ) ; end if
              (if (string-equal (substring module-name 0 
                                  (min len len-old-module-name))
                                (substring old-module-name 0 
                                  (min len len-old-module-name)))
              ; then ask if it is to be replaced
                (progn
                  (let ((data (match-data)))
                    (unwind-protect
                      (if (looking-at "=") (setq defined t))
                      (store-match-data data)
                    ) ; end protect
                  ) ; end let
                  (recenter 0)
                  (if (y-or-n-p (format 
                    "Replace this %s occurrence in Module %d? "
                    (if defined "defined in" "used in") 
                    (car modules-where-name-is-to-be-replaced)))
                  ; then
                    (progn
                      (if defined
                      ; then
                        (setq new-define (append new-define 
                          (list (car modules-where-name-is-to-be-replaced))))
                      ; else
                        (setq new-use (append new-use
                          (list (car modules-where-name-is-to-be-replaced))))
                      ) ; end if
                      (backward-char (+ (length (buffer-substring 
                        (match-beginning 2) (match-end 2))) 2))
                      (delete-char (length (buffer-substring 
                        (match-beginning 2) (match-end 2))))
                      (insert replace-module-name)
                    ) ; end then
                  ; else
                    (if defined
                    ; then
                      (setq old-define (append old-define 
                        (list (car modules-where-name-is-to-be-replaced))))
                    ; else
                      (setq old-use (append old-use
                        (list (car modules-where-name-is-to-be-replaced))))
                    ) ; end if
                  ) ; end if
                ) ; end then
              ) ; end if
            ) ; end while
            (setq modules-where-name-is-to-be-replaced
              (cdr modules-where-name-is-to-be-replaced))
          ) ; end while
          (cond
            ((and (null old-define) (null old-use))
              ; similar to replace all -- the old name must be removed
              (setcar (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in) replace-module-name)
              (setq ctr 0
                    temp-list nil)
              (while (< ctr (length web-module-names))
                (if (/= ctr position-in-module-names)
                ; then
                  (setq temp-list (append temp-list 
                    (list (nth ctr web-module-names))))
                ) ; end if
                (setq ctr (1+ ctr))
              ) ; end while
              (setq temp-list (append temp-list (list (concat
                replace-module-name "  " (int-to-string 
                position-in-web-module-name-defined-in-used-in-list)))))
              (setq web-module-names (sort temp-list 'string<))
              (web-view-module-names-list)
            ) ; end case
            ((null old-define)
              ; append a stub for the old
              (setq web-number-of-module-names (1+ web-number-of-module-names))
              (setcdr (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in) (append
                (list (list (web-append-a-stub-module (car 
                (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in)))))
                (list old-use)))
              (setq web-module-name-defined-in-used-in (append
                web-module-name-defined-in-used-in 
                (list (list replace-module-name
                new-define new-use))))
              (setq web-module-names (sort 
                (append web-module-names (list (concat
                replace-module-name "  " 
                (int-to-string (1- web-number-of-module-names)))))
                'string<))
              (web-view-module-names-list)
            ) ; end case
            ((and (null new-define) (null new-use))
              ; do nothing -- no name has been replaced
            ) ; end case
            ((null new-define)
              ; append a stub for the replace module name
              (setq web-number-of-module-names (1+ web-number-of-module-names))
              (setcdr (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in)
                (append (list old-define) (list old-use)))
              (setq web-module-name-defined-in-used-in (append
                web-module-name-defined-in-used-in 
                (list (list replace-module-name
                (list (web-append-a-stub-module replace-module-name))
                new-use))))
              (setq web-module-names (sort 
                (append web-module-names (list (concat
                replace-module-name "  " 
                (int-to-string (1- web-number-of-module-names)))))
                'string<))
              (web-view-module-names-list)
            ) ; end case
            (t
              ; update both lists
              (setq web-number-of-module-names (1+ web-number-of-module-names))
              (setcdr (nth position-in-web-module-name-defined-in-used-in-list
                web-module-name-defined-in-used-in)
                (append (list old-define) (list old-use)))
              (setq web-module-name-defined-in-used-in (append
                web-module-name-defined-in-used-in 
                (list (list replace-module-name
                new-define new-use))))
              (setq web-module-names (sort 
                (append web-module-names (list (concat
                replace-module-name "  " 
                (int-to-string (1- web-number-of-module-names)))))
                'string<))
              (web-view-module-names-list)
            ) ; end case
          ) ; end cond
        ) ; end case
      ) ; end cond
    ) ; end excursion
  ) ; end let
)

(defun web-update-the-location-of-module-vector (next-char)
  "This function is invoked by \\[web-is-this-a-new-module-beginning] whenever
a new module is started.  The vector web-location-of-module must be updated to
reflect the addition of the new module.
ARGUMENTS : next-char (required)
GLOBAL Variables : web-location-of-module, web-number-of-modules-in-file, and
                   web-max-number-of-modules
LOCAL Variables : ctr, file-number, number-of-module-to-be-inserted,
                  relative-position, and section-number
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-modules, 
       and web-what-module"
  (let ((ctr (web-count-modules))
        (file-number (web-check-if-buffer-is-one-of-the-web-files))
        (number-of-module-to-be-inserted (web-what-module))
        (relative-position 0)
        (section-number 0)
       )
    (if (>= ctr web-max-number-of-modules)
    ; then
      (error "Increase the constant web-max-number-of-modules.")
    ; else
      (message "Updating the web-location-of-module vector...")
      (while (>= ctr number-of-module-to-be-inserted)
        (if (= file-number (aref (aref web-location-of-module ctr) 0))
        ; then increment its relative position in the file
          (progn
            (setq relative-position (aref (aref web-location-of-module ctr) 1))
            (aset (aref web-location-of-module ctr) 1 
              (1+ (aref (aref web-location-of-module ctr) 1)))
          ) ; end then
        ) ; end if
        (if (char-equal next-char ?*)
        ; then increment the section number
          (aset (aref web-location-of-module ctr) 2
            (1+ (aref (aref web-location-of-module ctr) 2)))
        ) ; end if
        (aset web-location-of-module (1+ ctr) 
          (aref web-location-of-module ctr))
        (setq ctr (1- ctr))
      ) ; end while
      (if (char-equal next-char ?*)
      ; then
        (setq section-number (1+ (aref
          (aref web-location-of-module 
          (1- number-of-module-to-be-inserted)) 2)))
      ; else
        (setq section-number (aref
          (aref web-location-of-module 
          (1- number-of-module-to-be-inserted)) 2))
      ) ; end if
      (aset web-location-of-module number-of-module-to-be-inserted
        (vector file-number
                (if (not (zerop relative-position)) 
                ; then
                  relative-position
                ; else
                  (1+ (aref web-number-of-modules-in-file file-number))
                ) ; end if
                section-number
                0
        ) ; end vector
      ) ; end aset
      (aset web-number-of-modules-in-file file-number 
        (1+ (aref web-number-of-modules-in-file file-number)))
      (message "Updating the web-location-of-module vector...done")
    ) ; end if
  ) ; end let
)

(defun web-update-the-module-name-defined-in-used-in-list (new-module-number)
  "This function is invoked when NEW-MODULE-NUMBER is inserted.  All module
numbers in the list web-module-name-defined-in-used-in must be updated to
reflect the insertion of this new module.
ARGUMENTS : new-module-number
GLOBAL Variable : web-module-name-defined-in-used-in
LOCAL Variables : ctr, defined-in-list, i, and used-in-list
RETURNS : Nothing
USES : Nothing"
  (let ((ctr 0)
        defined-in-list
        (i 0)
        used-in-list
      )
    (while (nth ctr web-module-name-defined-in-used-in)
      (setq defined-in-list ()
            used-in-list ()
            i 0
      ) ; end setq
      (while (nth i (car (cdr (nth ctr web-module-name-defined-in-used-in))))
        (if (<= new-module-number
                (nth i (car (cdr 
                  (nth ctr web-module-name-defined-in-used-in)))))
        ; then add 1 to the number and append to the list
          (setq defined-in-list (append defined-in-list (list (1+ 
            (nth i (car (cdr 
            (nth ctr web-module-name-defined-in-used-in))))))))
        ; else
          (setq defined-in-list (append defined-in-list (list
            (nth i (car (cdr (nth ctr web-module-name-defined-in-used-in)))))))
        ) ; end if
        (setq i (1+ i))
      ) ; end while
      (setq i 0)
      (while (nth i 
               (car (cdr (cdr (nth ctr web-module-name-defined-in-used-in)))))
        (if (<= new-module-number
            (nth i 
              (car (cdr (cdr (nth ctr web-module-name-defined-in-used-in))))))
        ; then add 1 to the number and append to the list
          (setq used-in-list (append used-in-list (list (1+ 
            (nth i (car (cdr (cdr (nth ctr
             web-module-name-defined-in-used-in)))))))))
        ; else
          (setq used-in-list (append used-in-list (list
            (nth i (car (cdr (cdr (nth ctr
             web-module-name-defined-in-used-in))))))))
        ) ; end if
        (setq i (1+ i))
      ) ; end while
      (setcdr (nth ctr web-module-name-defined-in-used-in)
        (append (list defined-in-list) (list used-in-list)))
      (setq ctr (1+ ctr))
    ) ; end while
  ) ; end let
)

(defun web-view-changed-modules-list ()
  "This function can be invoked from either the buffer containing the Web
document or the buffer containing the associated CHange file.  It displays in
the minibuffer a list of the modules in the Web document that have been changed
in the CHange file.
ARGUMENTS : None
GLOBAL Variable : web-location-of-module
LOCAL Variables : changed-modules
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-convert-list-to-string,
       web-count-modules and web-journal"
  (interactive)
  (web-journal "web-view-changed-modules-list")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let (changed-modules
        (ctr 0)
        (index 0)
        line-to-be-inserted
        (number-of-modules (web-count-modules))
       )
    (while (< ctr number-of-modules)
      (if (not (zerop (aref (aref web-location-of-module ctr) 3)))
      ; then
        (setq changed-modules (append changed-modules 
          (list ctr)))
      ) ; end if
      (setq ctr (1+ ctr))
    ) ; end while
    (get-buffer-create "*CHanged Modules*")
    (save-excursion
      (set-buffer "*CHanged Modules*")
      (goto-char (point-min))
      (kill-line (count-lines (point-min) (point-max)))
      (if (zerop (length changed-modules))
      ; then
        (setq line-to-be-inserted "No modules have been changed.")
      ; else
        (setq line-to-be-inserted
          (concat 
            (if (= (length changed-modules) 1) "Module " "Modules ")
            (web-convert-list-to-string changed-modules) " "
            (if (= (length changed-modules) 1) "has " "have ")
            "been changed."
          ) ; end concat
        ) ; end setq
      ) ; end if
      (if (> (length line-to-be-inserted) 79)
      ; then
        (progn
          (while (> (length line-to-be-inserted) 79)
            (setq index 79)
            (while (not (char-equal ? 
              (string-to-char 
                (substring line-to-be-inserted (1- index) index))))
              (setq index (1- index))
            ) ; end while
            (insert-string (concat 
              (substring line-to-be-inserted 0 (1- index)) "\n"))
            (setq line-to-be-inserted
              (substring line-to-be-inserted index))
          ) ; end while
          (insert-string (concat line-to-be-inserted "\n"))
        ) ; end then
      ; else
        (insert-string (concat line-to-be-inserted "\n"))
      ) ; end if
    ) ; end excursion
    (split-window-vertically
      (save-excursion
        (set-buffer "*CHanged Modules*")
        (goto-char (point-min))
        (- (screen-height) (max 3 (count-lines (point-min) (point-max))) 2)
      ) ; end excursion
    ) ; end split-window-vertically
    (other-window 1)
    (switch-to-buffer "*CHanged Modules*")
    (use-local-map web-mode-map)
    (setq major-mode 'Web-mode
          mode-name "Web")
    (other-window 1)
    (message "Type C-x 1 to remove the bottom window")
  ) ; end let
)

(defun web-view-index ()
  "This function is used when the user wishes to see an index for the Web as
produced by Weave.  The Web document is Weaved if there have been changes or if
no INDEX.tex file exists for the Web document.  The index is then reformatted
slightly to accommodate presentation by Emacs.
ARGUMENTS : None
GLOBAL Variable : web-change-buffer-name, web-selected-index-entry-occurrences,
                  web-buffer-name, and web-index-buffer-name
LOCAL Variables : message-string, size-of-buffer, and web-type
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-reformat-the-index, and
       web-journal"
  (interactive)
  (web-journal "web-view-index")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let (message-string
        (size-of-buffer 0)
        web-type
       )
    (setq web-selected-index-entry-occurrences nil)
    (setq web-index-buffer-name 
      (concat "*INDEX for " (buffer-name (current-buffer)) "*"))
    (get-buffer-create web-index-buffer-name)
    (save-excursion
      (set-buffer web-index-buffer-name)
      (setq size-of-buffer (buffer-size))
      (use-local-map web-mode-map)
      (setq major-mode 'Web-mode
            mode-name "Web")
    ) ; end excursion
    (if (and (not (buffer-modified-p 
               (get-file-buffer (expand-file-name web-change-buffer-name))))
             (not (buffer-modified-p 
               (get-file-buffer (expand-file-name web-buffer-name))))
             (not (zerop size-of-buffer))
        )
    ; then
      (switch-to-buffer web-index-buffer-name)
    ; else
      (save-some-buffers)
      (setq web-type (read-string 
        "Is this WEB, Pascal, C, or Fortran? (P, p, C, c, F, f) => "))
      (while (and (not (char-equal (upcase (string-to-char web-type)) ?C))
                  (not (char-equal (upcase (string-to-char web-type)) ?P))
                  (not (char-equal (upcase (string-to-char web-type)) ?F)))
        (ding)
        (setq web-type (read-string 
          "Is this WEB, Pascal, C, or Fortran? (P, p, C, c, F, f) => "))
      ) ; end while
      (save-excursion ; determine if CHange file is empty
        (set-buffer web-change-buffer-name)
        (setq size-of-buffer (buffer-size))
      ) ; end excursion
      (if (not (zerop size-of-buffer))
      ; then a CHange file exists
        (cond
          ((char-equal (upcase (string-to-char web-type)) ?C)
          ; then CWEAVE the file
            (setq message-string (concat "CWEAVEing " 
              web-buffer-name " and " web-change-buffer-name "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/cweave" nil 
              (get-buffer-create "WEAVE-Output") nil 
              web-buffer-name                  
              web-change-buffer-name)
          ) ; end case
          ((char-equal (upcase (string-to-char web-type)) ?F)
          ; then FWEAVE the file
            (setq message-string (concat "FWEAVEing " 
              web-buffer-name " and " web-change-buffer-name "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/fweave" nil 
              (get-buffer-create "WEAVE-Output") nil 
              web-buffer-name                  
              web-change-buffer-name)
          ) ; end case
          (t
          ; else WEAVE the file (it's a Pascal Web)
            (setq message-string (concat "WEAVEing " 
              web-buffer-name " and " web-change-buffer-name "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/weave" nil 
              (get-buffer-create "WEAVE-Output") nil 
              web-buffer-name
              web-change-buffer-name)
          ) ; end case
        ) ; end cond
      ; else no Change file exists
        (cond
          ((char-equal (upcase (string-to-char web-type)) ?C)
          ; then CWEAVE the file
            (setq message-string (concat 
              "CWEAVEing " (buffer-name (current-buffer)) "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/cweave" nil 
              (get-buffer-create "WEAVE-Output") nil 
              web-buffer-name)
          ) ; end case
          ((char-equal (upcase (string-to-char web-type)) ?F)
          ; then FWEAVE the file
            (setq message-string (concat 
              "FWEAVEing " (buffer-name (current-buffer)) "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/fweave" nil 
              (get-buffer-create "WEAVE-Output") nil 
              web-buffer-name)
          ) ; end case
          (t
            ; else WEAVE the file (it's a Pascal Web)
            (setq message-string 
              (concat "Pascal WEAVEing " (buffer-name (current-buffer)) "..."))
            (message "%s" message-string)
            (call-process "/usr/local/lib/tex/web/weave" nil 
            (get-buffer-create "WEAVE-Output") nil 
            web-buffer-name)
          ) ; end case
        ) ; end cond
      ) ; end if
      (message "%sdone" message-string)
      (sit-for 1)
      (web-reformat-the-index)
    ) ; end if
  ) ; end let
)

(defun web-view-module-names-list ()
  "This function can be invoked by the user to display a list of the module
names used in the Web.  A list of the modules in which the module name is
Defined In and Used In is also displayed.
ARGUMENTS : None
GLOBAL Variables : web-module-name-defined-in-used-in, web-module-names, 
                   and web-mode-map
LOCAL Variables : ctr, index, and ptr
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, 
       web-convert-list-to-string, and web-journal"
  (interactive)
  (web-journal "web-view-module-names-list")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((ctr 0)
        (index 0)
        (ptr 0)
       )
    (switch-to-buffer "*Module Name (Defined In) (Used In)*")
    (use-local-map web-mode-map)
    (setq major-mode 'Web-mode
          mode-name "Web")
    (goto-char (point-min))
    (kill-line (count-lines (point-min) (point-max)))
    (while (nth ctr web-module-names)
      (setq ptr (string-to-int (substring (nth ctr web-module-names)
        (+ (string-match "  " (nth ctr web-module-names)) 2))))
      (setq line-to-be-inserted (concat 
        (car (nth ptr web-module-name-defined-in-used-in)) 
        " ("
        (web-convert-list-to-string 
          (car (cdr (nth ptr web-module-name-defined-in-used-in)))) 
        ") (" 
        (web-convert-list-to-string 
          (car (cdr (cdr (nth ptr web-module-name-defined-in-used-in))))) 
        ")")
      ) ; end setq
      (if (> (length line-to-be-inserted) 79)
      ; then
        (progn
          (while (> (length line-to-be-inserted) 79)
            (setq index 79)
            (while (not (char-equal ? 
              (string-to-char 
                (substring line-to-be-inserted (1- index) index))))
              (setq index (1- index))
            ) ; end while
            (insert-string (concat 
              (substring line-to-be-inserted 0 (1- index)) "\n"))
            (setq line-to-be-inserted (concat "  " 
              (substring line-to-be-inserted index)))
          ) ; end while
          (insert-string (concat line-to-be-inserted "\n"))
        ) ; end then
      ; else
        (insert-string (concat line-to-be-inserted "\n"))
      ) ; end if
      (setq ctr (1+ ctr))
    ) ; end while
    (goto-char (point-min))
  ) ; end let
)

(defun web-view-section-names-list ()
  "This function prints a list of the major section names in the Web document.
It also shows the module number where each major section begins.  The list is
displayed in another buffer, *Section Names*.
ARGUMENTS : None
GLOBAL Variable : web-mode-map
LOCAL Variables : ctr, number-of-modules, section-name, section-names,
                  size-of-buffer, and space-on-left
RETURNS : Nothing
USES : web-check-if-buffer-is-one-of-the-web-files, web-collect-section-names,
       web-count-modules and web-journal"
  (interactive)
  (web-journal "web-view-section-names-list")
  (web-check-if-buffer-is-one-of-the-web-files)
  (get-buffer-create "*Section Names*")
  (save-excursion
    (set-buffer "*Section Names*")
    (setq size-of-buffer (buffer-size))
  ) ; end excursion
  (let* ((ctr 0)
         (number-of-modules (web-count-modules))
         section-name
         (section-names (web-collect-section-names))
         (space-on-left
           (/ (- 7 (length (int-to-string number-of-modules))) 2))
        )
    (switch-to-buffer "*Section Names*")
    (use-local-map web-mode-map)
    (setq major-mode 'Web-mode
          mode-name "Web")
    (goto-char (point-min))
    (kill-line (count-lines (point) (point-max)))
    (insert-string "Module#  Section Name\n")
    (while (nth ctr section-names)
      (setq section-name (car (nth ctr section-names)))
      (if (> (length section-name) 71)
      ; then
        (setq section-name (concat (substring section-name 0 68) "..."))
      ) ; end if
      (insert-string (concat (make-string (+ space-on-left
        (- (length (int-to-string number-of-modules))
        (length (int-to-string (car (cdr (nth ctr section-names))))))) ? )
        (int-to-string (car (cdr (nth ctr section-names))))
        (make-string (- 9 (+ space-on-left 
        (length (int-to-string number-of-modules)))) ? )
        section-name "\n"))
      (setq ctr (1+ ctr))
    ) ; end while
    (goto-char (point-min))
  ) ; end let
)

(defun web-what-change ()
  "This function can only be invoked in the buffer containing the CHange file
associated with the Web document that \\[web-mode] was used on most recently.
Based on point's position in the CHange file buffer, this function displays a
descriptive message indicating what module is being changed.
ARGUMENTS : None
GLOBAL Variables : web-change-buffer-name, web-location-of-module,
                   and web-buffer-name
LOCAL Variables : bound-for-search, index, module-number, 
                  next-change-begins-at, number-of-changes, 
                  and number-of-preceding-@x
RETURNS : module-number
USES : web-journal"
  (interactive)
  (web-journal "web-what-change")
  (if (not (string= (buffer-name (current-buffer)) web-change-buffer-name))
  ; then
    (error "Not in CHange buffer associated with %s" web-buffer-name)
  ; else
    (if (zerop (buffer-size))
    ; then
      (error "CHange buffer is empty")
    ; else
      (let ((bound-for-search 0)
            (module-number 0)
            (next-change-begins-at 0)
            (number-of-preceding-@x 0)
           )
        (save-excursion
          (end-of-line)
          (setq bound-for-search (point))
          (if (not (re-search-backward "^@x" nil t))
          ; then
            (error "Point not positioned between an @x @z")
          ) ; end if
          (forward-char (length 
            (buffer-substring (match-beginning 0) (match-end 0))))
          (save-excursion
            (if (re-search-forward "^@x" nil t)
            ; then
              (setq next-change-begins-at (- (point) 
                (length (buffer-substring (match-beginning 0) (match-end 0)))))
            ; else
              (setq next-change-begins-at (point-max))
            ) ; end if
          ) ; end excursion
          (if (not (re-search-forward "^@z" next-change-begins-at t))
          ; then
            (error "This CHange has no matching @z")
          ) ; end if
          (if (> bound-for-search (save-excursion (end-of-line) (point)))
          ; then
            (error "Point not positioned between an @x @z")
          ; else
            (goto-char (point-min))
            (while (re-search-forward "^@x" bound-for-search t)
              (setq number-of-preceding-@x (1+ number-of-preceding-@x))
            ) ; end while
          ) ; end if
        ) ; end excursion
        (while (< (aref (aref web-location-of-module module-number) 4)
                  number-of-preceding-@x)
          (setq module-number (1+ module-number))
        ) ; end while
        (if (> (aref (aref web-location-of-module module-number) 4)
               number-of-preceding-@x)
        ; then search back to the previous change
          (progn
            (setq module-number (1- module-number))
            (while (/= (aref (aref web-location-of-module module-number) 3) 1)
              (setq module-number (1- module-number))
            ) ; end while
          ) ; end then
        ) ; end if
        (message "CHange corresponds to Module %d" module-number)
        module-number ; return the module number of the CHange
      ) ; end let
    ) ; end if
  ) ; end if
)

(defun web-what-module ()
  "This function tells the user what module point is currently positioned in.
ARGUMENTS : None
GLOBAL Variables : web-default-directory, web-files, and web-module-begins
LOCAL Variables : ctr, done, file-number, module-count, and stopping-point
RETURNS : the module # that point is currently positioned in
USES : web-check-if-buffer-is-one-of-the-web-files, web-count-the-matches and
       web-journal"
  (interactive)
  (web-journal "web-what-module")
  (save-excursion
    (let ((ctr 1)
          done
          (file-number (web-check-if-buffer-is-one-of-the-web-files))
          (module-count 0)
          (stopping-point (make-vector (length web-files) 0))
         )
      (while (nth ctr web-files)
        (save-excursion
          (set-buffer (get-file-buffer (expand-file-name (nth ctr web-files)
            web-default-directory)))
          (goto-char (point-max))
          (aset stopping-point ctr (point))
        ) ; end escursion
        (setq ctr (1+ ctr))
      ) ; end while
      (aset stopping-point file-number 
        (save-excursion (beginning-of-line) (point)))
      (save-excursion
        (set-buffer (get-file-buffer (expand-file-name (nth 1 web-files)
          web-default-directory)))
        (web-count-the-matches "^@\\(i\\| \\|\t\\|\n\\|\\*\\)")
      ) ; end excursion
      (beginning-of-line)
      (if (looking-at web-module-begins)
      ; then
        (setq module-count (1+ module-count))
      ) ; end if
    (message "Module %d" module-count)
    module-count ; return the module #
    ) ; end let
  ) ; end excursion
)

(defun web-what-section ()
  "This function tells the user what section the cursor is currently positioned
in.
ARGUMENTS : None
GLOBAL Variables : web-location-of-module and web-files
LOCAL Variables : current-module
RETURNS : the section number that point is positioned in
USES : web-check-if-buffer-is-one-of-the-web-files, web-journal, 
       and web-what-module"
  (interactive)
  (web-journal "web-what-section")
  (web-check-if-buffer-is-one-of-the-web-files)
  (let ((current-module (web-what-module)))
    (message "Section %d in %s"
      (aref (aref web-location-of-module current-module) 2)
      (upcase (nth 1 web-files)))
    ; return the section #
    (aref (aref web-location-of-module current-module) 2) 
  ) ; end let
)

(defun web-write-module-names-to-a-file ()
  "This is an internal support function that writes the module names,
defined-in lists, and Used In lists to a file.  This is done so that initial
startup can be speeded up.
ARGUMENTS : None
GLOBAL Variable : web-buffer-name
LOCAL Variables : None
RETURNS : Nothing
USES : web-view-module-names-list"
  (save-excursion
    (web-view-module-names-list)
    (write-region 1 (point-max) (concat (substring web-buffer-name 0 
      (string-match "\\." web-buffer-name)) ".mods") nil 1)
  ) ; end excursion
)
