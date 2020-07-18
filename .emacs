;;; .emacs --- My .emacs file

;;; Commentary:
					; Maintenance Log:
; 01/31/92 DEB commented out LaTeX-mode definition and set .tex files
;              to invoke tex-mode plus auto-fill-mode.
;
;
;	set up some globals
;
;
;	Add to your .login (for example):
;
;	setenv	EMACSLOADPATH	".:/local/lib/emacs/lisp"
;
;	or, (BETTER) fix up the definition below:
;
;
;
;	David M. Meyer
;	meyer@uoregon
;	12-Aug-86
;

; Oh, man, I haven't kept up the log.  But this thing has been growing
; organically since I got it from David in 1992.  It's March 2007 now, by the
; way -- AJT

; Now it's June, 2011. -- AJT

; Oh hey welcome to April 2014. -- AJT

; July 2016: put it on github -- AJT

; Make gc-cons-threshold huge for init

;;; Code:
(setq gc-cons-threshold 100000000)


(setq load-path (cons "~adam/.emacs-lib" load-path))

; Mac Stuff
;;; MacOS X specific stuff
(defvar mac-command-key-is-meta)
(setq mac-command-key-is-meta nil)

;; Define the return key to avoid problems on MacOS X
(define-key function-key-map [return] [13])

(global-set-key [(alt a)] 'mark-whole-buffer)
(global-set-key [(alt v)] 'yank)
(global-set-key [(alt c)] 'kill-ring-save)
(global-set-key [(alt x)] 'kill-region)
(global-set-key [(alt s)] 'save-buffer)
(global-set-key [(alt l)] 'goto-line)
(global-set-key [(alt o)] 'find-file)
(global-set-key [(alt f)] 'isearch-forward)
(global-set-key [(alt g)] 'isearch-repeat-forward)
(global-set-key [(alt w)]
                (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [(alt .)] 'keyboard-quit)

;; Set transparency
(setq default-frame-alist
      (append
       (list
	'(active-alpha . 0.92)  ;; active frame
	'(inactive-alpha . 0.8) ;; non active frame
	) default-frame-alist)
      )


;; I disabled this since I want to avoid hitting Cmd-q accidentally.
;(global-set-key [(alt q)] 'save-buffers-kill-emacs)

(require 'redo+)
(global-set-key [(alt z)] 'undo)
(global-set-key [(alt shift z)] 'redo)

;; More Mac stuff
;; Load Enhanced Carbon Emacs plugin
;;(unless (or (boundp 'enhanced-carbon-emacs)
;;	        (boundp 'aquamacs-version))
  (defun load-local-site-start (site-lisp-directory)
    "Load site-start.el from a given site-lisp directory"
    (let ((current-default-directory default-directory))
      (setq default-directory site-lisp-directory)
      (normal-top-level-add-subdirs-to-load-path)
      (setq default-directory current-default-directory)
      (setq load-path (cons site-lisp-directory load-path))
      (load (concat site-lisp-directory "/site-start.el"))
      ))
;;  (load-local-site-start
;;   "/Library/Application Support/emacs/ec-emacs/site-lisp"))
;; But these piss me off
(setq confirm-kill-emacs nil)
;;(unless (boundp 'aquamacs-version)
;;  (paren-deactivate))

;; Save state between sessions?
;; (load "desktop")
;; (desktop-load-default)
;; (desktop-read)
;; (desktop-save "~/")

;; Edit server for editing in Chrome.

(if (locate-library "edit-server")
            (progn
                    (require 'edit-server)
                    (setq edit-server-new-frame nil)
                    (edit-server-start)))

;;-------------------------------------------------------
;; begin sourcing of .bash_profile

;; only do this on Mac OS X
(when (string= system-type "darwin")
  ;; require common lisp extensions, for search
  (require 'cl)


  (defun src-shell-unescape (string)
    ;; replace \n \t \r \b \a \v \\
    ;; and octal escapes of the form \0nn

    (replace-regexp-in-string
     "\\\\\\([ntrbav]\\|\\(\\\\\\)\\|\\(0[0-7][0-7]\\)\\)"
     (lambda (str)
       ;; interpret octal expressions
       ;; of the form "\0nn"
       (let ((char1 (aref str 1)))
     (cond ((= ?0 (aref str 1))
        (byte-to-string
         (+ (* (- (aref str 2) ?0) 8)
            (- (aref str 3) ?0))))
           ((eq char1 ?n) "\n")
           ((eq char1 ?t) "\t")
           ((eq char1 ?r) "\r")
           ((eq char1 ?b) "\b")
           ((eq char1 ?a) "\a")
           ((eq char1 ?v) "\v")
           ((eq char1 ?\\) "\\\\")
           (t "")))) string))

  (defun src-set-environment-from-env-output(env-output)
    ;; set the environment from shell's "env" output
    (let ((lines (split-string env-output "\n" t)))
      (dolist (line lines)
    (let ((idx-equals (search "=" line)))
      (when (and (not (eq idx-equals nil))
             (> idx-equals 1))
        (let  ((key (substring line 0 idx-equals))
           (value (substring line (+ idx-equals 1))))
          (setenv key (src-shell-unescape value))
          ;; (message "%s = %s" key value)
          ))))))

  (defun src-source-shell-file (file-name)
    ;; if your shell is sh rather than bash, the "source " may need
    ;; to be ". " instead
    (let* ((command (concat "source '"  file-name "'; echo 'post-env'; env"))
       (output (shell-command-to-string command))
       (idx-post-env (search "post-env" output)))
      (if (eq nil idx-post-env)
      (message "Didn't find expected output after sourcing %s. Found: %s" file-name output)
    (let ((trimmed-output (substring output idx-post-env)))
      ;; (message "trimmed-output: %s" trimmed-output)
      (src-set-environment-from-env-output trimmed-output)))))



  (src-source-shell-file (expand-file-name "~/.bash_profile")))


;; end sourcing of .bash_profile
;;-------------------------------------------------------


;; This is pretty much the core of the ancient stuff...

;; (setq gc-cons-threshold 524288)		;512K
;; You know, that'd been there since at least '92.  Maybe it should
;;  be bigger in 2007.
(setq gc-cons-threshold (max 4000000 gc-cons-threshold)) ; At least 4M
(setq large-file-warning-threshold 100000000)   ; 100M?  Should be OK.
(setq max-lisp-eval-depth 60000)                ; Or so Deech says.
(setq max-specpdl-size 13000)                   ; Ditto.
(setq default-case-fold-search nil)	;distingush upper/lower cases
(setq require-final-newline t)		;ask
(setq default-major-mode 'text-mode)	;some hooks...
(setq shell-prompt-pattern "^[^#$%>)]*[#$%>)]")
(setq text-mode-hook
      (function
       (lambda ()
	 "Set fill column and turn on auto-fill-mode"
	 (setq fill-column 72)
	 (setq indent-line-function 'indent-relative-maybe)
	 (auto-fill-mode 1))))
(setq mail-mode-hook text-mode-hook)

(setq enable-arrow-keys t)              ;VT100 emulation shit
(blink-cursor-mode -1);                 ;the blink is awful

;;; do customizations that have nothing to do with my files

(transient-mark-mode 1)                ;; i like highlighting of regions
(line-number-mode 1)                   ;; i like line numbers

;; Fix backup locations
(defvar user-emacs-directory "~/.emacs.d/")
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t    ; backup of a file the first time it is saved.
      backup-by-copying t    ; don't clobber symlinks
      version-control t      ; version numbers for backup files
      delete-old-versions t  ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6    ; oldest versions to keep when a new numbered backup
                             ; is made (default: 2)
      kept-new-versions 9    ; newest versions to keep when a new numbered backup
                             ; is made (default: 2)
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 100 ; number of keystrokes between auto-saves (default: 300)
      )


;; ;;;;;;; Language support

(defun setup-tide-mode () "Set up tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Add Inform-mode

(autoload 'inform-mode "inform-mode" "Inform editing mode." t)
  (autoload 'inform-maybe-mode "inform-mode" "Inform/C header editing mode.")
  (setq auto-mode-alist
        (append '(("\\.h\\'"   . inform-maybe-mode)
                  ("\\.i6\\'"  . inform-mode)
                  ("\\.inf\\'" . inform-mode))
                auto-mode-alist))

;; To turn on font locking add:
(add-hook 'inform-mode-hook 'turn-on-font-lock)

(autoload 'tads-mode "tads2-mode" "TADS 2 editing mode." t)
  (setq auto-mode-alist
        (append (list (cons "\\.t$" 'tads-mode))
                auto-mode-alist))
(add-hook 'tads-mode-hook 'turn-on-font-lock)


(autoload 'noweb-mode "noweb-mode" "No-web literate programming editing mode." t)
  (setq auto-mode-alist
        (append (list (cons "\\.w$" 'noweb-mode))
                auto-mode-alist))
(add-hook 'noweb-mode-hook 'turn-on-font-lock)

;; COBOL mode, God help us
(autoload 'cobol-mode "cobol" "COBOL Editing mode" t)
   (setq auto-mode-alist
          (append '(("\\.cbl$" . cobol-mode)
                    ("\\.CBL$" . cobol-mode)
                    ("\\.COB$" . cobol-mode)
                    ("\\.cob$" . cobol-mode)
                    ("\\.COBOL$" . cobol-mode)
                    ("\\.cobol$" . cobol-mode)
		    ("\\.CPY$" . cobol-mode)
		    ("\\.cpy$" . cobol-mode)
	    ) auto-mode-alist))

;; Fix case sensitivity
(add-hook 'cobol-mode-hook '(lambda ()
			      (set (make-local-variable 'dabbrev-case-fold-search) t)
			      (set (make-local-variable 'dabbrev-case-replace) t)))

(add-hook 'cobol-mode-hook 'turn-on-font-lock)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; PL/1 Mode
(autoload 'pli-mode "pli-mode" "PL/I editing mode." t)
  (setq auto-mode-alist
        (append (list (cons "\\.pli$" 'pli-mode))
                auto-mode-alist))
(add-hook 'pli-mode-hook 'turn-on-font-lock)

;; Rexx Mode
(autoload 'rexx-mode  "rexx-mode"  "REXX mode" t)
(autoload 'rexx-debug "rexx-debug" "REXX source level debugger" t)
(setq auto-mode-alist
      (append
       (list (cons "\\.rexx$"  'rexx-mode)
             (cons "\\.REXX$"  'rexx-mode)
             (cons "\\.elx$"   'rexx-mode)
             (cons "\\.exec$"  'rexx-mode)
             (cons "\\.EXEC$"  'rexx-mode)
             (cons "\\.ncomm$" 'rexx-mode)
             (cons "\\.cpr$"   'rexx-mode)
             )
       auto-mode-alist))

(add-hook 'rexx-mode-hook 'turn-on-font-lock)

;; Docbook Mode
(autoload 'docbook-xml-mode "docbook-xml-mode" "Docbook XML editing mode." t)
(add-hook 'docbook-xml-mode-hook 'turn-on-font-lock)

;; Auto font lock mode
(defvar font-lock-auto-mode-list
  (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode
	'perl-mode 'scheme-mode 'scribe-mode 'shell-script-mode 'cobol-mode
	'dired-mode 'inform-mode 'tads2-mode 'pli-mode 'rexx-mode
	'docbook-xml-mode)
  "List of modes to always start in 'font-lock-mode'.")

(defvar font-lock-mode-keyword-alist
  '((c++-c-mode . c-font-lock-keywords)
    (perl-mode . perl-font-lock-keywords)
    (cobol-mode . cobol-font-lock-keywords)
    (tads2-mode . tads2-font-lock-keywords)
    (inform-mode . inform-font-lock-keywords)
    (pli-mode . pli-font-lock-keywords)
    (rexx-mode . rexx-font-lock-keywords)
    (dired-mode . dired-font-lock-keywords))
  "Associations between modes and keywords.")

(defun hook-c () "Set up C editing parameters."
  (setq c-set-style "k&r")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'knr-argdecl-intro 0))
(add-hook 'c-mode-common-hook 'hook-c)

;(setq c-basic-indent 2)
;(setq tab-width 4)
;(setq indent-tabs-mode nil )

(defun fixssh ()
  "Run fixssh script for use in GNU screen with X forwarding."
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect "~/bin/fixssh")))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (while (re-search-forward
              "\\([A-Z_][A-Z0-9_]*\\) *= *\"\\([^\"]*\\)\"" nil t)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (setenv key val)))
      (kill-buffer buffer))))
;;; Some interesting stuff from Russ, good for finding stealth EOL whitespace
(if window-system
    (add-hook 'post-command-hook
              (lambda () (setq cursor-type (if (eolp) '(bar . 6) t)))))

(setq user-mail-address "<adam@fsf.net>")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(delete-selection-mode nil nil (delsel))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(mail-host-address "fsf.net")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control) . 1))))
 '(package-selected-packages
   (quote
    (rainbow-delimiters ox-reveal company tide groovy-mode yaml-mode edit-server ess go-mode dockerfile-mode coffee-mode markdown-mode flycheck exec-path-from-shell py-autopep8 powershell icicles csharp-mode)))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(js-indent-level 2)
 '(typescript-indent-level 2))

;(add-to-list 'default-frame-alist '(font . "Fira Code-18"))
; or, on the netbook, 10-point.
; and make it shorter
(if window-system (
    if  ( < (display-pixel-height) 800 )
	( prog1 ( set-frame-size (selected-frame) 80 32)
	  ( add-to-list 'default-frame-alist '( font . "Fira Code-10")))
	( prog1 ( set-frame-size (selected-frame) 80 40)
	  (if ( string-equal system-type "darwin" )
	      (if ( < (display-pixel-height) 2000) ; 4K monitors...
		  ( add-to-list 'default-frame-alist '( font . "Fira Code-18"))
		( add-to-list 'default-frame-alist '( font . "Fira Code-24")))
	    ( add-to-list 'default-frame-alist '( font . "Fira Code-12"))))))

;; Ligature support (for Fira Code)

(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

; But not in helm
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

; Add MELPA, so I can easily get icicles and other things
(when (> emacs-major-version 23)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/"))


; Apparently needed for the package auto-complete (why?)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)
  (setq url-http-attempt-keepalives nil)
)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (require 'py-autopep8 nil 'noerror)
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

; No more editing the wrong copy of stuff...
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; Turn on flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

; Org-mode
;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org-reveal-presentation()
  ;; Borrowed from Rebecca Skinner
  "Setup 'org-mode' for exporting to reveal.js."
  (interactive)
  (require 'ox-reveal)
  (defvar code-block-history '())

  (defun org-html-start-end-frag (src backend _info)
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "{{{\\(.+?\\)}}}" "<span class=\"fragment fade-in\">\\1</span>" src)))

  (defun org-html-start-frag (src backend _info)
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "<span .+>{-</span><span .+?>start-frag-}</span>" "<span class=\"fragment fade-in\">" src)))

  (defun org-html-end-frag (src backend _info)
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "<span .+>{-</span><span .+?>end-frag-}</span>" "</span>" src)))

  (defun insert-code-block-without-contents(lang)
    (insert (format "#+begin_src %s" lang))
    (newline-and-indent)
    (insert "#+end_src")
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )
  (defun insert-code-block-with-contents(lang contents)
    (insert-code-block-without-contents lang)
    (insert contents)
    (next-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun add-code-block ()
    "Add a code block without spawning a mini-window."
    (interactive)
    (let ((lang (read-string "Code block language: " nil 'code-block-history)))
      (add-to-history 'code-block-history lang)
      (insert-code-block-without-contents lang)
      )
    )

  (global-set-key (kbd "C-c b") 'add-code-block)

  (setq org-export-filter-src-block-functions ())
  (add-to-list 'org-export-filter-src-block-functions 'org-html-start-end-frag)
  (add-to-list 'org-export-filter-src-block-functions 'org-html-start-frag)
  (add-to-list 'org-export-filter-src-block-functions 'org-html-end-frag)
  )

(if (not (require 'ox-reveal nil t))
    (message "`ox-reveal not found"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold 4000000) ; Reset to a sane small value after init.
(provide '.emacs)
;;; .emacs ends here
