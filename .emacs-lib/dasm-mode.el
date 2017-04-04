;;; dasm-mode
;
; Works fine with DAsm-style 6502 source code
; Requires Emacs 20.x or higher
;
; Questions or suggestions to MagerValp(at)cling.gu.se
;
; Copyright 2002 Per Olofsson
; Released under the Gnu General Public License, GPL

(setq auto-mode-alist (cons '("\\.asm$" . dasm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.i$" . dasm-mode) auto-mode-alist))

(defvar dasm-font-lock-keywords
  '(
    ("\\(;.*\\)$" . font-lock-comment-face)
    ("^\\([a-zA-Z0-9_:]+\\)\\b" . font-lock-constant-face)
    ("[ \t]+\\(d[csv].[bwl]?\\|hex\\|eqm\\|equ\\|set\\)\\b" . font-lock-type-face)
    ("[ \t]+\\(seg\\|seg.u\\|org\\|rorg\\|rend\\|align\\)\\b" . font-lock-warning-face)
    ("[ \t]+\\(include\\|incbin\\|incdir\\|ifconst\\|ifnconst\\|if\\|else\\|endif\\|eif\\|subroutine\\|repeat\\|repend\\)\\b" . font-lock-variable-name-face)
    ("[ \t]+\\(processor\\|err\\|echo\\|list\\)\\b" . font-lock-builtin-face)
    ("[ \t]+\\(adc\\|and\\|asl\\|bcc\\|bcs\\|beq\\|bit\\|bmi\\|bne\\|bpl\\|brk\\|bvc\\|bvs\\|clc\\|cld\\|cli\\|clv\\|cmp\\|cpx\\|cpy\\|dec\\|dex\\|dey\\|eor\\|inc\\|inx\\|iny\\|jmp\\|jsr\\|lda\\|ldx\\|ldy\\|lsr\\|nop\\|ora\\|pha\\|php\\|pla\\|plp\\|rol\\|ror\\|rti\\|rts\\|sbc\\|sec\\|sed\\|sei\\|sta\\|stx\\|sty\\|tax\\|tay\\|tsx\\|txa\\|txs\\|tya\\)\\b" . font-lock-keyword-face)
    )
  "Expressions to highlight in dasm-mode.")

(define-derived-mode dasm-mode fundamental-mode "DAsm"
  "Mode for editing DAsm cross assembler source."
  (interactive)
  (setq tab-width 16)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "$")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dasm-font-lock-keywords nil t))

)
