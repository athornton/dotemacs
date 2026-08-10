; Because of the below, just don't bother with native compilation.
(setq native-comp-deferred-compilation nil)

; This stuff doesn't seem to work; you still get a zillion warnings at
; startup about mmacos-min-version.

(defun my-append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
                     value)))

(let ((gccjitpath "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib"))
  (mapc (lambda (var-name) (my-append-env-var var-name gccjitpath))
        '("LIBRARY_PATH" "LD_LIBRARY_PATH" "PATH")))
