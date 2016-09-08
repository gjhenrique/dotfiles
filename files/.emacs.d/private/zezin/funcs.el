; Inspired by https://github.com/kaushalmodi/.emacs.d/
(defvar modi/fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")

(defconst modi/fns-regexp (concat "(\\s-*"
                                  "\\(cl-\\)*"
                                  "\\(defun\\|defmacro\\|defsubst\\)"
                                  "\\**"
                                  "\\s-+"
                                  "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
  "Regexp to find defun or defmacro definition.")

(defun modi/toggle-edebug()
  (interactive)
  (save-excursion
    (re-search-backward modi/fns-regexp)
    (let ((start (point))
          (fn (match-string 1))
          end
          selection)
      ;; (message "Parsed: %s fns-in-edebug: %s" fn modi/fns-in-edebug)
      (forward-sexp 1)
      (setq end (point))
      (if (member fn modi/fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
            (eval-buffer)
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (save-restriction
          (narrow-to-region start end)
          (add-to-list 'modi/fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn))))))

