;;; purpose.el ---                                   -*- lexical-binding: t; -*-

(defun zezin-get-frames ()
  (let* ((initial-frames
	 '(((title . "EmacsPrimary"))
	   ((title . "EmacsNotes") (start-fn . zezin-start-notes-frame))
	   ((title . "EmacsGit") (start-fn . zezin-start-magit-frame))
	   ((title . "EmacsCompilation") (start-fn . zezin-start-compilation-frame)))))
    (when (= (length (display-monitor-attributes-list)) 2)
      (push '((title . "EmacsSecondary")) initial-frames))

    (when (>= (length (display-monitor-attributes-list)) 3)
      (push '((title . "EmacsSecondary")) initial-frames)
      (push '((title . "EmacsTertiary")) initial-frames))

    initial-frames))

(setq zezin-work-file "$HOME/Life/prezi.org")

(defun zezin-add-purposes ()
  (add-to-list 'purpose-user-mode-purposes '(ruby-mode . ruby))
  (add-to-list 'purpose-user-mode-purposes '(rspec-mode . spec))
  (add-to-list 'purpose-user-mode-purposes '(help-mode . help))
  (add-to-list 'purpose-user-mode-purposes '(web-mode . web))
  (add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . elisp))
  (add-to-list 'purpose-user-mode-purposes '(js2-mode . javascript))
  (add-to-list 'purpose-user-mode-purposes '(js-mode . javascript))
  (add-to-list 'purpose-user-mode-purposes '(rjsx-mode . javascript))
  (add-to-list 'purpose-user-mode-purposes '(go-test-mode . search))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*ai.*" . search))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*aider" . search))
  (add-to-list 'purpose-user-regexp-purposes '("^\\magit" . magit)))

(defun zezin-disable-purpose-with-dired ()
  (defalias 'dired-find-file-without-purpose
    (without-purpose-command #'dired-find-file))

  (with-eval-after-load 'dired
    (define-key dired-mode-map [remap dired-find-file] #'dired-find-file-without-purpose)))

(defun zezin-add-reusable-buffers (buffer-regex)
  (add-to-list 'display-buffer-alist
	       `(,buffer-regex
		 nil
		 (reusable-frames . t))))

(defun zezin-dedicate-purpose-window ()
  (let ((title (substring-no-properties
		(cdr (assoc 'title (frame-parameters))))))
    (purpose-set-window-purpose 'magit)))

(defun zezin-frame-title (frame)
  (cdr (assq 'title (frame-parameters frame))))

(defun zezin-frame-exists? (title)
  (member title
	  (-map
	   (lambda (frame) (zezin-frame-title frame))
	   (frame-list))))

(defun zezin-make-new-frame (frame-config)
  (let ((title (assoc 'title frame-config)))
    (when (not (zezin-frame-exists? (cdr title)))
      (make-frame `(,title)))))

(defun zezin-start-frames ()
  (interactive)
  (-each (zezin-get-frames) 'zezin-make-new-frame))

(defun zezin-find-start-fn (frame-title)
  (cdr (assoc 'start-fn
	      (-first
	       (lambda (frame-config)
		 (string= (cdr (assoc 'title frame-config)) frame-title))
	       (zezin-get-frames)))))

(defun zezin-start-notes-frame (frame)
  (find-file (substitute-in-file-name zezin-work-file))
  (purpose-toggle-window-buffer-dedicated))

(defun zezin-start-magit-frame (frame)
  (switch-to-buffer (get-buffer-create "magit: purpose"))
  (purpose-toggle-window-purpose-dedicated))

(defun zezin-start-compilation-frame (frame)
  (switch-to-buffer (get-buffer-create "*compilation: purpose"))
  (compilation-mode)
  (purpose-toggle-window-purpose-dedicated))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (let* ((title (zezin-frame-title frame))
	     (start-fn (zezin-find-start-fn title)))
	     (when start-fn
	      (select-frame frame)
	      (funcall start-fn frame)))))

(use-package window-purpose
  :config
  (progn
    (purpose-mode)
    (purpose-x-kill-setup)

    (add-to-list 'purpose-special-action-sequences
	     '(popup-frame
	       purpose-display-reuse-window-buffer
	       purpose-display-reuse-window-purpose
	       purpose-display-pop-up-frame))

    (zezin-add-reusable-buffers "\\*Go Test\\*")
    (zezin-add-reusable-buffers "\\**compilation\\*")
    (zezin-add-reusable-buffers "\\magit*")
    (zezin-add-reusable-buffers "\\*aider")
    (zezin-add-purposes)
    (zezin-disable-purpose-with-dired)
    (purpose-compile-user-configuration)))

(provide '+purpose)
;;; +purpose.el ends here
