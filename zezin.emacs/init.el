(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; emacs configuration
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq load-prefer-newer t)
(setq initial-scratch-message nil)
(setq help-window-select t)
(setq column-number-mode t)
(setq x-alt-keysym 'meta)

(setq frame-title-format "%b")

;; Backup handling
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat user-emacs-directory "autosave/")
      tramp-auto-save-directory  (concat user-emacs-directory "tramp-autosave/")
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave/") t)))

(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Only store encrypted secrets on this file
(setq auth-sources '("~/.authinfo.age"))

(use-package tramp
  :defer
  :straight nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package dash)

;; Evil
(use-package evil
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil))
  :config
  (evil-mode 1))

(use-package expand-region
  :commands er/expand-region)

(defun +copy-path-clipboard ()
  "Copy the current directory into the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun +clipboard-get ()
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun +go-to-path-clipboard ()
  "Go to file in the clipboard"
  (interactive)
  (let ((filename (string-trim (+clipboard-get))))
    (if (file-exists-p filename)
	(ffap filename)
      (message "File %s not exists" filename))))


(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines evilnc-comment-or-uncomment-paragraphs))

(use-package evil-multiedit
  :commands (evil-multiedit-match-symbol-and-next evil-multiedit-match-symbol-and-next)
  :after evil)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(magit dired ivy comint corfu forge))
  (evil-collection-init))

;; ivy/counsel/swiper
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
	ivy-height 20
	ivy-virtual-abbreviate 'full)
  :config
  (ivy-mode 1))

(use-package undo-fu
  :defer 5)

(use-package counsel
  :config
  (cl-defun +region-or-symbol (&optional initial-text)
    (or initial-text
	(if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning) (region-end))
	  (thing-at-point 'symbol))))

  (defun +counsel-find-read-dir ()
    (interactive)
    (let ((dir (file-name-directory (read-file-name "Choose directory: "))))
      (counsel-fzf nil dir)))

  (defun +counsel-rg-directory (dir &optional extra-args text)
    (interactive)
    (let ((text (or text (+region-or-symbol) "")))
      (counsel-rg text dir extra-args)))

  (defun +counsel-rg-read-dir ()
    (interactive)
    (let ((dir (file-name-directory (read-file-name "rg in directory: "))))
      (+counsel-rg-directory dir)))

(defun +counsel-rg-initial-project ()
    (interactive)
    (let* ((pr (project-current))
	   (dir (if pr (project-root pr) default-directory)))
      ;; send empty input when switching to a project
      (+counsel-rg-directory dir "" "")))

  (defun +counsel-rg-project ()
    (interactive)
    (let* ((pr (project-current))
	   (dir (if pr (project-root pr) default-directory)))
      (+counsel-rg-directory dir)))

  (defun +counsel-rg-project-without-test ()
    (interactive)
    (let* ((pr (project-current))
	   (dir (if pr (project-root pr) default-directory))
	   (extra-args "!g test"))
      (+counsel-rg-directory dir extra-args)))

  (defun +counsel-rg-project-with-args ()
    (interactive)
    (let* ((pr (project-current))
	   (dir (if pr (project-root pr) default-directory))
	   (extra-args (read-string "Args for rg: ")))
      (+counsel-rg-directory dir extra-args))))

(use-package amx
  ;; setup done by counsel
  :defer t)

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode))

(use-package avy
  :commands avy-goto-line)

(use-package envrc
  :config
  (envrc-global-mode +1))

(use-package corfu
  :hook (prog-mode . global-corfu-mode)
  :custom
  (corfu-auto t)
  :config
  (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package savehist
  :straight nil
  :init
  (savehist-mode t))

(use-package treesit
  :straight nil
  :init
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	  (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
	  ;; Recent tree-sitter is not compatible with emacs 29. Try it again with Emacs 30
	  (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
	  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (go-template "https://github.com/ngalaiko/tree-sitter-go-template")
	  (java "https://github.com/tree-sitter/tree-sitter-java")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))

  (setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
	(go-mode . go-ts-mode)
	(kotlin-mode . kotlin-ts-mode)
	(python-mode . python-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(ruby-mode . ruby-ts-mode)))
  :config
  (defun +install-all-tree-sitter-languages ()
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(use-package rust-ts-mode
  :straight nil
  :mode (("\\.rs\\'" . rust-ts-mode)))

(use-package kotlin-ts-mode
  :mode (("\\.kt\\'" . kotlin-ts-mode)
	 ("\\.gradle.kts\\'" . kotlin-ts-mode))
  :config
  (reformatter-define
    kotlin-format
    :program "ktlint"
    :args '("-F" "--stdin" "--log-level=error")))

(use-package typescript-ts-mode
  :straight nil
  :mode "\\.ts\\'")

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
   (reformatter-define nix-format
        :program "alejandra"
        :args '("-")))

(use-package dockerfile-mode
  :mode (("\\Dockerfile.*\\'" . dockerfile-mode)))

(use-package go-ts-mode
  :straight nil
  :mode ("\\.go\\'")
  :hook ((go-ts-mode . gofmt-on-save-mode)
	 (go-ts-mode . goimports-on-save-mode))
  :config
  (setq ff-other-file-alist
		   '(("_test\\.go$" (".go"))
		     ("\\.go$" ("_test.go"))))

  (defun +go-test-add-test-hook ()
    (interactive)
    (if (string-match "_test\.go$" buffer-file-name)
	(add-hook 'after-save-hook #'go-test-current-test 0 t)
      (add-hook 'after-save-hook #'+go-test-current-file 0 t)))

  ;; Don't open the test file in the code
  (defun +go-test-current-file ()
    (interactive)
    (save-window-excursion
	(go-test-current-file)))

  (reformatter-define gofmt :program "gofmt")
  (reformatter-define goimports :program "goimports"))

(use-package gotest
  :commands go-test-current-test go-test-current-file go-test-current-test)

(use-package yaml-mode
  :mode ("\\.\\(yaml\\|yml\\|yml.j2\\|yaml.j2\\)\\'"))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-open-command "xdg-open"))

(use-package terraform-mode
  :mode (("\\.tf\\(vars\\)?\\'" . terraform-mode))
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package reformatter
  :defer t)

(use-package scala-mode
  :mode ("\\.scala\\'"))

(use-package groovy-mode
  :mode ("\\.groovy\\'")
  :hook (groovy-mode . jenkinsfile-mode))

(use-package cue-mode
  :commands (cue-mode)
  :mode "\\.cue$"
  :hook ((cue-mode . cuefmt-on-save-mode))
  :config
  (reformatter-define cuefmt :program "cue" :args '("fmt" "-" "-s")))

(use-package jenkinsfile-mode
  :mode (("Jenkinsfile\\'" . jenkinsfile-mode) ("Jenkinsfile.*\\'" . jenkinsfile-mode)))

(defun +split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun +split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right))

;; git
;;
(use-package magit
  :commands magit-file-delete magit-status-mode)

(use-package git-timemachine
  :commands git-timemachine)

(use-package browse-at-remote
  :commands (browse-at-remote)
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
	       '(:host "^gitlab\\.freedesktop\\.org" :type "gitlab")))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
	 (text-mode . git-gutter-mode)))

(use-package git-modes
  :defer t)

(use-package gitignore-mode
  :straight nil
  :mode ("/.dockerignore\\'"))

(use-package forge
  :after magit
  :preface
  (setq forge-add-default-bindings nil))

;; IDE
(use-package dumb-jump
  :commands dumb-jump-go
  :config
  (setq dumb-jump-selector 'ivy
	dumb-jump-aggressive nil))


;;editor
(use-package recentf
  :hook ((prog-mode . recentf-mode)
	 (text-mode . recentf-mode))
  :config
  (setq recentf-max-saved-items 200)
  (run-at-time nil 600 'recentf-save-list))

;; keybindings
(with-eval-after-load 'evil
  (progn
    (evil-set-leader '(visual normal) (kbd "SPC"))

    (evil-define-key 'normal 'global (kbd "<leader>.") 'find-file)
    (evil-define-key 'normal 'global (kbd "<leader>,") 'ivy-switch-buffer)

    (evil-define-key 'normal 'global (kbd "<leader>ww") 'evil-window-next)
    (evil-define-key 'normal 'global (kbd "<leader>wd") 'evil-window-delete)
    (evil-define-key 'normal 'global (kbd "<leader>jo") '+split-window-right-and-focus)
    (evil-define-key 'normal 'global (kbd "<leader>jz") '+split-window-below-and-focus)

    (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
    (evil-define-key 'normal 'global (kbd "<leader>gt") 'git-timemachine)
    (evil-define-key 'normal 'global (kbd "<leader>go") 'browse-at-remote)
    (evil-define-key 'normal 'global (kbd "<leader>gk") 'browse-at-remote-kill)

    (evil-define-key 'normal 'global (kbd "<leader>cd") 'xref-find-definitions)
    (evil-define-key 'normal 'global (kbd "<leader>js") 'evilnc-comment-or-uncomment-lines)
    (evil-define-key 'normal 'global (kbd "<leader>g,") 'dumb-jump-go)

    (evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
    (evil-define-key 'normal 'global (kbd "<leader>pc") 'project-compile)
    (evil-define-key 'normal 'global (kbd "<leader>SPC") 'project-find-file)
    (evil-define-key 'normal 'global (kbd "<leader>*") '+counsel-rg-project)
    (evil-define-key 'normal 'global (kbd "<leader>ja") '+counsel-rg-project-with-args)
    (evil-define-key 'normal 'global (kbd "<leader>si") 'counsel-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>sb") 'counsel-grep-or-swiper)
    (evil-define-key 'normal 'global (kbd "<leader>jb") 'swiper-thing-at-point)
    (evil-define-key 'normal 'global (kbd "<leader>jc") '+counsel-rg-read-dir)
    (evil-define-key 'normal 'global (kbd "<leader>jn") '+counsel-find-read-dir)
    (evil-define-key 'normal 'global (kbd "<leader>jl") 'evil-avy-goto-line)

    (evil-define-key 'normal 'global (kbd "u") 'undo-fu-only-undo)
    (evil-define-key 'normal 'global (kbd "M-_") 'undo-fu-only-redo)

    (evil-define-key 'normal 'global (kbd "gf") 'browse-url)
    (evil-define-key 'normal 'global (kbd "M-o") 'er/expand-region)
    (evil-define-key 'normal 'global (kbd "M-d") 'evil-multiedit-match-symbol-and-next)
    (evil-define-key 'normal 'global (kbd "M-D") 'evil-multiedit-match-symbol-and-prev)
    (evil-define-key 'visual 'global (kbd "M-d") 'evil-multiedit-match-symbol-and-next)
    (evil-define-key 'visual 'global (kbd "M-D") 'evil-multiedit-match-symbol-and-prev)

    (evil-define-key 'normal 'go-ts-mode-map (kbd "<leader>tf") '+go-test-current-file)
    (evil-define-key 'normal 'go-ts-mode-map (kbd "<leader>tc") 'go-test-current-test)
    (evil-define-key 'normal 'go-ts-mode-map (kbd "<leader>tp") 'go-test-current-project)
    (evil-define-key 'normal 'go-ts-mode-map (kbd "<leader>gi") 'eglot-code-action-organize-imports)

    ;; buffer
    (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>bl") 'evil-switch-to-windows-last-buffer)))

(bind-keys ("M-x" . counsel-M-x))

(use-package age
  :ensure t
  :demand t
  :custom
  (age-program "rage")
  (age-default-identity "~/.ssh/age_yubikey")
  (age-default-recipient '("~/.ssh/age_yubikey.pub"))
  :config
  (age-file-enable))

(use-package passage
  :straight (:host github :repo "anticomputer/passage.el")
  :commands (passage))

(use-package project
  :straight nil
  :commands (browse-at-remote)
  :init
  (setq +projects-directory '("~/Projects" "~/.prezi"))

  :custom
  (project-switch-commands
   '((project-dired "Dired" ?d)
     (project-find-file "File" ?f)
     (magit-project-status "Git" ?g)
     (+counsel-rg-initial-project "Search" ?s)
     (+project-browse-at-remote "Remote" ?r)))
  :config

  (defun +project-browse-at-remote ()
    (interactive)
    (with-temp-buffer
      (setq default-directory (project-root (project-current)))
      ;; hack to make browse-at-remote open current directory
      (magit-status-mode)
      (browse-at-remote)))

  (defun +remember-project ()
    (when (project-current)
      (unless
	  (member (project-root (project-current))
		  (project-known-project-roots))
	(project-remember-project (project-current)))))

  ;; Remember project when visiting a file
  (add-hook 'find-file-hook '+remember-project)

  (progn
    (defun +remember-projects-inside (directory)
      "Remember all projects inside directory with depth 2."
      (dolist (file (directory-files directory t))
	(when (and (file-directory-p file)
		   (not (string-prefix-p "." (file-name-nondirectory file))))
	  (dolist (subfile (directory-files file t))
	    (when (and (file-directory-p subfile)
		       (not (string-prefix-p "." (file-name-nondirectory subfile))))
	      (project-remember-projects-under subfile))))))

    (defun +remember-my-projects ()
      (interactive)
      (dolist (project-dir +projects-directory)
	(+remember-projects-inside project-dir))))

  :commands (project-switch-project project-current))

(use-package gptel
  :commands gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "claude-3-5-sonnet-20241022")
  (gptel-display-buffer-action 'switch-to-buffer-other-frame)
  :config
  (setq gptel-backend
	(gptel-make-anthropic
	    "chat"
	  :stream t
	  :key (auth-source-pick-first-password :host "claude.anthropic.com"))))

(use-package tldr
  :commands tldr)

(use-package json-mode
  :commands json-mode-beautify)

(use-package know-your-http-well
  :commands (http-status-code http-header))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :straight nil
  :custom
  (org-startup-truncated nil)
  (org-startup-folded t))

(use-package org-present
  :commands (org-present org-present-quit))

(use-package writeroom-mode
  :defer t)

(use-package yasnippet
  :commands (yas-expand yas-minor-mode)
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode))
  :config
  (let ((custom-snippets-dir (format "%s%s" (expand-file-name user-emacs-directory) "snippets/custom")))
    (add-to-list 'yas-snippet-dirs custom-snippets-dir)))

(use-package yasnippet-snippets
  :after yasnippet)

(defvar +modules-dir (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path +modules-dir)
(require '+purpose)
(require '+presentation)

;;
;; themes

(use-package doom-themes
  :defer t)

(setq +dark-theme 'doom-tomorrow-night)
(setq +light-theme 'doom-ayu-light)

(load-theme +dark-theme t)

(defun zezin-load-light-theme ()
  (interactive)
  (load-theme +light-theme t))

(defun zezin-load-dark-theme ()
  (interactive)
  (load-theme +dark-theme t))

(setq +font "Jetbrains Mono") ;; Source Code Pro is an alternative
(setq +font-size 12)

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(set-frame-font (format "%s %s" +font +font-size))))
  (set-frame-font (format "%s %s" +font +font-size)))

(provide 'init)
;;; init.el ends here
