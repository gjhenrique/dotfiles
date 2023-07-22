
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

(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Evil
(use-package evil
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil))
  :config
  (evil-mode 1))

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

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(magit dired ivy comint corfu))
  (evil-collection-init))

;; ivy/counsel/swiper
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy-height 20)
  :config
  (ivy-mode 1))

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

  (defun +counsel-rg-directory (dir &optional initial-text)
    (interactive)
    (let ((text (or initial-text (+region-or-symbol) "")))
      (counsel-rg text dir)))

  (defun +counsel-rg-read-dir ()
    (interactive)
    (let ((dir (file-name-directory (read-file-name "rg in directory: "))))
      (+counsel-rg-directory dir)))

  (defun +counsel-rg-project ()
    (interactive)
    (let* ((pr (project-current))
           (dir (if pr (project-root pr) default-directory)))
      (message dir)
      (+counsel-rg-directory dir))))

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
  :init
  (savehist-mode t))

(use-package treesit
  :straight nil
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")))

  (setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (go-mode . go-ts-mode)
        (python-mode . python-ts-mode)))
  :config
  (defun +install-all-tree-sitter-languages ()
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))


(use-package go-ts-mode
  :straight nil
  :mode ("\\.go\\'")
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (reformatter-define terraform-format
                      :program "terraform" :args '("fmt" "-")))

(use-package yaml-mode
  :mode ("\\.\\(yaml\\|yml\\)\\'"))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-open-command "xdg-open"))

(use-package terraform-mode
  :mode (("\\.tf\\(vars\\)?\\'" . terraform-mode))
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (reformatter-define terraform-format
                      :program "terraform" :args '("fmt" "-")))

(use-package reformatter
  :defer t)

(use-package groovy-mode
  :mode ("\\.groovy\\'")
  :hook (groovy-mode . jenkinsfile-mode))

(use-package jenkinsfile-mode
  :mode ("Jenkinsfile*\\'" . jenkinsfile-mode))

;; install pyls
;; install yaml-language-server

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
  :commands magit-file-delete)

(use-package git-timemachine
  :commands git-timemachine)

(use-package browse-at-remote
  :commands browse-at-remote
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^gitlab\\.freedesktop\\.org" :type "gitlab")))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode)))

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
    (evil-define-key 'normal 'global (kbd "<leader>SPC") 'project-find-file)
    (evil-define-key 'normal 'global (kbd "<leader>*") '+counsel-rg-project)
    (evil-define-key 'normal 'global (kbd "<leader>si") 'counsel-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>sb") 'counsel-grep-or-swiper)
    (evil-define-key 'normal 'global (kbd "<leader>jb") 'swiper-thing-at-point)
    (evil-define-key 'normal 'global (kbd "<leader>jc") '+counsel-rg-read-dir)
    (evil-define-key 'normal 'global (kbd "<leader>jn") '+counsel-find-read-dir)
    (evil-define-key 'normal 'global (kbd "<leader>jl") 'evil-avy-goto-line)

    (evil-define-key 'normal 'global (kbd "gf") 'browse-url)

    ;; buffer
    (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-current-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>bl") 'evil-switch-to-windows-last-buffer)))

(use-package project
  :straight nil
  :init
  (setq +projects-directory '("~/Projects"))
  :config
  (message "Config")

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

(use-package tldr
  :commands tldr)

(use-package know-your-http-well
  :commands (http-status-code http-header))


(defvar +modules-dir (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path +modules-dir)
(require '+purpose)

;;
;; themes

(use-package doom-themes
  :defer t)

(setq +dark-theme 'doom-gruvbox)
(setq +light-theme 'doom-solarized-light)

(load-theme +dark-theme)

(defun zezin-load-light-theme ()
  (interactive)
  (load-theme +light-theme))

(defun zezin-load-dark-theme ()
  (interactive)
  (load-theme +dark-theme))

;; (find-file "/home/guilherme/Projects/golang/delve/_fixtures/teststepprog.go")
;; (find-file "/home/guilherme/Projects/python/Flexget/flexget/api/core/authentication.py")
;; (find-file "/home/guilherme/Projects/mine/dotfiles/files/.zezin.emacs/init.el")
;; (find-file "/home/guilherme/Projects/jenkins/helm-charts/charts/jenkins/templates/jenkins-controller-statefulset.yaml")
;; (find-file "/home/guilherme/Projects/terraform/eks-workshop-v2/terraform/main.tf")
;; (find-file "/home/guilherme/Projects/terraform/eks-workshop-v2/README.md")
;; (find-file "/home/guilherme/Projects/jenkins/pipeline-library/vars/buildPlugin.groovy")

(provide 'init)
;;; init.el ends here

