
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


(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines evilnc-comment-or-uncomment-paragraphs))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(magit dired ivy comint))
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


(use-package envrc
  :config
  (envrc-global-mode +1))

;; langs
(push '(python-mode . python-ts-mode) major-mode-remap-alist)

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

(use-package git-timemachine
  :commands browse-at-remote)

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode)))

;; IDE
(use-package dumb-jump
  :commands dumb-jump-go
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-aggressive nil))

;; TODO: Put this inside counsel


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

;; (find-file "/home/guilherme/Projects/python/Flexget/flexget/api/core/authentication.py")
(find-file "/home/guilherme/Projects/mine/dotfiles/files/.zezin.emacs/init.el")

(provide 'init)
;;; init.el ends here

