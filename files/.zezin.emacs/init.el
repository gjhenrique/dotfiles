
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

;; Evil
(straight-use-package 'evil)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-collection)

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
  (setq evil-collection-mode-list '(magit dired))
  (evil-collection-init))

;; ivy/counsel/swiper
(straight-use-package 'ivy)
(straight-use-package 'ivy-rich)
(straight-use-package 'counsel)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy-height 20)
  :config
  (ivy-mode 1))

(use-package counsel)

(use-package ivy-rich
  ;; :defer 10
  :after (ivy counsel)
  :config
  (ivy-rich-mode))


(straight-use-package 'envrc)
(use-package envrc
  :config
  (envrc-global-mode +1))

;; python
(push '(python-mode . python-ts-mode) major-mode-remap-alist)

(with-eval-after-load 'eglot
  (push '(python-ts-mode "pylsp" "--verbose" "--log-file=/tmp/server.log") eglot-server-programs))

(defun zezin-split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun zezin-split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right))

;; git
;;
(straight-use-package 'magit)
(straight-use-package 'git-timemachine)
(straight-use-package 'browse-at-remote)
(straight-use-package 'git-gutter)

(use-package magit
  :commands magit-file-delete)

(use-package git-timemachine
  :commands git-timemachine)

(use-package git-timemachine
  :commands browse-at-remote)

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode)))

;; code editing
(straight-use-package 'dumb-jump)
(use-package dumb-jump
  :commands dumb-jump-go
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-aggressive nil))

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

(defun +counsel-rg-region-or-symbol-read-dir ()
  (interactive)
  (let ((dir (file-name-directory (read-file-name "rg in directory: "))))
    (+counsel-rg-directory dir)))

(defun +counsel-rg-project ()
  (interactive)
  (let ((dir (or (cdr (cdr (project-current))) default-directory)))
    (message dir)
    (+counsel-rg-directory dir)))

;; keybindings
(with-eval-after-load 'evil
  (progn
    (evil-set-leader 'normal (kbd "SPC"))

    (evil-define-key 'normal 'global (kbd "<leader>.") 'find-file)

    (evil-define-key 'normal 'global (kbd "<leader>ww") 'evil-window-next)
    (evil-define-key 'normal 'global (kbd "<leader>wd") 'evil-window-delete)
    (evil-define-key 'normal 'global (kbd "<leader>jo") 'zezin-split-window-below-and-focus)
    (evil-define-key 'normal 'global (kbd "<leader>js") 'zezin-split-window-right-and-focus)

    (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
    (evil-define-key 'normal 'global (kbd "<leader>gt") 'git-timemachine)
    (evil-define-key 'normal 'global (kbd "<leader>go") 'browse-at-remote)
    (evil-define-key 'normal 'global (kbd "<leader>gk") 'browse-at-remote-kill)

    (evil-define-key 'normal 'global (kbd "<leader>cd") 'xref-find-definitions)
    (evil-define-key 'normal 'global (kbd "<leader>js") 'evilnc-comment-or-uncomment-lines)
    (evil-define-key 'normal 'global (kbd "<leader>g,") 'dumb-jump-go)

    (evil-define-key 'normal 'global (kbd "<leader>*") '+counsel-rg-project)
    ))

;; magit
;;

(find-file "/home/guilherme/Projects/python/Flexget/flexget/api/core/authentication.py")
(provide 'init)
;;; init.el ends here
