
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

(use-package evil
  :init
  (progn
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil))
  :config
  (evil-mode 1))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines evilnc-comment-or-uncomment-paragraphs))

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

(provide 'init)
;;; init.el ends here
