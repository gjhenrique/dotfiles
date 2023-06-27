;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Guilherme Henrique"
      user-mail-address "gjhenrique@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

(setq doom-font (font-spec :family "Source Code Pro" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Life/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil
      x-alt-keysym 'meta)

;; Here are some additional functions/macros that could help you configure Doom:
;;

;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! ivy
  (setq ivy-use-virtual-buffers t))

(defun zezin-clipboard/get ()
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun zezin-go-to-file-in-clipboard ()
  "Go to file in the clipboard"
  (interactive)
  (let ((filename (string-trim (zezin-clipboard/get))))
    (if (file-exists-p filename)
        (ffap filename)
      (message "File %s not exists" filename))))

(defun split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun zezin-find-root-lib (folder lib-identifier)
  "Cut the folder of the last"
  (let ((directories (f-split folder)))
    (and (member lib-identifier directories)
         (apply 'f-join (-take
                         (+ (-elem-index lib-identifier directories) 2)
                         directories)))))

(after! counsel
  (defun counsel-find-read-dir ()
    (interactive)
    (let ((folder (file-name-directory (read-file-name "Choose directory: "))))
      (doom-project-find-file folder)))

  (cl-defun counsel-ag-directory (dir &optional initial-text)
    (interactive)
    (let ((text (or initial-text (doom-thing-at-point-or-region) "")))
          (counsel-ag text dir "--hidden")))

  (defun counsel-ag-region-or-symbol-read-dir ()
    (interactive)
    (let ((folder (file-name-directory (read-file-name "rg in directory: "))))
      (counsel-ag-directory folder))) )

(after! rainbow-delimiters
  (add-hook! 'prog-mode-hook
             #'rainbow-delimiters-mode))

(after! smartparens
  (show-smartparens-global-mode +1))

(after! counsel
  (setq counsel-rg-base-command
        ;; Include hidden files in search
        "rg -M 240 --hidden -g '!.git' --with-filename --no-heading --line-number --color never %s"))

(after! groovy-mode
  (add-to-list 'auto-mode-alist '("Jenkinsfile.*\\'" . groovy-mode)))

(map!
 "M-o" #'er/expand-region
 :v "M-c" #'evil-multiedit-toggle-marker-here
 :leader
 "g," #'dumb-jump-go
 (:prefix-map ("j" . "Personal")
  "b" #'swiper-thing-at-point
  "s" #'evilnc-comment-or-uncomment-lines
  "l" #'evil-avy-goto-line

  "z" #'zezin-go-to-file-in-clipboard
  "o" #'split-window-right-and-focus
  "z" #'split-window-below-and-focus

  "n" #'counsel-find-read-dir
  "c" #'counsel-ag-region-or-symbol-read-dir))

;; Disable q key for compilation-mode because it brings some problems with purpose workflow
(map! :map compilation-mode-map
      :nv "q" #'evil-record-macro)

(defun zezin-load-light-theme ()
  (interactive)
  (load-theme 'doom-solarized-light)
  (doom/reload-theme))

(defun zezin-load-dark-theme ()
  (interactive)
  (load-theme 'doom-gruvbox)
  (doom/reload-theme))

(add-hook! '(js2-mode-hook typescript-mode-hook typescript-tsx-mode-hook)
  (if (locate-dominating-file default-directory ".prettierrc")
      (format-all-mode +1)))

(add-hook! '(go-mode-hook)
  (format-all-mode +1))

(add-hook! '(terraform-mode-hook)
  (format-all-mode +1))

(add-hook! 'ruby-mode-hook
  (setq flycheck-command-wrapper-function
        (lambda (command) command)))

(use-package! meson-mode
  :mode (("/meson\\.build\\'" . meson-mode)))

(use-package! nginx-mode
  :mode ("nginx\\.conf\\'" "/docker-nginx/.*\\.tmpl\\'"))

(use-package! strace-mode
  :mode "\\.strace\\'")

(use-package! know-your-http-well
  :commands (http-status-code http-header))

(use-package! tldr
  :commands tldr
  :init
  (setq tldr-directory-path (concat doom-cache-dir "tldr/")))

(defun zezin-emacs-everywhere ()
  (interactive)
  (let* ((run-path (format "/run/user/%s" (user-real-uid)))
         (sway-file (car (directory-files "/run/user/1000" t "sway-ipc"))))
         (unless (eq (getenv "SWAYSOCK") sway-file)
           (setenv "SWAYSOCK" sway-file)))
  (emacs-everywhere))

(defvar zezin-work-script (expand-file-name "Life/work.el" (substitute-in-file-name "$HOME")))
(when (file-exists-p zezin-work-script)
  (load! zezin-work-script))

(defun zezin-update-frame-font-size (displays)
  (-map (lambda (display)
          (let ((res (nth 0 (alist-get 'workarea display)))
                (zezin-frames (alist-get 'frames display)))
            (if (eq res 0)
                (set-frame-font (font-spec :family "Source Code Pro" :size 28)  nil zezin-frames)
              (set-frame-font (font-spec :family "Source Code Pro" :size 24)  nil zezin-frames))))
        displays))

(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'js2-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)

(eval-after-load 'browse-at-remote
  '(progn
     (add-to-list 'browse-at-remote-remote-type-regexps
                  '(:host "^gitlab\\.freedesktop\\.org" :type "gitlab"))))

(after! emacs-everywhere
  (defun zezin-jira-page-p ()
    (let ((title (emacs-everywhere-app-title emacs-everywhere-current-app)))
      (string-match-p title "JIRA")))

  (defun zezin-convert-from-jira ()
    (when (zezin-jira-page-p)
      (shell-command-on-region (point-min) (point-max)
                               "pandoc -f jira -t org" nil t)))

  (defun zezin-convert-to-jira ()
    (when (zezin-jira-page-p)
      (shell-command-on-region (point-min) (point-max)
                               "pandoc -f jira -t org" nil t)))

  (add-hook 'emacs-everywhere-init-hooks 'zezin-convert-from-jira t)
  (add-hook 'emacs-everywhere-final-hooks 'zezin-convert-to-jira)

  (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
          (width . 120)
          (height . 25))))
