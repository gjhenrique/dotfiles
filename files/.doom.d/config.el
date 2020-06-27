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
(setq doom-font (font-spec :family "Source Code Pro" :size 23))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

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

(defvar zezin-lib-directories '("gems" "elpa" "node_modules" "repos"))

(defun zezin-find-lib-folder (folder)
  (cl-some
   (lambda (lib) (zezin-find-root-lib folder lib))
   zezin-lib-directories))

(after! counsel
  (defun counsel-find-read-dir ()
    (interactive)
    (let ((folder (file-name-directory (read-file-name "Choose directory: "))))
      (doom-project-find-file folder)))

  (cl-defun counsel-rg-directory (dir &optional initial-text)
    (interactive)
    (let ((text (or initial-text (doom-thing-at-point-or-region) "")))
          (counsel-rg text dir "--hidden")))

  (defun counsel-rg-use-package ()
    (interactive)
    (counsel-rg-directory doom-emacs-dir "(use-package "))

  (defun counsel-rg-read-lib ()
    (interactive)
    (let ((folder (or (zezin-find-lib-folder default-directory) projectile-project-root)))
      (counsel-rg-directory folder)))

  (defun counsel-rg-read-gem (gem-name)
    (interactive (list (completing-read "Bundled gem: " (bundle-list-gems-cached))))
    (let ((gem-location (bundle-gem-location gem-name)))
      (counsel-rg-directory gem-location)))

  (defun counsel-rg-region-or-symbol-read-dir ()
    (interactive)
    (let ((folder (file-name-directory (read-file-name "rg in directory: "))))
      (counsel-rg-directory folder))))

(after! rainbow-delimiters
  (add-hook! 'prog-mode-hook
             #'rainbow-delimiters-mode))

(after! smartparens
  (show-smartparens-global-mode +1))

(after! counsel
  (setq counsel-rg-base-command
        ;; Include hidden files in search
        "rg -M 240 --hidden -g '!.git' --with-filename --no-heading --line-number --color never %s"))

(map!
 "M-o" #'er/expand-region
 :v "M-c" #'evil-multiedit-toggle-marker-here
 :leader
 "g," #'dumb-jump-go
 (:prefix-map ("j" . "Personal")
  ;; "f" #'counsel-find-file ;; SPC f f - counsel-find-file
  ;; "d" #'counsel-projectile-find-file ;; SPC p f - +ivy/projectile-find-file
  ;; "k" #'kill-this-buffer ;; SPC b d - kill-current-buffer
  ;; "p" #'counsel-projectile-switch-project ;; SPC p p - counsel-projectile-switch-project
  ;; "g" #'counsel-projectile-switch-to-buffer ;; retire. SPC j j does the trick
  ;; "a" #'projectile-compile-project ;; SPC p c - projectile-compile-project
  ;; "m" #'mode-line-other-buffer ;; SPC b l - switch-to-last-buffer
  ;; "j" #'ivy-switch-buffer ;; SPC b b|SPC , ivy-switch-buffer
  ;; "r" #'counsel-projectile-rg ;; SPC s p +default/search-project
  ;; "c" #'counsel-fzf ;; SPC f F +default/find-file-under-here
  ;; "u" #'browse-url-at-point ;; gf +lookup/file
  ;; "e" #'counsel-rg-region-or-symbol-projectile ;; SPC * +default/search-project-for-symbol-at-point
  ;; "h" #'evil-window-delete ;; C-w d

  "b" #'swiper-thing-at-point
  "s" #'evilnc-comment-or-uncomment-lines
  "l" #'evil-avy-goto-line

  "z" #'zezin-go-to-file-in-clipboard
  "o" #'split-window-right-and-focus
  "z" #'split-window-below-and-focus

  "n" #'counsel-find-read-dir
  "c" #'counsel-rg-region-or-symbol-read-dir
  "x" #'counsel-rg-read-lib
  "v" #'google-translate-smooth-translate))

;; Disable q key for compilation-mode because it brings some problems with purpose workflow
(map! :map compilation-mode-map
      :nv "q" #'evil-record-macro)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun zezin-load-light-theme ()
  (interactive)
  (load-theme 'doom-solarized-light)
  (doom/reload-theme))

(defun zezin-load-dark-theme ()
  (interactive)
  (load-theme 'doom-city-lights)
  (doom/reload-theme))

(add-hook! '(js2-mode-hook typescript-mode-hook)
  (if (locate-dominating-file default-directory ".prettierrc")
      (format-all-mode +1)))

(add-hook! 'ruby-mode-hook
  (setq-local flycheck-command-wrapper-function
        (lambda (command) (append '("bundle" "exec") command))))

(use-package! tldr
  :commands tldr
  :init
  (setq tldr-directory-path (concat doom-cache-dir "tldr/")))

(use-package! google-translate
  :commands google-translate-smooth-translate
  :init
  (setq google-translate-translation-directions-alist '(("de" . "en") ("en" . "pt") ("pt" . "en") ("en" . "de"))
        google-translate-show-phonetic t
        google-translate-pop-up-buffer-set-focus t)
  :config
  (require 'google-translate-smooth-ui))

(defvar zezin-work-script (expand-file-name "Life/work.el" (substitute-in-file-name "$HOME")))
(when (file-exists-p zezin-work-script)
  (load! zezin-work-script))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181E24" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(custom-safe-themes
   (quote
    ("0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" default)))
 '(fci-rule-color "#56697A")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#41505E"))
 '(objed-cursor-color "#D95468")
 '(pdf-view-midnight-colors (cons "#A0B3C5" "#1D252C"))
 '(rustic-ansi-faces
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8BD49C")
    (cons 40 "#abcd93")
    (cons 60 "#cbc68b")
    (cons 80 "#EBBF83")
    (cons 100 "#e5ae6f")
    (cons 120 "#df9e5b")
    (cons 140 "#D98E48")
    (cons 160 "#dc885f")
    (cons 180 "#df8376")
    (cons 200 "#E27E8D")
    (cons 220 "#df7080")
    (cons 240 "#dc6274")
    (cons 260 "#D95468")
    (cons 280 "#b35365")
    (cons 300 "#8d5163")
    (cons 320 "#675160")
    (cons 340 "#56697A")
    (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
