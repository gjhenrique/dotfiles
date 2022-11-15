;; Blogging

(defvar zezin-blog-path (substitute-in-file-name "$HOME/Projects/mine/blog/"))

(with-eval-after-load 'org
  (load-file (concat zezin-blog-path "posts-config.el"))

  ;; Hugo directories
  (zezin-set-posts-info (concat zezin-blog-path "org")
                        (concat zezin-blog-path "content/posts")
                        (concat zezin-blog-path "static"))

  (setq org-html-htmlize-output-type 'css)

  (defun zezin-rewrite-link (orig-fun &rest args )
    "Replaces org-html-link images with absolue URLs"
    (let ((res (apply orig-fun args)))
      (if (and (stringp res) (string-match-p "<img src=" res))
          (replace-regexp-in-string "./res" "/res" res)
        res)))
  (advice-add #'org-html-link :around #'zezin-rewrite-link))
