;; Blogging
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  ;; Fix invalid face hl-line error
  (require 'hl-line)
  (set-face-background 'hl-line "gray97")
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-export-with-sub-superscripts nil)
  (setq org-html-htmlize-output-type 'css))

(defun zezin-org-publish-all ()
  (interactive)
  (save-excursion
    (org-publish-all)))

(defun zezin-org-publish-current-file ()
  (interactive)
  (save-excursion
    (org-publish-current-file)))

(provide 'zezin-blogging)
;;; zezin-blogging ends here
