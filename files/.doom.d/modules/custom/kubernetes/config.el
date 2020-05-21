;;; custom/kubernetes/config.el -*- lexical-binding: t; -*-

(use-package! kubernetes
    :defer t
    :commands (kubernetes-overview)
    :init
    (map! :leader
     "o k" #'kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes-overview)
