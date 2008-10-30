;;; name:           pa_monad-custom-tuareg.el
;;; synopsis:       customization for tuareg mode, which makes "perform" a keyword
;;; author:         Lydia E. van Dijk
;;; last revision:  Thu Oct 30 08:52:54 UTC 2008
;;; Emacs version:  21.1.1
;;; Tuareg version: 1.45.6

(add-hook 'tuareg-mode-hook
          '(lambda ()
             ;; font locking
             (setq tuareg-font-lock-keywords
                   (cons (list "\\<perform\\>"
                               0 'tuareg-font-lock-governing-face nil nil)
                         tuareg-font-lock-keywords))
             ;; indentation
             (setq tuareg-keyword-regexp
                   (concat tuareg-keyword-regexp "\\|\\<perform\\>"))
             (setq tuareg-matching-keyword-regexp
                   (concat tuareg-matching-keyword-regexp "\\|\\<perform\\>"))
             (setq tuareg-keyword-alist
                   (cons '("perform" . tuareg-default-indent)
                         tuareg-keyword-alist))))
