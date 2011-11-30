(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "C:\Program Files (x86)\emacs-23.3\site-lisp\slime")
(require 'slime)
(slime-setup)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(whitespace-style (quote (face tabs trailing empty))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
