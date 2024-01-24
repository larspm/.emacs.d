(package-initialize)

(global-set-key [mouse-13]  'highlight-symbol-at-point)
(global-set-key [mouse-15]  'highlight-symbol-remove-all)


(global-set-key (kbd "<f9>")    'kill-star-buffers)
(global-set-key (kbd "<f10>")   'next-sym)
(global-set-key (kbd "<f12>")   'kill-this-buffer)

(global-set-key [mouse-8]     'highlight-symbol-prev)
(global-set-key [mouse-9]     'highlight-symbol-next)

(global-set-key [mouse-3]     'xref-find-tag-window-under-mouse)

(global-set-key [(control mouse-4)]     'highlight-symbol-prev-highlighted)
(global-set-key [(control mouse-5)]     'highlight-symbol-next-highlighted)

(global-set-key [wheel-right] 'swap-buffers-in-windows)

(global-set-key (kbd "C-q") 'fit-window-to-buffer-horizontally)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-c o") 'ace-window)
(global-set-key (kbd "C-c p") 'ggtags-find-tag-aw)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(defun ecwd ()
  (interactive)
  (w32-shell-execute "explore" default-directory))

(defun ggtags-find-tag-aw ()
  (interactive)
  (let ((tag (xref-backend-identifier-at-point (xref-find-backend)))
        (buf (current-buffer)))
    (when tag
      (select-window (aw-select "foo"))
      (set-buffer buf)
      (xref-find-definitions tag))))

(defun ggtags-find-tag-dwim-other-window ()
  (interactive)
  (let ((tag (ggtags-tag-at-point)))
    (message tag)
    (when tag
      (other-window 1)
      (xref-find-definitions tag))))

(defun xref-find-tag-window-under-mouse ()
  (interactive)
  (let ((pos (mouse-position))
        (tag (xref-backend-identifier-at-point (xref-find-backend)))
        (buf (current-buffer)))
    (when (and tag pos)
      (select-frame (car pos))
      (select-window (window-at (cadr pos) (cddr pos)))
      (set-buffer buf)
      (xref-find-definitions tag))))


(defun kill-star-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (if (string-match "\*.*\*" (buffer-name b)) (kill-buffer b))))

(defun swap-buffers-in-windows ()
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun get-longest-line-length ()
  "Get the length of the longest line in the selected window."
  (save-excursion
    (beginning-of-buffer)
    (let ((max-length 0)
          (last-line (count-lines (point-min) (point-max))))
      (while (<= (line-number-at-pos) last-line)
        (setq max-length (max max-length (- (point-at-eol) (point-at-bol))))
        (forward-line))
      (1+ max-length))))

(defun fit-window-to-buffer-horizontally ()
  "Fit the selected window to the width of its longest line.
Return the window width delta."
  (interactive)
  (let* ((current-width (window-width))
         (longest-line (get-longest-line-length))
         (delta (* -1 (- current-width longest-line))))
    (if (zerop (window-resizable (selected-window) delta t)) nil
      (window-resize (selected-window) delta t))
    delta))

(add-hook 'first-change-hook 'whitespace-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(auto-save-default nil)
 '(buffer-file-coding-system 'utf-8-unix t)
 '(buffers-menu-max-size nil)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(cursor-type 'bar)
 '(delete-selection-mode t)
 '(horizontal-scroll-bar-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(autocrypt multiple-cursors highlight-symbol auto-dim-other-buffers ace-window))
 '(pixel-scroll-mode t)
 '(pixel-scroll-precision-interpolate-page t)
 '(pixel-scroll-precision-interpolation-total-time 0.2)
 '(pixel-scroll-precision-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(whitespace-style '(face tabs trailing empty)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package eglot :ensure t)
(add-to-list 'eglot-server-programs
             '(vhdl-mode . ("vhdl_ls")))
