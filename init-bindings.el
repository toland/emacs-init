;;; init-bindings.el --- Set up some handy key bindings

;; Find unbound keys
(require 'unbound)

;; Key bindings and more from TextMate
(require 'textmate)
(textmate-mode t)

;; Mo betta scrolling
(require 'pager)
(global-set-key [remap scroll-up] 'pager-page-down)
(global-set-key [remap cua-scroll-up] 'pager-page-down)
(global-set-key [remap scroll-up-mark] 'pager-page-down-extend-region)
(global-set-key [next] 'pager-page-down)
(global-set-key [\S-next] 'pager-page-down-extend-region)
(global-set-key [\M-up] 'pager-page-up)
(global-set-key [remap scroll-down] 'pager-page-up)
(global-set-key [remap cua-scroll-down] 'pager-page-up)
(global-set-key [remap scroll-down-mark] 'pager-page-up-extend-region)
(global-set-key [prior] 'pager-page-up)
(global-set-key [\S-prior] 'pager-page-up-extend-region)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x i") 'ido-goto-symbol)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(autoload 'findr "findr" "Find file name." t)
(define-key global-map (kbd "C-c S") 'findr)

(autoload 'findr-search "findr" "Find text in files." t)
(define-key global-map (kbd "C-c s") 'findr-search)

(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map (kbd "C-c R") 'findr-query-replace)

;; Window switching. (C-x o goes to the next window)
;(windmove-default-keybindings) ;; Shift+direction
(global-set-key "\C-xO" (lambda () (interactive) (other-window -1))) ;; back one
;(global-set-key "\C-x\C-o" (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Duplicate the current line
(global-set-key (kbd "C-c d") 'duplicate-current-line)

;; Make the delete key behave
(global-set-key (kbd "<kp-delete>") 'delete-char)

;; What it says on the box - toggle full screen mode
(global-set-key (kbd "M-n") 'toggle-fullscreen)

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'tabbar-local-mode)

;(global-set-key [(meta shift right)] 'ido-switch-buffer)
;(global-set-key [(meta shift up)] 'recentf-ido-find-file)
;(global-set-key [(meta shift down)] 'ido-find-file)
;(global-set-key [(meta shift left)] 'magit-status)

;(global-set-key [(meta H)] 'delete-other-windows)

(global-set-key [(meta D)] 'backward-kill-word) ;; (meta d) is opposite

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'init-bindings)
;;; init-bindings.el ends here