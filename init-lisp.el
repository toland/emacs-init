;;; init-lisp.el --- Some helpful Lisp code

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-j") 'newline)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(eval-after-load 'paredit
  '(progn
     ;; Not sure why paredit behaves this way with comments; it's annoying
     (define-key paredit-mode-map (kbd ";")   'self-insert-command)
     (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
     (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))))

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'coding-hook)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Clojure

(add-hook 'clojure-mode-hook 'coding-hook)
(font-lock-add-keywords 'clojure-mode
                        '(("(\\|)" . 'esk-paren-face)))

;; You might like this, but it's a bit disorienting at first:
;; (setq clojure-enable-paredit t)

;;; Scheme

(add-hook 'scheme-mode-hook 'coding-hook)
(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Common Lisp

(add-hook 'lisp-mode-hook 'coding-hook)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Slime

;; Configure SLIME for OpenMCL
(setq inferior-lisp-program "/usr/local/ccl/scripts/openmcl64")
(setq slime-lisp-implementations
      `((openmcl ("ccl"))
        (sbcl ("sbcl"))))

(require 'slime-autoloads)
(add-hook 'lisp-mode-hook (lambda ()
                            (cond ((not (featurep 'slime))
                                   (require 'slime)
                                   (normal-mode)))))

(eval-after-load "slime"
  '(progn
     ;(setq load-path (append load-path (list "~/.emacs.d/lisp/slime/contrib")))
     ;(slime-setup '(slime-fancy slime-asdf slime-banner))
     (slime-setup)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; Shortcut key for starting a SLIME CL connection
(global-set-key [f5] 'slime)

;; Shortcut key for selecting SLIME buffers
(global-set-key [(control f5)] 'slime-selector)


(provide 'init-lisp)
;; init-lisp.el ends here