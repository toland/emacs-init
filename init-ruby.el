;;; init-ruby.el --- Some helpful Ruby code

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-compilation)
     (setq ruby-use-encoding-map nil)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (inf-ruby-keys)
                 (ruby-electric-mode t)))

     ;(set (make-local-variable 'indent-tabs-mode) 'nil)
     ;(set (make-local-variable 'tab-width) 2)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-j") 'newline)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")
     (define-key ruby-mode-map (kbd "C-c v") 'ruby-eval-buffer)))

(global-set-key (kbd "C-h r") 'ri)

;; Rake files are ruby, too, as are gemspecs.
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.sake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;;; Rake

(defun pcomplete/rake ()
  "Completion rules for the `rake' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
			(if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
		     (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))


;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(add-hook 'ruby-mode-hook 'coding-hook)

;;; Flymake

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

;;; ri

(setq ri-ruby-script (expand-file-name "~/.emacs.d/lisp/ri-emacs/ri-emacs.rb"))
(autoload 'ri (expand-file-name "~/.emacs.d/lisp/ri-emacs/ri-ruby.el") nil t)

(add-hook 'ruby-mode-hook
          (lambda ()
             (local-set-key (kbd "f1") 'ri)
             (local-set-key (kbd "f4") 'ri-ruby-show-args)
             (local-set-key (kbd "M-C-i") 'ri-ruby-complete-symbol)))

;; Rinari
;; (require 'rinari)

;; HAML and SASS
(autoload 'haml-mode "haml-mode.el"
  "Major mode for editing HAML files" t)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
;(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
;(define-key haml-mode-map [(control meta left)] 'haml-up-list)
;(define-key haml-mode-map [(control meta right)] 'haml-down-list)

(autoload 'sass-mode "sass-mode.el"
  "Major mode for editing SASS files" t)

(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(eval-after-load 'haml-mode
  (if (functionp 'whitespace-mode)
      (add-hook 'haml-mode-hook 'whitespace-mode)))


(provide 'init-ruby)
;; init-ruby.el ends here
