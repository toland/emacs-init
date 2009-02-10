;;; init-c.el --- C, C++, Java, etc


;; Xcode
(require 'cc-mode)
(require 'xcode)
(define-key objc-mode-map [(meta r)] 'xcode-compile)
(define-key objc-mode-map [(meta K)] 'xcode-clean)

(add-hook 'c-mode-common-hook
          (lambda()
            (c-set-style "stroustrup")
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (local-set-key (kbd "M-O") 'ff-find-other-file)
            (hs-minor-mode t)))             ; Hide and show blocks

;; Make the compilation window small
(setq compilation-window-height 12)

;; Setup the compilation window to go away on successful compiles
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;; Errors
            (message "Compile failed; press C-x ` to visit errors")
          ;; Else -- make the compilation window go away
          (delete-windows-on buf)
          (message "Compile succeeded"))))
(setq compilation-scroll-output t)

;; Setup command to toggle between .hpp and .cpp
(defun toggle-header-buffer()
  (interactive)
  (let ((ext (file-name-extension buffer-file-name))
        (fname (file-name-sans-extension buffer-file-name)))
    (cond ((string= "h" ext)
           (find-file (concat fname ".cpp")))
          ((string= "cpp" ext)
           (find-file (concat fname ".h"))))))

;; Setup command to toggle between the file we're looking at and the
;; last one we were looking at
(defun toggle-previous-buffer()
    (interactive)
    (iswitch-to-buffer t))

;;; Compilation helper functions

(defun parent-dir(filename)
  (expand-file-name (concat (expand-file-name filename) "/..")))

(defun search-up(filename directory)
    (let ((target-file (expand-file-name filename directory)))
      (cond ((file-exists-p target-file) (expand-file-name directory))
            ((eq directory "/") nil)
            ((search-up filename (parent-dir directory))))))

(defun search-and-run(filename command)
  (let ((old-default-dir default-directory))
    (setq default-directory  (search-up filename default-directory))
    (compile (concat "cd " default-directory " && " command))
    (setq default-directory old-default-dir)))
  
(defun run-make()
  (interactive)
  (search-and-run "Makefile" "make"))

;; Setup shortcuts for compilation and switching between .hpp/.cpp files
(global-set-key "\M-t" 'toggle-buffer)
(global-set-key "\M-r" 'run-make)
(global-set-key "\M-n" 'next-error)


(provide 'init-c)
;;; init-c.el ends here