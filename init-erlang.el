;;; init-erlang.el --- Configure Erlang mode and Distel

;;; Erlang mode

(setq erlang-root-dir "/usr/local/erlware")
(require 'erlang-start)

(setq auto-mode-alist
      (append
       '(("\\.app$" . erlang-mode)
         ("\\.config$" . erlang-mode))
       auto-mode-alist))

(add-hook 'erlang-mode-hook
          (lambda ()
            (coding-hook)
            ;; Bind RET to newline-and-indent and bind C-j to just newline...
            (define-key erlang-mode-map '[return] 'newline-and-indent)
            (define-key erlang-mode-map [(control j)] 'newline)))

;;; Distel

(require 'distel)
(distel-setup)

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)

;; default node name to emacs@localhost
(setq inferior-erlang-machine-options '("-sname" "emacs"))

;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(provide 'init-erlang)
;; init-erlang.el ends here
