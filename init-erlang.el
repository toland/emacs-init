;;; init-erlang.el --- Configure Erlang mode and Distel

(setq erlang-root-dir "/usr/local/erlware")
(require 'erlang-start)

(setq auto-mode-alist
      (append
       '(("\\.app$" . erlang-mode)
	 ("\\.config$" . erlang-mode))
       auto-mode-alist))

(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    ;(imenu-add-to-menubar "imenu")
	    ;; Bind RET to newline-and-indent and bind C-j to just newline...
	    (define-key erlang-mode-map '[return] 'newline-and-indent)
	    (define-key erlang-mode-map [(control j)] 'newline)))


(provide 'init-erlang)
;; init-erlang.el ends here
