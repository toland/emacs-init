;;; init.el --- SML and Haskell modes

;;; Sml

(load "sml-mode-startup.el")
(setq sml-program-name "/opt/local/bin/sml")

(add-hook 'sml-mode-hook
	  (lambda ()
	    ;; Bind RET to newline-and-indent and bind C-j to just newline...
	    (define-key sml-mode-map '[return] 'newline-and-indent)
	    (define-key sml-mode-map [(control j)] 'newline)))


;;; Haskell

(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)

(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hic?$"   . haskell-mode)
                ("\\.hsc$"    . haskell-mode)
                ("\\.chs$"    . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))

(require 'inf-haskell)

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
;(add-hook 'haskell-mode-hook 'turn-off-haskell-decl-scan)
;(add-hook 'haskell-mode-hook 'turn-off-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;(add-hook 'haskell-mode-hook
;   (function
;    (lambda ()
;      (setq haskell-program-name "ghci")
;      (setq haskell-ghci-program-name "ghci6")
;      (setq haskell-ghci-program-args
;         '("-fcontext-stack=30"
;           "-fglasgow-exts"
;           "-farrows")))))



(provide 'init-ml)
;;; init-ml.el ends here
