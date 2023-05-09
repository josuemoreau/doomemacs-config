;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Josué Moreau"
      user-mail-address "jsmr14.12@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                         GENERAL EDITOR CONFIGURATION                         ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'normal))
(defun config-font ()
  "Increase spacing between lines."
  (setq line-spacing 1))
(add-hook 'prog-mode-hook 'config-font)
(add-hook 'coq-mode-hook 'config-font)

;; Doom double buffering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq shell-file-name "/bin/sh")

;; tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; rule at 80 characters
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#2A4D44")
;; (add-hook 'prog-mode-hook 'fci-mode)

(setq comment-style 'multi-line)
(setq comment-multi-line t)
(setq comment-continue "   ")

;; Enable the www ligature in every possible major mode
;; (ligature-set-ligatures 't '("www"))

(defvar ligature-list '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                        ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                        "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                        "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                        "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                        "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                        "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                        "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                        "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                        "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"
                        "/\\" "\\/" "//=" "|-"))

;; Enable ligatures in programming modes and Coq modes
(ligature-set-ligatures 'prog-mode ligature-list)
(ligature-set-ligatures 'coq-mode ligature-list)

(global-ligature-mode 't)

(setq vc-follow-symlinks nil)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(setq doom-theme 'doom-gruvbox)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                            BUFFERS CONFIGURATION                             ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(defcustom buffer-skip-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
                  "*Messages*" "*package*" "*Warnings*"
                  "*scratch*"
                  "*Async-native-compile-log*"
                  "*Native-compile-Log*"
                  "*doom*" "*coq*" "*goals*" "*response*")
              (seq (zero-or-more anything) "lsp" (zero-or-more anything))
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything))
              (seq "magit-stash" (zero-or-more anything))
              (seq "*" (zero-or-more anything) "*"))
              eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)

(defun buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `buffer-skip-regexp'."
  (string-match-p buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'buffer-skip-p)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                                KEY BINDINGS                                  ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

;; Not useful if merlin is replaced with OcamlLSP
;; (defun my/next-error ()
;;   (interactive)
;;   (if (eq major-mode 'tuareg-mode)
;;       (merlin-error-next)
;;     (next-error)))

(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or ARG)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or ARG)."
  (interactive "p")
  (scroll-down (or arg 1)))

(global-set-key [f5]   'compile)
(global-set-key [f6]   'recompile)
(global-set-key [f7]   'next-error)
(global-set-key (kbd "C-x <f7>") 'flycheck-copy-errors-as-kill)
;; (global-set-key [f7] (lambda ()
;;                        (interactive)
;;                        (progn
;;                          (save-buffer)
;;                          (recompile))))
(global-set-key "\es" 'scroll-one-line-up)
(global-set-key "\ez" 'scroll-one-line-down)

;; Trailing whitespaces (show and delete key)
(require 'whitespace)
(setq-default whitespace-style '(face trailing))
(setq-default whitespace-line-column 80)
(global-whitespace-mode 1)
(global-set-key [f8] 'delete-trailing-whitespace)

;; Deadgrep
(global-set-key (kbd "<f9>") #'deadgrep)
(global-set-key (kbd "<f10>") #'deadgrep-kill-all-buffers)

;; Switch Window
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

(global-set-key (kbd "C-x C-;") 'comment-line)

;; Disable 'insert' key
(define-key global-map [(insert)] nil)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                      CUSTOM MODULES FOR SOME LANGUAGES                       ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

;; load why3 package
(load! "why3")

;; load mix package
(load! "mix.el")

(load! "b.el")

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                              AUTO-COMPLETION                                 ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

;; (require 'company)
(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (setq company-idle-delay 0))

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                             OCAML CONFIGURATION                              ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    ;; (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    ;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
    ;; (add-hook 'caml-mode-hook 'merlin-mode t)
    (add-hook 'tuareg-mode-hook #'lsp)
    (add-hook 'caml-mode-hook #'lsp)
    ;; (require 'merlin-company)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(setq lsp-ui-sideline-enable t)

(add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))

;; (global-set-key (kbd "<C-right>") 'auto-complete)

;; (require 'auto-complete-config)
;; (ac-config-default)

;; (global-set-key (kbd "\t") 'ac-expand)
;; (global-set-key [tab] 'ac-complete)
;; (global-set-key "\r" 'ac-complete)

;; (define-key ac-mode-map (kbd "TAB") nil)
;; (define-key ac-completing-map (kbd "TAB") nil)
;; (define-key ac-completing-map [tab] nil)

(defvar
  tuareg-font-lock-constructor-face
  'font-lock-constant-face)

;; Disable ocp-indent for *.mll and *.mly files
(defun disable-ocp-indent-mll ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.mll\\'" buffer-file-name))
    (kill-local-variable 'indent-line-function)
    (kill-local-variable 'indent-region-function)
    (setq indent-tabs-mode nil)
    (setq-default tab-width 2)
    (setq indent-line-function 'insert-tab)
    (setq merlin-mode nil)
    )
  )

(defun disable-ocp-indent-mly ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.mly\\'" buffer-file-name))
    (kill-local-variable 'indent-line-function)
    (kill-local-variable 'indent-region-function)
    (setq indent-tabs-mode nil)
    (setq-default tab-width 2)
    (setq indent-line-function 'insert-tab)
    (setq merlin-mode nil)
    )
  )

(add-hook 'tuareg-mode-hook 'disable-ocp-indent-mly t)
(add-hook 'tuareg-mode-hook 'disable-ocp-indent-mll t)
(add-hook 'tuareg-mode-hook
  (lambda ()
    (progn
      (define-key tuareg-mode-map [tab] 'smie-indent-line)
      (define-key tuareg-mode-map (kbd "TAB") 'smie-indent-line)
    )))

(setq tuareg-indent-align-with-first-arg t)

(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(opam-env)

(setq lsp-ocaml-lsp-server-command '("ocamllsp" "--fallback-read-dot-merlin"))

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                              COQ CONFIGURATION                               ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(setq coq-prog-name "/home/josue/.opam/ocaml/bin/coqtop")
;; disables Proof General splash screen at startup
(setq proof-splash-enable nil)
(setq proof-three-window-mode-policy 'hybrid)

(defun coq-prettify-symbols ()
  "Beautify some Coq symbols."
  (interactive)
  (setq prettify-symbols-alist
        '(("forall" . "∀")
          ("exists" . "∃")))
  (prettify-symbols-mode))

(add-hook 'coq-mode-hook 'coq-prettify-symbols)

(defun coq-keymaps ()
  "Set local coq-mode keybindings."
  (define-key coq-mode-map [tab] 'smie-indent-line)
  (define-key coq-mode-map (kbd "TAB") 'smie-indent-line)
  (local-set-key (kbd "C-M-c") #'coq-Check)
  (local-set-key (kbd "C-M-p") #'coq-Print))

;; Add some monadic notations
(add-hook 'coq-mode-hook
  (lambda ()
    (dolist (token '(("doo" . "let monadic")
                     ("dob" . "let monadic")
                     ("do*" . "let monadic")
                     ("doo*" . "let monadic")))
      (add-to-list 'coq-smie-monadic-tokens token))))

(add-hook 'coq-mode-hook #'coq-keymaps)

(add-hook 'coq-mode-hook
          (lambda ()
            (progn
              (setq abbrev-expand-function #'ignore)
              ;; redefine some coq faces according to the theme
              (face-remap-add-relative 'coq-solve-tactics-face '(:inherit font-lock-keyword-face))
              (face-remap-add-relative 'proof-tactics-name-face '(:inherit font-lock-constant-face))
              (face-remap-add-relative 'proof-tacticals-name-face '(:inherit font-lock-variable-name-face))
              ;; disable yasnippet on 'TAB' key
              (define-key yas-minor-mode-map [(tab)] nil)
              (define-key yas-minor-mode-map (kbd "TAB") nil)
              )))

;; Fix for slow startup in doom emacs
;; https://github.com/doomemacs/doomemacs/issues/5823
(after! doom-editor
  (add-to-list 'doom-detect-indentation-excluded-modes 'coq-mode))

(eval-after-load "proof-script"
  '(progn
     (setq electric-indent-mode nil)
     ))

;; Fix for auto-completion not starting in doom emacs
;;   There is a problem between company-coq, proof general and doom emacs
;;   which prevents coq-company's auto-completion from showing up.
;;   One simple fix is to disable coq-company-mode then re-enable it.
;;   A mort convenient fix is described here :
;; https://github.com/doomemacs/doomemacs/issues/2868
;; https://github.com/doomemacs/doomemacs/pull/2857
;; `+company-init-backends-h' in `after-change-major-mode-hook' overrides
;; `company-backends' set by `company-coq' package. This dirty hack fixes
;; completion in coq-mode. TODO: remove when company backends builder is
;; reworked.
(defvar-local +coq--company-backends nil)
(after! company-coq
  (defun +coq--record-company-backends-h ()
    (setq +coq--company-backends company-backends))
  (defun +coq--replay-company-backends-h ()
    (setq company-backends +coq--company-backends))
  (add-hook! 'company-coq-mode-hook
    (defun +coq--fix-company-coq-hack-h ()
      (add-hook! 'after-change-major-mode-hook :local #'+coq--record-company-backends-h)
      (add-hook! 'after-change-major-mode-hook :append :local #'+coq--replay-company-backends-h))))

;; disable company-coq symbol prettification
(with-eval-after-load 'company-coq
  (dolist (feature '(prettify-symbols snippets))
    (add-to-list 'company-coq-disabled-features feature)))
;; enable company-coq in coq-mode
(add-hook 'coq-mode-hook #'company-coq-mode)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                            PYTHON CONFIGURATION                              ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

;; (add-hook 'python-mode-hook 'pyvenv-mode-hook)
(defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'my/python-mode-hook)
(setq jedi:complete-on-dot t)

;; (add-hook 'python-mode-hook 'jedi:setup)
;;   (push 'company-jedi company-backends)
;;   :config
;;   (setq jedi:complete-on-dot t))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                              LATEX CONFIGURATION                             ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(defun latex-init ()
  (interactive nil)
  (setq font-latex-fontify-sectioning 1.3) ;; headers with larger font
  (font-latex-update-sectioning-faces)
  (setq compile-command "rubber --pdf main.tex"))

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'latex-init)

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                             ORG MODE CONFIGURATION                           ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

(setq org-support-shift-select t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ocaml . t)))

;; ---------------------------------------------------------------------------- ;;
;;                                                                              ;;
;;                                  MISCELLANEOUS                               ;;
;;                                                                              ;;
;; ---------------------------------------------------------------------------- ;;

;; (require 'iso-transl)

;; (setq auto-window-vscroll nil)

;; (add-to-list 'auto-mode-alist '("\\.smt\\'" . z3-mode))
;; (add-to-list 'auto-mode-alist '("\\.smt2\\'" . Z3-mode))
