;; mix.el - GNU Emacs mode for Mix

(defvar mix-mode-hook nil)

(defvar mix-mode-map nil
  "Keymap for MIX major mode")

(require 'rainbow-delimiters)
(add-hook 'mix-mode-hook 'rainbow-delimiters-mode)

(defgroup mix
  nil
  "Support for the MIX language."
  :group 'languages)

(if mix-mode-map nil
  (setq mix-mode-map (make-keymap))
  (define-key mix-mode-map [(control return)] 'font-lock-fontify-buffer))

(setq auto-mode-alist
      (append
       '(("\\.\\(mix\\)\\'" . mix-mode))
       auto-mode-alist))

;; face definitions

(defgroup mix-faces nil
  "Special faces for the Mix mode."
  :group 'mix)

(defface mix-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face decription for all operators"
  :group 'mix-faces)
(defvar mix-font-lock-operator-face
  'mix-font-lock-operator-face)

;; font-lock

(defun mix-regexp-opt (l)
  (regexp-opt l 'words))

(defun mix-regexp-opt-operators (l)
  (regexp-opt l 'symbols))

(defun mix-regexp-opt-logical-operators (l)
  (regexp-opt l nil))

(defvar mix-keywords
  '("jmp" "j5nz")
  )

;; keep synchronized with src/parser/lexer.mll
(defconst mix-font-lock-keywords-1
  (list
   `(,(mix-regexp-opt
       ;; '("JMP" "j5nz" "assert" "assume"
       ;;   "check" "diverges" "ensures" "invariant"
       ;;   "raises" "reads" "requires" "returns"
       ;;   "variant" "writes")
       mix-keywords
       ) . font-lock-keyword-face)
   `(,(mix-regexp-opt
       '("2H" "4H" "4F" "4B" "2B" "2F")) . font-lock-type-face)
   `(,(mix-regexp-opt-operators
       '("->" "<->")) . font-lock-keyword-face)
   `(,(mix-regexp-opt-logical-operators
       '("/\\" "\\/")) . font-lock-keyword-face)
   `(,(mix-regexp-opt-operators
       '("<" ">" "<=" ">=" "=" "+" "-" "*" "/" "&&" "||")) . mix-font-lock-operator-face)
   `(,"\\<\\(0\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\|9\\)+\\.\\B" . font-lock-keyword-face)
   `(,"\\<\\(0\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\|9\\)+\\.\\(0\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\|9\\)+\\>" . font-lock-keyword-face)
   `(,"\\<\\(0\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\|9\\)+\\>" . font-lock-constant-face)
   )
  "Minimal highlighting for Mix mode")

(defvar mix-font-lock-keywords mix-font-lock-keywords-1
  "Default highlighting for Mix mode")

(defvar mix-indent 4
  "How many spaces to indent in mix mode.")
(make-variable-buffer-local 'mix-indent)

;; syntax

(defvar mix-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; identifiers
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)
    ; strings
    (modify-syntax-entry ?\" "\"" st)
    ; comments
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for mix-mode")

(defconst mix--syntax-propertize
  (syntax-propertize-rules
    ; attributes: [@foo]
    ("\\(\\[\\)@[^]]*\\(]\\)" (1 "!]") (2 "!["))
    ; star: (*)
    ("\\((\\)\\*\\()\\)" (1 "()") (2 ")("))
  ))

;; setting the mode
;;(defun mix-mode ()
(define-derived-mode mix-mode fundamental-mode "Mix"
  "Major mode for editing Mix programs.

\\{mix-mode-map}"
  (interactive)
  ;; (kill-all-local-variables)
  ; hilight
  (set-syntax-table mix-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(mix-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ; indentation
  ;(make-local-variable 'indent-line-function)
  ;(setq indent-line-function 'mix-indent-line)
  ;; show trailing white spaces
  (setq-default spacemacs-show-trailing-whitespace t)
  ;; (setq indent-tabs-mode nil)
  ;; (setq highlight-numbers-mode nil)
  ;; (setq-default tab-width 2)
  ;; (setq indent-line-function 'insert-tab)
  ; OCaml style comments for comment-region, comment-dwim, etc.
  ;; (set (make-local-variable 'comment-start) "(*")
  ;; (set (make-local-variable 'comment-end)   "*)")
  ;; (setq-local syntax-propertize-function mix--syntax-propertize)
  ; menu
  ; providing the mode
  (setq major-mode 'mix-mode)
  (setq mode-name "Mix")
  (use-local-map mix-mode-map)
  (font-lock-mode 1)
  ; (mix-menu)
  (run-hooks 'mix-mode-hook))

(provide 'mix)
