;; b.el - GNU Emacs mode for B

(defvar b-mode-hook nil)

(defvar b-mode-map nil
  "Keymap for B major mode")

(require 'rainbow-delimiters)
(add-hook 'b-mode-hook 'rainbow-delimiters-mode)

(defgroup b
  nil
  "Support for the B language."
  :group 'languages)

(if b-mode-map nil
  (setq b-mode-map (make-keymap))
  (define-key b-mode-map [(control return)] 'font-lock-fontify-buffer))

(setq auto-mode-alist
      (append
       '(("\\.\\(b\\)\\'" . b-mode))
       auto-mode-alist))

(defgroup b-faces nil
  "Special faces for the Why3 mode."
  :group 'b)

(defface b-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face decription for all operators"
  :group 'b-faces)
(defvar b-font-lock-operator-face
  'b-font-lock-operator-face)

(setq b-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("break" "else" "for" "step" "if" "return" "while" "extern"))
            (x-types '("void" "bool" "f32" "f64" "i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64"))
            (x-constants '("true" "false"))
            (x-events '("mut"))
            (x-functions '("assert"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-events-regexp . 'font-lock-builtin-face)
          (,x-functions-regexp . 'font-lock-function-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          ("\\[\\(.+?\\)\\]" . (1 'font-lock-constant-face))
          (,(regexp-opt
             '("+" "-" "*" "/" "<<" ">>" "&&" "&" "|" "||" "^" "<" "<=" ">" ">=" "!=" "="))
           . b-font-lock-operator-face)
          )))

(defvar b-mode-syntax-table nil "Syntax table for `b-mode'.")

(setq b-mode-syntax-table
      (let ( (st (make-syntax-table)))
        ;; comments
        (modify-syntax-entry ?/ ". 124b" st)
        (modify-syntax-entry ?\n "> b" st)
        (modify-syntax-entry ?* ". 23" st)
        st))

(define-derived-mode b-mode fundamental-mode "B"
  "Major mode for editing B language code."
  (set-syntax-table b-mode-syntax-table)
  (setq font-lock-defaults '(b-font-lock-keywords)))

(provide 'b-mode)
