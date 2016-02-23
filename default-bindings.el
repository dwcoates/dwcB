(keyboard-translate ?\C-i ?\H-i)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; KEY GENERICS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction
(defconst dwcB-backward-key "j")
(defconst dwcB-forward-key "l")
(defconst dwcB-downward-key "k")
(defconst dwcB-upward-key "i")
;; Beginning/End
(defconst dwcB-beginning-key "o")
(defconst dwcB-end-key "p")
;; Search
(defconst dwcB-search-alpha-key "y")
(defconst dwcB-search-beta-key "n")
;; Bigger/Smaller
(defconst dwcB-bigger-key ">")
(defconst dwcB-smaller-key "<")
;; Kill
(defconst dwcB-kill-element-key "d")
(defconst dwcB-kill-big-key "a")
(defconst dwcB-kill-or-save-key "s")
;; Yank
(defconst dwcB-yank-key "f")
;; Transpose
(defconst dwcB-transpose-key "t")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; INTRA-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwcB-global-map (kbd (concat "C-" dwcB-backward-key)) 'backward-char)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-forward-key)) 'forward-char)
(define-key dwcB-global-map (kbd (concat "H-" dwcB-upward-key)) 'previous-line)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-downward-key)) 'next-line)
;
(define-key dwcB-global-map (kbd (concat "M-" dwcB-backward-key)) 'backward-word)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-forward-key)) 'forward-word)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-downward-key)) 'forward-paragraph)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-upward-key)) 'backward-paragraph)
;
(define-key dwcB-global-map (kbd (concat "C-S-" dwcB-forward-key)) 'scroll-left)
(define-key dwcB-global-map (kbd (concat "C-S-" dwcB-backward-key)) 'scroll-right)
(define-key dwcB-global-map (kbd (concat "C-S-" dwcB-upward-key)) 'scroll-down-command)
(define-key dwcB-global-map (kbd (concat "C-S-" dwcB-downward-key)) 'scroll-up-command)
;; Beginning/End
(define-key dwcB-global-map (kbd (concat "C-M-" dwcB-upward-key)) 'scroll-other-window-down)
(define-key dwcB-global-map (kbd (concat "C-M-" dwcB-downward-key)) 'scroll-other-window)
;
(define-key dwcB-global-map (kbd (concat "C-" dwcB-beginning-key)) 'beginning-of-line)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-end-key)) 'end-of-line)
;
(define-key dwcB-global-map (kbd (concat "M-" dwcB-end-key)) 'forward-to-indentation)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-beginning-key)) 'backward-to-indentation)
;
(define-key dwcB-global-map (kbd (concat "C-M-" dwcB-beginning-key)) 'beginning-of-buffer)
(define-key dwcB-global-map (kbd (concat "C-M-" dwcB-end-key)) 'end-of-buffer)
;; Bigger/Smaller
(define-key dwcB-global-map (kbd (concat "C-" dwcB-smaller-key)) 'narrow-to-region)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-smaller-key)) 'narrow-to-page)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-bigger-key)) 'widen)
;; Search
(define-key dwcB-global-map (kbd (concat "C-" dwcB-search-alpha-key)) 'isearch-forward)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-search-beta-key)) 'isearch-backward)
;; Misc
(define-key dwcB-global-map (kbd "C-'") 'recenter-top-bottom)
;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwcB-global-map (kbd (concat "C-S-M-" dwcB-upward-key)) 'upcase-region)
(define-key dwcB-global-map (kbd (concat "C-S-M-" dwcB-downward-key)) 'downcase-region)
;; Kill
(define-key dwcB-global-map (kbd (concat "C-" dwcB-kill-big-key)) 'kill-line)
(define-key dwcB-global-map (kbd (concat "C-" dwcB-kill-or-save-key)) 'kill-region)
;
(define-key dwcB-global-map (kbd (concat "M-" dwcB-kill-big-key)) 'kill-sentence)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-kill-or-save-key)) 'kill-ring-save)
;
(define-key dwcB-global-map (kbd "C-z") 'zap-to-char)
;; Whitespace
(define-key dwcB-global-map (kbd "C-v") 'delete-horizontal-space)
(define-key dwcB-global-map (kbd "M-v") 'delete-indentation)
;; Transpose
(define-key dwcB-global-map (kbd (concat "C-" dwcB-transpose-key)) 'transpose-chars)
(define-key dwcB-global-map (kbd (concat "M-" dwcB-transpose-key)) 'transpose-words)
;; Yank
(define-key dwcB-global-map (kbd (concat "C-" dwcB-yank-key)) 'yank)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; INTER-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-key dwcB-inter-buffer-map (kbd dwcB-forward-key) 'next-buffer)
(define-key dwcB-inter-buffer-map (kbd dwcB-backward-key) 'previous-buffer)
;
(define-key dwcB-inter-buffer-map (kbd "U") 'switch-to-buffer-other-window)
(define-key dwcB-inter-buffer-map (kbd "u") 'switch-to-buffer)
(define-key dwcB-inter-buffer-map (kbd "C-u") 'helm-buffers-list)
;
(define-key dwcB-inter-buffer-map (kbd "o") 'other-frame)
(define-key dwcB-inter-buffer-map (kbd "O") 'other-window)
;; DIRED
(define-key dwcB-inter-buffer-map (kbd "/") 'dired-jump)
(define-key dwcB-inter-buffer-map (kbd "?") 'dired-jump-other-window)
;; FIND FILE
(define-key dwcB-inter-buffer-map (kbd dwcB-search-alpha-key) 'ido-find-file)
(define-key dwcB-inter-buffer-map (kbd (concat "C-" dwcB-search-alpha-key)) 'ido-find-file-other-window)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-search-alpha-key)) 'ido-find-file-other-frame)
;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; RESIZE
(define-key dwcB-inter-buffer-map (kbd dwcB-bigger-key) 'enlarge-window-horizontally)
(define-key dwcB-inter-buffer-map (kbd dwcB-smaller-key) 'shrink-window-horizontally)
(define-key dwcB-inter-buffer-map (kbd (concat "C-" dwcB-bigger-key)) 'enlarge-window)
(define-key dwcB-inter-buffer-map (kbd (concat "M-" dwcB-smaller-key)) 'shrink-window)
;
(define-key dwcB-inter-buffer-map (kbd "b") 'balance-windows)
(define-key dwcB-inter-buffer-map (kbd "B") 'shrink-window-if-larger-than-buffer)
(define-key dwcB-inter-buffer-map (kbd "v") 'golden-ratio)
(define-key dwcB-inter-buffer-map (kbd "V") 'golden-ratio-mode)
;
(define-key dwcB-inter-buffer-map (kbd "z") 'suspend-frame)
;; SPLIT
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-forward-key)) 'split-window-right)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-backward-key)) 'split-horizontally)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-upward-key)) 'split-window-vertically)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-downward-key)) 'split-window-below)
;; KILL
(define-key dwcB-inter-buffer-map (kbd dwcB-kill-element-key) 'kill-buffer-and-window)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-kill-element-key)) 'kill-some-buffers)
(define-key dwcB-inter-buffer-map (kbd (concat "C-" dwcB-kill-element-key)) 'kill-buffer)
;
(define-key dwcB-inter-buffer-map (kbd (concat "3 " (upcase dwcB-kill-element-key))) 'delete-other-frames)
(define-key dwcB-inter-buffer-map (kbd (concat "3 " dwcB-kill-element-key)) 'delete-frame)
;; SAVE
(define-key dwcB-inter-buffer-map (kbd (concat "C-" dwcB-kill-or-save-key)) 'save-buffer)
(define-key dwcB-inter-buffer-map (kbd (upcase dwcB-kill-or-save-key)) 'write-file)
(define-key dwcB-inter-buffer-map (kbd (concat "M-" dwcB-kill-or-save-key)) 'save-some-bueffer)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERAL LISP ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
   :ensure t)

(defvar dwcB-lisp-mode-shared-map
  (dwcB-create-map nil lisp-mode-shared-map (keymap-parent lisp-mode-shared-map))
  "Keymap from which most lisp maps should derive in the dwcB scheme."
 )
(require 'use-package)

 ;; should be done by dwcB-add-major-map
(setq dwcB-lisp-mode-shared-prefix-map (make-sparse-keymap))
(define-key dwcB-lisp-mode-shared-map (kbd "C-e") dwcB-lisp-mode-shared-prefix-map)

   ;; Beg/End
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-beginning-key)  'sp-beginning-of-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-end-key)  'sp-end-of-sexp)
   ;; Upward/Downward
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-downward-key)  'sp-down-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-upward-key)  'sp-up-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" dwcB-downward-key))  'sp-backward-down-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" dwcB-upward-key))  'sp-backward-up-sexp)
   ;; Forward/Backward
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-forward-key)  'sp-forward-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-backward-key)  'sp-backward-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" dwcB-forward-key))   'sp-forward-symbol)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" dwcB-backward-key))  'sp-backward-symbol)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" (upcase dwcB-forward-key))) 'sp-next-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" (upcase dwcB-backward-key))) 'sp-previous-sexp)
   ;; Slurp/Barf
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd "e")  'sp-forward-slurp-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd "r")  'sp-forward-barf-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd "w")   'sp-backward-slurp-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd "q")   'sp-backward-barf-sexp)
;; Transpose
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-transpose-key)  'sp-transpose-sexp)
   ;; Kill and Save
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd dwcB-kill-element-key) 'sp-kill-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" dwcB-kill-element-key)) 'sp-kill-hybrid-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "C-" (upcase dwcB-kill-element-key))) 'sp-backward-kill-sexp)
(define-key dwcB-lisp-mode-shared-prefix-map
  (kbd (concat "M-" dwcB-kill-element-key)) 'sp-copy-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; EMACS LISP ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar dwcB-emacs-lisp-mode-map
;;   (dwcB-add-major-map 'emacs-lisp-mode nil nil dwcB-lisp-mode-shared-map)
;;   "Keymap used by dwcB for emacs-lisp-mode buffers."
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; CLOJURE ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar dwcB-clojure-mode-map
;;   (dwcB-add-major-map 'clojure-mode nil nil dwcB-lisp-mode-shared-map)
;;   "Keymap used by dwcB for clojure-mode buffers."
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar dwcB-org-mode-map
;;   (dwcB-add-major-map 'org-mode nil 'org-mode-map (keymap-parent org-mode-map))
;;   "Keymap used by dwcB for org-mode buffers."
;;   )


;; (define-key dwcB-org-mode-map
;;   (kbd (concat "C-" dwcB-downward-key))  nil)
;; (define-key dwcB-org-mode-map
;;   (kbd (concat "C-" dwcB-upward-key))  nil)
;; (define-key dwcB-org-mode-map
;;   (kbd (concat "C-" dwcB-backward-key))  nil)
;; (define-key dwcB-org-mode-map
;;   (kbd (concat "C-" dwcB-forward-key))  nil)



;; (setq dwcB-org-mode-prefix-map (make-sparse-keymap))
;; (define-key dwcB-org-mode-map (kbd dwcB-major-prefix) dwcB-org-mode-prefix-map)



(provide 'default-bindings)
