;;; Code:

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; DWCB  GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :base global-map
 :gen-binds `(
   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; NAVIGATION ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;
              ;; Direction
              (,(concat "C-" dwcB-backward-key) . backward-char)
              (,(concat "C-" dwcB-forward-key) . forward-char)
              (,(concat "H-" dwcB-upward-key) . previous-line)
              (,(concat "C-" dwcB-downward-key) . next-line)
              (,(concat "M-" dwcB-backward-key) . backward-word)
              (,(concat "M-" dwcB-forward-key) . forward-word)
              (,(concat "M-" dwcB-downward-key) . forward-paragraph)
              (,(concat "M-" dwcB-upward-key) . backward-paragraph)
              (,(concat "C-S-" dwcB-forward-key) . scroll-left)
              (,(concat "C-S-" dwcB-backward-key) . scroll-right)
              (,(concat "C-S-" dwcB-upward-key) . scroll-down-command)
              (,(concat "C-S-" dwcB-downward-key) . scroll-up-command)
              (,(concat "C-M-" dwcB-upward-key) . scroll-other-window-down)
              (,(concat "C-M-" dwcB-downward-key) . scroll-other-window)
              ;; Beginning/End
              (,(concat "C-" dwcB-beginning-key) . beginning-of-line)
              (,(concat "C-" dwcB-end-key) . end-of-line)
              (,(concat "M-" dwcB-end-key) . forward-to-indentation)
              (,(concat "M-" dwcB-beginning-key) . backward-to-indentation)
              (,(concat "C-M-" dwcB-beginning-key) . beginning-of-buffer)
              (,(concat "C-M-" dwcB-end-key) . end-of-buffer)
              ;; Bigger/Smaller
              (,(concat "C-" dwcB-smaller-key) . narrow-to-region)
              (,(concat "M-" dwcB-smaller-key) . narrow-to-page)
              (,(concat "C-" dwcB-bigger-key) . widen)
              ;; Search
              (,(concat "C-" dwcB-search-alpha-key) . isearch-forward)
              (,(concat "C-" dwcB-search-beta-key) . isearch-backward)
              (,(concat "C-S-M-" dwcB-upward-key) . upcase-region)
              (,(concat "C-S-M-" dwcB-downward-key) . downcase-region)
   ;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; EDITING ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;
              ;; Kill
              (,(concat "C-" dwcB-kill-big-key) . kill-line)
              (,(concat "C-" dwcB-kill-or-save-key) . kill-region)
              (,(concat "M-" dwcB-kill-big-key) . kill-sentence)
              (,(concat "M-" dwcB-kill-or-save-key) . kill-ring-save)
              ("C-z" . zap-to-char)
              ("C-v" . delete-horizontal-space)
              ("M-v" . delete-indentation)
              ;; Transpose
              (,(concat "C-" dwcB-transpose-key) . transpose-chars)
              (,(concat "M-" dwcB-transpose-key) . transpose-words)
              ;; Yank
              (,(concat "C-" dwcB-yank-key) . yank))
 :wnd-binds `(
   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; NAVIGATION ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;
              (,dwcB-forward-key . next-buffer)
              (,dwcB-backward-key . previous-buffer)
              ("U" . switch-to-buffer-other-window)
              ("u" . switch-to-buffer)
              ("C-u" . helm-buffers-list)
              ("o" . other-frame)
              ("O" . other-window)
              ("/" . dired-jump)
              ("?" . dired-jump-other-window)
              ;; Search
              (,dwcB-search-alpha-key . ido-find-file)
              (,(concat "C-" dwcB-search-alpha-key) . ido-find-file-other-window)
              (,(upcase dwcB-search-alpha-key) . ido-find-file-other-frame)
   ;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; EDITING ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;
              ;; RESIZE
              (,dwcB-bigger-key . enlarge-window-horizontally)
              (,dwcB-smaller-key . shrink-window-horizontally)
              (,(concat "C-" dwcB-bigger-key) . enlarge-window)
              (,(concat "M-" dwcB-smaller-key) . shrink-window)
              ("b" . balance-windows)
              ("B" . shrink-window-if-larger-than-buffer)
              ("v" . golden-ratio)
              ("V" . golden-ratio-mode)
              ("z" . suspend-frame)
              (,(upcase dwcB-forward-key) . split-window-right)
              (,(upcase dwcB-backward-key) . split-horizontally)
              (,(upcase dwcB-upward-key) . split-window-vertically)
              (,(upcase dwcB-downward-key) . split-window-below)
              ;; KILL
              (,dwcB-kill-element-key . kill-buffer-and-window)
              (,(upcase dwcB-kill-element-key) . kill-some-buffers)
              (,(concat "C-" dwcB-kill-element-key) . kill-buffer)
              (,(concat "3 " (upcase dwcB-kill-element-key)) . delete-other-frames)
              (,(concat "3 " dwcB-kill-element-key) . delete-frame)
              ;; SAVE
              (,(concat "C-" dwcB-kill-or-save-key) . save-buffer)
              (,(upcase dwcB-kill-or-save-key) . write-file)
              (,(concat "M-" dwcB-kill-or-save-key) . save-some-bueffer))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERAL LISP ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dwcB-configure
 :key lisp-map
 :env-binds `(;; Beg/End
              (dwcB-beginning-key  sp-beginning-of-sexp)
              (dwcB-end-key .  sp-end-of-sexp)
              ;; Upward/Downward
              (dwcB-downward-key  sp-down-sexp)
              (dwcB-upward-key  sp-up-sexp)
              ((concat "C-" dwcB-downward-key)  sp-backward-down-sexp)
              ((concat "C-" dwcB-upward-key)  sp-backward-up-sexp)
              ;; Forward/Backward
              (dwcB-forward-key  sp-forward-sexp)
              (dwcB-backward-key  sp-backward-sexp)
              ((concat "C-" dwcB-forward-key)   sp-forward-symbol)
              ((concat "C-" dwcB-backward-key)  sp-backward-symbol)
              ((concat "C-" (upcase dwcB-forward-key)) sp-next-sexp)
              ((concat "C-" (upcase dwcB-backward-key)) sp-previous-sexp)
              ;; Slurp/Barf
              ("e"  sp-forward-slurp-sexp)
              ("r"  sp-forward-barf-sexp)
              ("w"   sp-backward-slurp-sexp)
              ("q"   sp-backward-barf-sexp)
              ;; Transpose
              (dwcB-transpose-key  sp-transpose-sexp)
              ;; Kill and Save
              (dwcB-kill-element-key sp-kill-sexp)
              ((concat "C-" dwcB-kill-element-key) sp-kill-hybrid-sexp)
              ((concat "C-" (upcase dwcB-kill-element-key)) sp-backward-kill-sexp)
              ((concat "M-" dwcB-kill-element-key) sp-copy-sexp)
              )
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key emacs-lisp-mode
 :parent lisp-map
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; CLOJURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key clojure-mode
 :parent lisp-map
 )

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


(define-key dwcB--global-map (kbd "C-'") 'recenter-top-bottom)
