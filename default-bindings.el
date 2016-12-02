;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; DWCB  GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to add
;
;(C-c . hide-entry)
;(C-t . hide-body)
;(C-l . hide-leaves)

(dwcB-configure
 :base global-map
 :gen-binds `(
   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; NAVIGATION ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;
              ;; Direction
                ; small direction
              (,(concat "C-" dwcB-backward-key)  .  backward-char)
              (,(concat "C-" dwcB-forward-key)  .   forward-char)
              (,(concat "H-" dwcB-upward-key)   .   previous-line)
              (,(concat "C-" dwcB-downward-key)  .  next-line)
                ; large direction
              (,(concat "M-" dwcB-backward-key)  .  backward-word)
              (,(concat "M-" dwcB-forward-key)   .  forward-word)
              (,(concat "M-" dwcB-downward-key)  .  forward-paragraph)
              (,(concat "M-" dwcB-upward-key)   .   backward-paragraph)
                ; scroll
              (,(concat "C-S-" dwcB-forward-key) .  scroll-left)
              (,(concat "C-S-" dwcB-backward-key) . scroll-right)
              (,(concat "C-S-" dwcB-upward-key)  .  scroll-down-command)
              (,(concat "C-S-" dwcB-downward-key) . scroll-up-command)
              (,(concat "C-M-" dwcB-upward-key)  .  scroll-other-window-down)
              (,(concat "C-M-" dwcB-downward-key) . scroll-other-window)
              ;; Beginning/End
              (,(concat "C-" dwcB-beginning-key)  .  beginning-of-line)
              (,(concat "C-" dwcB-end-key)     .     end-of-line)
              (,(concat "M-" dwcB-end-key)     .     forward-to-indentation)
              (,(concat "M-" dwcB-beginning-key)  .  backward-to-indentation)
              (,(concat "C-M-" dwcB-beginning-key) . beginning-of-buffer)
              (,(concat "C-M-" dwcB-end-key)    .    end-of-buffer)
              ;; Bigger/Smaller
              (,(concat "C-" dwcB-smaller-key) . narrow-to-region)
              (,(concat "M-" dwcB-smaller-key) . narrow-to-page)
              (,(concat "C-" dwcB-bigger-key) .  widen)
              ;; Search
              (,(concat "C-" dwcB-search-alpha-key) . isearch-forward)
              (,(concat "C-" dwcB-search-beta-key) .  isearch-backward)
              (,(concat "C-S-M-" dwcB-upward-key)  .  upcase-region)
              (,(concat "C-S-M-" dwcB-downward-key) . downcase-region)
              (,(concat "M-" dwcB-search-alpha-key) . helm-swoop)
   ;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; EDITING ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;
              ;; Kill
              (,(concat "C-" dwcB-kill-big-key)   .   kill-line)
              (,(concat "C-" dwcB-kill-or-save-key) . kill-region)
              (,(concat "M-" dwcB-kill-big-key)   .   kill-sentence)
              (,(concat "M-" dwcB-kill-or-save-key) . kill-ring-save)
              ("C-z" . zap-to-char)
              (,dwcB-remove-key . delete-horizontal-space)
              (,(concat "C-" dwcB-remove-key) . delete-indentation)
              ;; Transpose
              (,(concat "C-" dwcB-transpose-key) . transpose-chars)
              (,(concat "M-" dwcB-transpose-key) . transpose-words)
              ;; Yank
              (,(concat "C-" dwcB-yank-key) . yank)
              ;; Note
              (,dwcB-note-key . org-capture)
              ;; Big/Small
              ; ((concat "C-c " ) . fill-region)
              )

 :wnd-binds `(
   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; NAVIGATION ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;
              (,dwcB-forward-key  .  next-buffer)
              (,dwcB-backward-key .  previous-buffer)
              ("U"         .         switch-to-buffer-other-window)
              ("u"         .         switch-to-buffer)
              ("C-u"        .        helm-buffers-list)
              ("o"         .         other-frame)
              ("O"         .         other-window)
              ("/"         .         dired-jump)
              ("?"         .         dired-jump-other-window)
              ;; Search
              (,dwcB-search-alpha-key        .        ido-find-file)
              (,(concat "C-" dwcB-search-alpha-key) . ido-find-file-other-window)
              (,(upcase dwcB-search-alpha-key)   .    ido-find-file-other-frame)
              ;; Search
              (,dwcB-search-alpha-key .  helm-mini)
              (,dwcB-search-alpha-key .  helm-buffers-list)
              (,dwcB-search-beta-key  .  helm-find-files)
   ;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; EDITING ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;
              ;; RESIZE
              (,dwcB-bigger-key         .          enlarge-window-horizontally)
              (,dwcB-smaller-key         .         shrink-window-horizontally)
              (,(concat "C-" dwcB-bigger-key)  .   enlarge-window)
              (,(concat "M-" dwcB-smaller-key)  .  shrink-window)
              ("b"                .                balance-windows)
              ("B"                .                shrink-window-if-larger-than-buffer)
              ("v"                .                golden-ratio)
              ("V"                .                golden-ratio-mode)
              ("z"                .                suspend-frame)
              (,(upcase dwcB-forward-key)     .    split-window-right)
              (,(upcase dwcB-backward-key)    .    split-horizontally)
              (,(upcase dwcB-upward-key)     .     split-window-vertically)
              (,(upcase dwcB-downward-key)    .    split-window-below)
              ;; KILL
              (,dwcB-kill-element-key             .            kill-buffer-and-window)
              (,(upcase dwcB-kill-element-key)        .        kill-some-buffers)
              (,(concat "C-" dwcB-kill-element-key)     .      kill-buffer)
              (,(concat "3 " (upcase dwcB-kill-element-key)) . delete-other-frames)
              (,(concat "3 " dwcB-kill-element-key)     .      delete-frame)
              ;; SAVE
              (,(concat "C-" dwcB-kill-or-save-key)  .  save-buffer)
              (,(upcase dwcB-kill-or-save-key)    .     write-file)
              (,(concat "M-" dwcB-kill-or-save-key)  .  save-some-bueffer))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'prog-map
 :base prog-mode-map
 :env-binds: `(
               (,dwcB-note-key . comment-dwim)
               )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERAL LISP ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dwcB-configure
 :key 'lisp-map
 :parent 'prog-map
 :env-binds `(;; Beg/End
              (,dwcB-beginning-key . sp-beginning-of-sexp)
              (,dwcB-end-key    .    sp-end-of-sexp)
              ;; Upward/Downward
              (,dwcB-downward-key       .       sp-down-sexp)
              (,dwcB-upward-key        .        sp-up-sexp)
              (,(concat "C-" dwcB-upward-key) . sp-backward-up-sexp)
              ;; Forward/Backward
              (,dwcB-forward-key             .             sp-forward-sexp)
              (,dwcB-backward-key             .            sp-backward-sexp)
              (,(concat "C-" dwcB-forward-key)      .      sp-forward-symbol)
              (,(concat "C-" dwcB-backward-key)     .      sp-backward-symbol)
              (,(concat "C-" (upcase dwcB-forward-key))  . sp-next-sexp)
              (,(concat "C-" (upcase dwcB-backward-key)) . sp-previous-sexp)
              ;; Slurp/Barf
              ("e" . sp-forward-slurp-sexp)
              ("r" . sp-forward-barf-sexp)
              ("w" . sp-backward-slurp-sexp)
              ("q" . sp-backward-barf-sexp)
              ;; Transpose
              (,dwcB-transpose-key . sp-transpose-sexp)
              ;; Kill and Save
              (,dwcB-kill-element-key            .             sp-kill-sexp)
              (,(concat "C-" dwcB-kill-element-key)     .      sp-kill-hybrid-sexp)
              (,(concat "C-" (upcase dwcB-kill-element-key)) . sp-backward-kill-sexp)
              (,(concat "M-" dwcB-kill-element-key)      .     sp-copy-sexp)
              )
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'emacs-lisp-mode
 :parent 'lisp-map
 :env-binds `(
              (,dwcB-interpret-key . ielm)
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLOJURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'clojure-mode
 :parent 'lisp-map
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'python-mode
 :parent 'prog-map
 :env-binds `(
              ;; Search/Query
              (,dwcB-query-key     .     elpy-doc)
              (,dwcB-description-key  .  elpy-goto-definition)
              (,dwcB-search-alpha-key .  elpy-rgrep-symbol)
              ;; Direction
              (,dwcB-forward-key         .         elpy-nav-forward-indent)
              (,(concat "C-" dwcB-forward-key)  .  python-nav-forward-sexp)
              (,(concat "M-" dwcB-forward-key)  .  elpy-nav-forward-block)
              ;; Beg/End
              (,dwcB-beginning-key        .        python-nav-beginning-of-statement)
              (,(concat "C-" dwcB-end-key)    .    python-nav-beginning-of-block)
              (,dwcB-end-key           .           python-nav-end-of-statement)
              (,(concat "C-" dwcB-end-key)    .    python-nav-end-of-block)
              ;; Interpreter/Compiler
              (,dwcB-interpret-key         .         elpy-shell-switch-to-shell)
              (,(concat "C-" dwcB-interpret-key)  .  elpy-shell-send-region-or-buffer)
              (,(concat "M-" dwcB-interpret-key)  .  pyvenv-restart-python)
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'org-mode
 :gen-binds `(
              ;; Kill
              (,(concat "C-" dwcB-kill-big-key)  . org-kill-line)
              ;; Beg/End
              (,(concat "C-" dwcB-beginning-key) . org-beginning-of-line)
              (,(concat "C-" dwcB-end-key)    .    org-end-of-line)
              )
 :env-binds `(;; Direction
              (,(concat "C-" dwcB-forward-key)   .  org-forward-heading-same-level)
              (,(concat "C-" dwcB-backward-key)  .  org-previous-heading-same-level)
              (,(concat "C-" dwcB-upward-key)   .   outline-next-visible-heading)
              (,(concat "C-" dwcB-downward-key)  .  outling-previous-visible-heading)
              ;; Beg/Eng
              ;; Bigger/Smaller
              (,dwcB-bigger-key         .          widen)
              (,dwcB-smaller-key         .         org-narrow-to-element)
              (,(concat "C-" dwcB-smaller-key)  .  org-narrow-to-subtree)
              (,(concat "C-" dwcB-smaller-key)  .  org-narrow-to-block)
              ;; Search
              ;; Misc
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; HELM MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'helm--minor-mode
 :parent 'helm-map
 :gen-binds `(
              ;; Direction
              (,(concat "C-" dwcB-downward-key)  .  helm-next-line)
              (,(concat "H-" dwcB-upward-key)   .   helm-previous-line)
              (,(concat "C-" dwcB-forward-key)   .  helm-next-source)
              (,(concat "C-" dwcB-backward-key)  .  helm-previous-source)
              (,(concat "M-" dwcB-downward-key)  .  helm-next-page)
              (,(concat "M-" dwcB-upward-key)   .   helm-previous-page)
              )
 )

(provide 'default-bindings)


;;; default-bindings.el ends here
