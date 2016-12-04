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
 :gen-binds `((
               C-modifier     (;; direction
                                (,dwcB-backward-key  .  backward-char)
                                (,dwcB-forward-key  .   forward-char)
                                (,dwcB-upward-key   .   previous-line)
                                (,dwcB-downward-key  .  next-line)
                                ;; beg/end
                                (,dwcB-beginning-key  .  beginning-of-line)
                                (,dwcB-end-key     .     end-of-line)
                                ;; presentation
                                (,dwcB-smaller-key . narrow-to-region)
                                (,dwcB-bigger-key .  widen)
                                ;; search
                                (,dwcB-search-alpha-key . isearch-forward)
                                (,dwcB-search-beta-key .  isearch-backward)
                                (,dwcB-kill-big-key   .   kill-line)
                                (,dwcB-kill-or-save-key . kill-region)
                                (,dwcB-remove-key . delete-indentation)
                                (,dwcB-transpose-key . transpose-chars)
                                (,dwcB-remove-key . delete-horizontal-space)
                                ("z" . zap-to-char)
                                (,dwcB-yank-key . yank)
                                (,dwcB-note-key . org-capture))
                M-modifier     (;; direction
                                (,dwcB-backward-key  .  backward-word)
                                (,dwcB-forward-key   .  forward-word)
                                (,dwcB-downward-key  .  forward-paragraph)
                                (,dwcB-upward-key   .   backward-paragraph)
                                ;; beg/end
                                (,dwcB-end-key     .     forward-to-indentation)
                                (,dwcB-beginning-key  .  backward-to-indentation)
                                (,dwcB-search-alpha-key . helm-swoop)
                                (,dwcB-smaller-key . narrow-to-page)
                                (,dwcB-kill-big-key   .   kill-sentence)
                                (,dwcB-kill-or-save-key . kill-ring-save)
                                (,dwcB-transpose-key . transpose-words))
                C-M-modifier   (;; direction
                                (,dwcB-upward-key  .  scroll-other-window-down)
                                (,dwcB-downward-key . scroll-other-window)
                                ;; beg/end
                                (,dwcB-beginning-key . beginning-of-buffer)
                                (,dwcB-end-key    .    end-of-buffer))
                C-S-modifier   ((,dwcB-forward-key .  scroll-left)
                                (,dwcB-backward-key . scroll-right)
                                (,dwcB-upward-key  .  scroll-down-command)
                                (,dwcB-downward-key . scroll-up-command))
                M-S-modifier   ()
                C-S-M-modifier ((,dwcB-upward-key  .  upcase-region)
                                (,dwcB-downward-key . downcase-region))
              ; ((concat "C-c " ) . fill-region)
                )
              `())

 :wnd-binds `(
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
              ("C-" dwcB-search-alpha-key . ido-find-file-other-window)
              (dwcB-search-alpha-key   .    ido-find-file-other-frame)
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
              ("C-" dwcB-bigger-key  .   enlarge-window)
              ("M-" dwcB-smaller-key  .  shrink-window)
              ("b"                .                balance-windows)
              ("B"                .                shrink-window-if-larger-than-buffer)
              ("v"                .                golden-ratio)
              ("V"                .                golden-ratio-mode)
              ("z"                .                suspend-frame)
              (dwcB-forward-key     .    split-window-right)
              (dwcB-backward-key    .    split-horizontally)
              (dwcB-upward-key     .     split-window-vertically)
              (dwcB-downward-key    .    split-window-below)
              ;; KILL
              (,dwcB-kill-element-key             .            kill-buffer-and-window)
              (,(upcase dwcB-kill-element-key)        .        kill-some-buffers)
              ("C-" dwcB-kill-element-key     .      kill-buffer)
              ("3 " (upcase dwcB-kill-element-key) . delete-other-frames)
              ("3 " dwcB-kill-element-key     .      delete-frame)
              ;; SAVE
              ("C-" dwcB-kill-or-save-key  .  save-buffer)
              (dwcB-kill-or-save-key    .     write-file)
              ("M-" dwcB-kill-or-save-key  .  save-some-bueffer))
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
