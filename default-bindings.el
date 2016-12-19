;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; KEY GENERICS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Direction
(defconst dwcB-backward-key "j")
(defconst dwcB-forward-key "l")
(defconst dwcB-downward-key "k")
(defconst dwcB-upward-key "i")
;; Beginning/End
(defconst dwcB-beginning-key "u")
(defconst dwcB-end-key "o")
;; Search
(defconst dwcB-search-alpha-key "p")
(defconst dwcB-search-beta-key "y")
;; Bigger/Smaller
(defconst dwcB-bigger-key ">")
(defconst dwcB-smaller-key "<")
;; Add/Subtract
(defconst dwcB-shift-left-key "n")  ;; I dont think this is needed
(defconst dwcB-shift-right-key "m") ;; ditto
;; Kill
(defconst dwcB-kill-element-key "d")  ;; should be delete alpha
(defconst dwcB-kill-big-key "a")      ;; should be delete beta
(defconst dwcB-kill-or-save-key "s")
(defconst dwcB-remove-key "z")
;; Create
(defconst dwcB-yank-key "f")    ;; should be insert alpha
(defconst dwcB-insert-key "v")  ;; shoulb be insert beta
(defconst dwcB-note-key "+")
;; Transpose
(defconst dwcB-transpose-key "t") ;; should be switch
;; Undo/Redo
(defconst dwcB-undo-key "r")
(defconst dwcB-redo-key "t")
;; Query
(defconst dwcB-query-key "?")
(defconst dwcB-description-key "/")
;; Compile/Interpret
(defconst dwcB-evaluate-key ":")
(defconst dwcB-compile-key ";")
;; Present
(defconst dwcB-present-key "/")
(defconst dwcB-enter-key "RET")
(defconst dwcB-edit-key "*")
(defconst dwcB-mark-key "&")
(defconst dwcB-sort-key "#")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; DWCB  GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; to add to global ;;;;
;
;(C-c . hide-entry)
;(C-t . hide-body)
;(C-l . hide-leaves)
;; (C-u . outline-up-heading)
;; (C-v . outline-move-subtree-down)
;; (C-^ . outline-move-subtree-up)
;; (dwcB-present-key  .  show-branches)
;; (dwcB-upward-key   .   outline-next-visible-heading)
;; (dwcB-downward-key  .  outline-previous-visible-heading)
;; (dwcB-beginning-key     .     show-all)
;; (dwcB-end-key        .        show-entry)

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
                C-S-M-modifier ((,dwcB-upward-key  .  upcase-region)
                                 ;((concat "C-c " ) . fill-region)
                                (,dwcB-downward-key . downcase-region)))
              )
 :wnd-binds `((
              no-modifier    ((,dwcB-forward-key     .    next-buffer)
                              (,dwcB-backward-key    .    previous-buffer)
                              (,dwcB-search-alpha-key  .  ido-find-file)
                              ("U"         .         switch-to-buffer-other-window)
                              ("u"         .         switch-to-buffer)
                              ("C-u"        .        helm-buffers-list)
                              ("o"         .         other-frame)
                              ("O"         .         other-window)
                              ("/"         .         dired-jump)
                              ("?"         .         dired-jump-other-window)
                              ("b"                .                balance-windows)
                              ("B"                .                shrink-window-if-larger-than-buffer)
                              ("v"                .                golden-ratio)
                              ("V"                .                golden-ratio-mode)
                              ("z"                .                suspend-frame)
                              (,dwcB-search-alpha-key .  helm-mini)
                              (,dwcB-search-alpha-key .  helm-buffers-list)
                              (,dwcB-search-beta-key  .  helm-find-files)
                              (,dwcB-bigger-key         .          enlarge-window-horizontally)
                              (,dwcB-smaller-key         .         shrink-window-horizontally)
                              (dwcB-forward-key     .    split-window-right)
                              (dwcB-backward-key    .    split-horizontally)
                              (dwcB-upward-key     .     split-window-vertically)
                              (dwcB-downward-key    .    split-window-below)
                              (,dwcB-kill-or-save-key  .  save-buffer)
                              (,dwcB-kill-element-key             .            kill-buffer-and-window)
                              (,(upcase dwcB-kill-element-key)        .        kill-some-buffers)
                              )
              C-modifier     ((,dwcB-search-alpha-key . ido-find-file-other-window)
                              (,dwcB-bigger-key  .   enlarge-window)
                              (,dwcB-kill-element-key     .      kill-buffer)
                              (,dwcB-kill-element-key (upcase dwcB-kill-element-key) . delete-other-frames)
                              ((upcase dwcB-kill-element-key) dwcB-kill-element-key     .      delete-frame)
                              (dwcB-kill-or-save-key    .     write-file)

                              )
              M-modifier     ((,dwcB-smaller-key  .  shrink-window)
                              (,dwcB-kill-or-save-key  .  save-some-buffers)))
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'prog-map
 :base prog-mode-map
 :env-binds: `((
                no-modifier ((,dwcB-note-key . comment-dwim))
               ))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; GENERAL LISP ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(dwcB-configure
 :key 'lisp-map
 :parent 'prog-map
 :env-binds `(
              (no-modifier    ((,dwcB-beginning-key    .    sp-beginning-of-sexp)
                               (,dwcB-end-key       .       sp-end-of-sexp)
                               (,dwcB-downward-key    .     sp-down-sexp)
                               (,dwcB-upward-key     .      sp-up-sexp)
                               (,dwcB-forward-key     .     sp-forward-sexp)
                               (,dwcB-backward-key    .     sp-backward-sexp)
                               (dwcB-bigger-key      .      sp-forward-slurp-sexp)
                               (dwcB-smaller-key     .      sp-forward-barf-sexp)
                               ((upcase dwcB-bigger-key)  . sp-backward-slurp-sexp)
                               ((upcase dwcB-smaller-key) . sp-backward-barf-sexp)
                               (,dwcB-transpose-key    .    sp-transpose-sexp)
                               (,dwcB-kill-element-key  .   sp-kill-sexp))
               C-modifier     ((,dwcB-upward-key        .        sp-backward-up-sexp)
                               (,dwcB-forward-key       .        sp-forward-symbol)
                               (,dwcB-backward-key        .      sp-backward-symbol)
                               (upcase dwcB-forward-key     .    sp-next-sexp)
                               (upcase dwcB-backward-key    .    sp-previous-sexp)
                               (dwcB-kill-element-key     .      sp-kill-hybrid-sexp)
                               ((upcase dwcB-kill-element-key) . sp-backward-kill-sexp))
               M-modifier     ((dwcB-kill-element-key      .     sp-copy-sexp))
               ))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'emacs-lisp-mode
 :parent 'lisp-map
 :env-binds `(
              (no-modifier ((,dwcB-interpret-key . ielm)))
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
              (no-modifier ((,dwcB-query-key          .          elpy-doc)
                            (,dwcB-description-key       .       elpy-goto-definition)
                            (,dwcB-search-alpha-key      .       elpy-rgrep-symbol)
                            (,dwcB-forward-key         .         elpy-nav-forward-indent)
                            (,dwcB-beginning-key        .        python-nav-beginning-of-statement)
                            (,dwcB-end-key           .           python-nav-end-of-statement)
                            (,dwcB-interpret-key        .        elpy-shell-switch-to-shell))
               C-modifier  ((dwcB-forward-key  .  python-nav-forward-sexp)
                            (dwcB-end-key    .    python-nav-beginning-of-block)
                            (dwcB-end-key    .    python-nav-end-of-block)
                            (dwcB-interpret-key . elpy-shell-send-region-or-buffer))
               M-modifier  ((dwcB-forward-key   .   elpy-nav-forward-block)
                            (dwcB-interpret-key  .  pyvenv-restart-python)))
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'org-mode
 :gen-binds `(
              (C-modifier  ((dwcB-beginning-key . org-beginning-of-line)
                            (dwcB-end-key    .    org-end-of-line)
                            (dwcB-transpose-key . org-transpose-element)
                            (dwcB-kill-big-key  . org-kill-line)))
              (;; Context sensitive shifters and Timer
               C-modifier   ((dwcB-downward-key .  org-shiftcontroldown)
                             (dwcB-leftward-key .  org-shiftcontrolleft)
                             (dwcB-rightward-key . org-shiftcontrolright)
                             (dwcB-upward-key  .   org-shiftcontrolup))
               M-modifier   ((dwcB-yank-key     .  org-paste-special)
                             (dwcB-enter-key   .   org-timer-pause-or-continue)
                             (dwcB-beginning-key . org-timer-stop)
                             (dwcB-end-key     .   org-timer-start)
                             (dwcB-insert-key  .   org-timer-item)
                             (dwcB-yank-key    .   org-timer)
                             (dwcB-delete-key   .  org-timer-cancel-timer)
                             (dwcB-enter-key   .   org-timer-set-timer))
               ;; Clocks
               M-S-modifier ((dwcB-query-key     .    org-clock-report)
                             (dwcB-note-key     .     org-clock-display)
                             (dwcB-kill-element-key . org-clock-cancel)
                             (dwcB-search-alpha-key . org-clock-goto)
                             (dwcB-end-key     .      org-clock-out)
                             (dwcB-beginning-key   .  org-clock-in)
                             (dwcB-enter-key     .    org-resolve-clocks)
                             (dwcB-present-key    .   org-toggle-time-stamp-overlays))
               ;; Tables
               C-S-modifier ((dwcB-yank-key     .    org-insert-columns-dblock)
                             (dwcB-evaluate-key  .   org-table-eval-formula)
                             ("+"         .          org-table-sum)
                             (dwcB-kill-element   .  org-table-blank-field)
                             (dwcB-description-key . org-table-toggle-formula-debugger)
                             (dwcB-present-key  .    org-table-toggle-coordinate-overlays)
                             (dwcB-transpose-key  .  org-table-create-with-table\.el)
                             (dwcB-insert-key    .   org-table-create-or-convert-from-region)
                             (dwcB-present-key   .   org-table-edit-field)
                             (dwcB-edit-key     .    org-edit-special)
                             (dwcB-shift-right-key . org-table-rotate-recalc-marks)
                             (dwcB-query-key    .    org-table-field-info)))
              )
 :env-binds `(;; Basic navigation and editing
              (no-modifier  ((dwcB-forward-key      .      org-forward-element)
                             (dwcB-backward-key      .     org-backward-element)
                             ((upcase dwcB-bigger-key)  .  widen)
                             (dwcB-smaller-key      .      org-narrow-to-element)
                             (dwcB-kill-big-key     .      org-kill-note-or-show-branches)
                             (dwcB-kill-small-key    .     org-copy)
                             (dwcB-transpose       .       org-emphasize)
                             (dwcB-search-alpha-key   .    org-goto)
                             (dwcB-search-beta-key    .    org-goto-calendar)
                             ((upcase dwcB-kill-big-key) . org-copy-visible)
                             (dwcB-note-key        .       org-add-note)
                             ((upcase dwcB-note-key)   .   org-toggle-comment)
                             (dwcB-insert-key       .      org-ctrl-c-ret)
                             ((upcase dwcB-insert-key)  .  org-sparse-tree)
                             (dwcB-yank-key       .        org-refile)
                             (dwcB-mark-key       .        org-mark-subtree)
                             (dwcB-evaluate-key      .     org-ctrl-c-ctrl-c)
                             (dwcB-enter-key       .       org-open-at-point)
                             (dwcB-edit-key        .       org-tree-to-indirect-buffer)
                             (dwcB-present-key      .      org-toggle-inline-images)
                             (dwcB-sort-key        .       org-sort)
                             (dwcB-bigger-key       .      org-reveal)
                             ((upcase dwcB-present-key) .  org-preview-latex-fragment))
               ;; Navigate headings && General
               C-modifier   ((dwcB-transpose-key .  org-list-make-subtree)
                             (dwcB-forward-key   .  org-forward-heading-same-level)
                             (dwcB-backward-key  .  org-previous-heading-same-level)
                             (dwcB-smaller-key  .   org-narrow-to-subtree)
                             (dwcB-note-key     .   org-footnote-action)
                             (dwcB-enter-key    .   org-meta-return))
               ;; Navigate headings finely && Edit TODOs
               M-modifier   ((dwcB-upward-key    .   org-previous-block)
                             (dwcB-downward-key   .  org-next-block)
                             (dwcB-forward-key   .   org-forward-sentence)
                             (dwcB-backward-key   .  org-backward-sentence)
                             (dwcB-smaller-key   .   org-narrow-to-block)
                             (dwcB-enter-key    .    org-insert-todo-heading)
                             (dwcB-transpose-key  .  org-todo))
               ;; Property editing (context sensitive)
               C-S-modifier ((dwcB-shift-right-key . org-shiftright)
                             (dwcB-shift-left-key  . org-shiftleft)
                             (dwcB-smaller-key   .   org-shiftdown)
                             (dwcB-bigger-key    .   org-shiftup))
               ;; Heading/Table editing (context sensitive)
               M-S-modifier ((dwcB-transpose-key . org-ctrl-c-star)
                             (dwcB-rightward-key . org-shiftmetaright)
                             (dwcB-leftward-key  . org-shiftmetaleft)
                             (dwcB-downward-key  . org-shiftmetadown)
                             (dwcB-upward-key   .  org-shiftmetaup))
               ;; Property editing && Misc
               C-M-modifier ((dwcB-backward-key  . org-previous-link)
                             (dwcB-forward-key  .  org-next-link)
                             (dwcB-yank-key    .   org-insert-link)
                             (dwcB-transpose-key . org-inc-effort)
                             (dwcB-insert-key   .  org-set-effort)
                             (dwcB-transpose-key . org-toggle-checkbox)))
              ;; junk
              `(no-modifier ((x . org-babel-do-key-sequence-in-edit-buffer)
                             (C-x . org-babel-do-key-sequence-in-edit-buffer)
                             (h . org-babel-describe-bindings)
                             (a . org-babel-sha1-hash)
                             (dwcB-beginning-key . org-babel-sha1-hash)
                             (z . org-babel-switch-to-session-with-code)
                             (C-z . org-babel-switch-to-session)
                             (I . org-babel-view-src-block-info)
                             (i . org-babel-lob-ingest)
                             (TAB . org-babel-view-src-block-info)
                             (l . org-babel-load-in-session)
                             (C-l . org-babel-load-in-session)
                             (j . org-babel-insert-header-arg)
                             (C-j . org-babel-insert-header-arg)
                             (c . org-babel-check-src-block)
                             (C-c . org-babel-check-src-block)
                             (f . org-babel-tangle-file)
                             (C-f . org-babel-tangle-file)
                             (t . org-babel-tangle)
                             (C-t . org-babel-tangle)
                             (d . org-babel-demarcate-block)
                             (C-d . org-babel-demarcate-block)
                             (s . org-babel-execute-subtree)
                             (C-s . org-babel-execute-subtree)
                             (b . org-babel-execute-buffer)
                             (C-b . org-babel-execute-buffer)
                             (C-r . org-babel-goto-named-result)
                             (r . org-babel-goto-named-result)
                             (g . org-babel-goto-named-src-block)
                             (C-u . org-babel-goto-src-block-head)
                             (u . org-babel-goto-src-block-head)
                             (v . org-babel-expand-src-block)
                             (C-v . org-babel-expand-src-block)
                             (C-o . org-babel-open-src-block-result)
                             (o . org-babel-open-src-block-result)
                             (dwcB-end-key . org-babel-execute-maybe)
                             (e . org-babel-execute-maybe)
                             (C-n . org-babel-next-src-block)
                             (n . org-babel-next-src-block)
                             (C-p . org-babel-previous-src-block)
                             (p . org-babel-previous-src-block)))
 )


 ;; goes in parents map
 ;; (remap keymap
 ;;        (delete-backward-char . org-delete-backward-char)
 ;;        (delete-char . org-delete-char)
 ;;        (self-insert-command . org-self-insert-command)
 ;;        (transpose-words . org-transpose-words)
 ;;        (backward-paragraph . org-backward-paragraph)
 ;;        (forward-paragraph . org-forward-paragraph)
 ;;        (open-line . org-open-line)
 ;;        (outline-insert-heading . org-ctrl-c-ret)
 ;;        (outline-demote . org-demote-subtree)
 ;;        (outline-promote . org-promote-subtree)
 ;;        (show-branches . org-kill-note-or-show-branches)
 ;;        (outline-backward-same-level . org-backward-heading-same-level)
 ;;        (outline-forward-same-level . org-forward-heading-same-level)
 ;;        (show-subtree . org-show-subtree)
 ;;        (outline-mark-subtree . org-mark-subtree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; HELM MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'helm--minor-mode
 :parent 'helm-map
 :gen-binds `(
              (C-modifier ((dwcB-downward-key  .  helm-next-line)
                           (dwcB-upward-key   .   helm-previous-line)
                           (dwcB-forward-key   .  helm-next-source)
                           (dwcB-backward-key  .  helm-previous-source))
               M-modifier ((dwcB-downward-key  .  helm-next-page)
                           (dwcB-upward-key   .   helm-previous-page)))
              )
 )

(provide 'default-bindings)


;;; default-bindings.el ends here
