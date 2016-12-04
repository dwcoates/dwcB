;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; DWCB  GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; to add ;;;;
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

 :wnd-binds `((
              no-modifier    ((,dwcB-forward-key  .  next-buffer)
                              (,dwcB-backward-key .  previous-buffer)
                              (,dwcB-search-alpha-key        .        ido-find-file)
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
              `()
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
              (no-modifier    ((,dwcB-beginning-key . sp-beginning-of-sexp)
                               (,dwcB-end-key    .    sp-end-of-sexp)
                               (,dwcB-downward-key       .       sp-down-sexp)
                               (,dwcB-upward-key        .        sp-up-sexp)
                               (,dwcB-forward-key             .             sp-forward-sexp)
                               (,dwcB-backward-key             .            sp-backward-sexp)
                               ("e" . sp-forward-slurp-sexp)
                               ("r" . sp-forward-barf-sexp)
                               ("w" . sp-backward-slurp-sexp)
                               ("q" . sp-backward-barf-sexp)
                               (,dwcB-transpose-key . sp-transpose-sexp)
                               (,dwcB-kill-element-key            .             sp-kill-sexp))
               C-modifier     ((,dwcB-upward-key . sp-backward-up-sexp)
                               (,dwcB-forward-key      .      sp-forward-symbol)
                               (,dwcB-backward-key     .      sp-backward-symbol)
                               (upcase dwcB-forward-key  . sp-next-sexp)
                               (upcase dwcB-backward-key . sp-previous-sexp)
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
              (no-modifier ((,dwcB-query-key     .     elpy-doc)
                            (,dwcB-description-key  .  elpy-goto-definition)
                            (,dwcB-search-alpha-key .  elpy-rgrep-symbol)
                            (,dwcB-forward-key         .         elpy-nav-forward-indent)
                            (,dwcB-beginning-key        .        python-nav-beginning-of-statement)
                            (,dwcB-end-key           .           python-nav-end-of-statement)
                            (,dwcB-interpret-key         .         elpy-shell-switch-to-shell))
               C-modifier  ((dwcB-forward-key  .  python-nav-forward-sexp)
                            (dwcB-end-key    .    python-nav-beginning-of-block)
                            (dwcB-end-key    .    python-nav-end-of-block)
                            (dwcB-interpret-key  .  elpy-shell-send-region-or-buffer))
               M-modifier  ((dwcB-forward-key  .  elpy-nav-forward-block)
                            (dwcB-interpret-key  .  pyvenv-restart-python)))
              )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dwcB-configure
 :key 'org-mode
 :gen-binds `(
              (C-modifier  ((dwcB-kill-big-key  . org-kill-line)
                            (dwcB-beginning-key . org-beginning-of-line)
                            (dwcB-end-key    .    org-end-of-line)))
              )
 :env-binds `(;; Direction
              (no-modifier ((,dwcB-bigger-key         .          widen)
                            (,dwcB-smaller-key         .         org-narrow-to-element))
               C-modifier  ((dwcB-forward-key   .  org-forward-heading-same-level)
                            (dwcB-backward-key  .  org-previous-heading-same-level)
                            (dwcB-upward-key   .   outline-next-visible-heading)
                            (dwcB-downward-key  .  outling-previous-visible-heading)
                            (dwcB-smaller-key  .  org-narrow-to-subtree)
                            (dwcB-smaller-key  .  org-narrow-to-block)))
              )
 )

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
