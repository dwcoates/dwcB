(defun dc/get-key-desc-at-ooint ()
  (interactive)
  (save-excursion
    (let ((number (number-at-point)))
      (if number
        (progn
          (forward-word)
          (backward-word)
          (kill-word 1)
          (insert (key-description (cons number '())))
          )
        (print "no number")
        )
      )
    ))

(defvar dwc-map-bak
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; really shouldn't be anything here. Wait. That's not true.

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENV ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; DIRECTION
                                        ; element
  (dwcB-forward-key . org-forward-element)
  (dwcB-backward-key . org-backward-element)
                                        ; block/sentence
  ((concat "M-" dwcB-upward-key) . org-previous-block)
  ((concat "M-" dwcB-downward-key) . org-next-block)
  ((concat "M-" dwcB-forward-key) . org-forward-sentence)
  ((concat "M-" dwcB-backward-key) . org-backward-sentence)

  ;; GENERAL
  ((concat dwcB-shift-right-key) . org-shiftright)
  ((concat dwcB-shift-left-key) . org-shiftleft)
  ((concat dwcB-smaller-key) . org-shiftdown)
  ((concat dwcB-bigger-key) . org-shiftup)

  ;; TRANSPOSE
  (dwcB-transpose-key . org-transpose-element)

  ;; BIGGER/SMALLER
     ; show heading
         ; show branches, not body
    ("key" . show-branches)
         ; show body, not branches
    (dwcB-end-key . show-entry)
         ; show all headings in file
    (dwcB-beginning-key . show-all)
    ; close heading
    (C-c . hide-entry)
    (C-t . hide-body)
    (C-r . org-reveal)
    (C-o . hide-other)


    (C-> . outline-demote)
    (C-< . outline-promote)

  ;; Insert
  (RET . outline-insert-heading)

  ;; Copy/Kill
  (dwcB-kill-big-key . org-kill-note-or-show-branches)
  (w . org-copy)
  (v . org-copy-visible)

  ;; Add/Subtract
  (C-z . org-add-note)
  (";" . org-toggle-comment)

  ;; Misc
  (C-* . org-list-make-subtree)
  (C-o . org-open-at-point)
  (p . org-footnote-action)
  ("d" . org-export-dispatch)

  (RET . org-ctrl-c-ret)
  (C-d . org-deadline)
  (C-s . org-schedule)
  (C-j . org-goto)
  (m . org-meta-return)
  (M . org-insert-todo-heading)


  ;; General Navigation
  (> . org-goto-calendar)
  (/ . org-sparse-tree)

  ;; Cycle
  (C-t . org-todo)

  ;; who cares
  (: . org-toggle-fixed-width-section)

  ;; Misc
  (C-c . org-ctrl-c-ctrl-c)

    ;;;;;;;; Link (should be general)
  (C-p . org-previous-link)
  (C-n . org-next-link)
  (C-l . org-insert-link)

    ;;;;;;;; Heading Editing (should be general)
  (E . org-inc-effort)
  (e . org-set-effort)
  (C-b . org-toggle-checkbox)
  (* . org-ctrl-c-star)
  (- . org-ctrl-c-minus)
  (^ . org-sort)
  (@ . org-mark-subtree)
  (C-u . outline-up-heading)
  (C-w . org-refile)
                 ; direction
  ((concat "M-S-" dwcB-rightward-key) . org-shiftmetaright)
  ((concat "M-S-" dwcB-leftward-key) . org-shiftmetaleft)
  (C-v . outline-move-subtree-down)
  (C-^ . outline-move-subtree-up)
  ((concat "M-S-" dwcB-downward-key) . org-shiftmetadown)
  ((concat "M-S-" dwcB-upward-key) . org-shiftmetaup)
  ((concat "C-S-" dwcB-leftward-key) . org-shiftcontrolleft) ;prev
  ((concat "C-S-" dwcB-rightward-key) . org-shiftcontrolright) ;next

    ;;;;;;;; Presentation
  (C-v . org-toggle-inline-images)
  (C-l . org-preview-latex-fragment)
  (b . org-tree-to-indirect-buffer)
  (C-f . org-emphasize)

        ;;;;;;;; Clock
  ((concat "C-S-" dwcB-downward-key) . org-shiftcontroldown) ;prev
  ((concat "C-S-" dwcB-upward-key) . org-shiftcontrolup) ;next
  (_ . org-timer-stop)
  (0 . org-timer-start)
  (- . org-timer-item)
  (. . org-timer)
  (: . org-timer-cancel-timer)
  (";" . org-timer-set-timer)
  (C-r . org-clock-report)
  (C-d . org-clock-display)
  (C-q . org-clock-cancel)
  (C-j . org-clock-goto)
  (C-o . org-clock-out)
  (C-z . org-resolve-clocks)
  (C-x . org-clock-in-last)
  (TAB . org-clock-in)
  (C-t . org-toggle-time-stamp-overlays)
  (, . org-timer-pause-or-continue)
        ;;;;;;;; Table
  (C-c . org-columns)
  (i . org-insert-columns-dblock)
  (= . org-table-eval-formula)
  (+ . org-table-sum)
  (SPC . org-table-blank-field)
  ({ . org-table-toggle-formula-debugger)
  (} . org-table-toggle-coordinate-overlays)
  (~ . org-table-create-with-table\.el)
  (| . org-table-create-or-convert-from-region)
  (` . org-table-edit-field)
  (' . org-edit-special)
                                        ;("C-#" . org-table-rotate-recalc-marks)
  (? . org-table-field-info)
  (dwcB-yank-key . org-paste-special)

  ;; need to test to see if this works. I think it will.
  (remap kaeymap
         (delete-backward-char . org-delete-backward-char)
         (delete-char . org-delete-char)
         (self-insert-command . org-self-insert-command)
         (transpose-words . org-transpose-words)
         (backward-paragraph . org-backward-paragraph)
         (forward-paragraph . org-forward-paragraph)
         (open-line . org-open-line)
         (outline-insert-heading . org-ctrl-c-ret)
         (outline-demote . org-demote-subtree)
         (outline-promote . org-promote-subtree)
         (show-branches . org-kill-note-or-show-branches)
         (outline-backward-same-level . org-backward-heading-same-level)
         (outline-forward-same-level . org-forward-heading-same-level)
         (show-subtree . org-show-subtree)
         (outline-mark-subtree . org-mark-subtree))
  ;; unchanged
  (C-v keymap
       (ESC keymap
            (C-h . org-babel-mark-block))
       (x . org-babel-do-key-sequence-in-edit-buffer)
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
       (p . org-babel-previous-src-block))
