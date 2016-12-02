; general editing
(indent-rigidly)
(delete-blank-lines)
(mark-page)
(transpose-lines)
(repeat-complex-command)

; register
(43 . increment-register)
(110 . number-to-register)
(103 . insert-register)
(120 . copy-to-register)
(106 . jump-to-register)
(32 . point-to-register)

; rectangle
(78 . rectangle-number-lines)  ;; useful
(116 . string-rectangle)
(111 . open-rectangle)  ;; useful
(121 . yank-rectangle)
(100 . delete-rectangle) ;; useful
(107 . kill-rectangle)
(99 . clear-rectangle)
(rectangle-mark-mode)
