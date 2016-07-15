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
;; Kill
(defconst dwcB-kill-element-key "d")
(defconst dwcB-kill-big-key "a")
(defconst dwcB-kill-or-save-key "s")
;; Yank
(defconst dwcB-yank-key "f")
;; Transpose
(defconst dwcB-transpose-key "t")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; MAPS/PREFIXES ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwcB--global-map (make-sparse-keymap)
  "alist for tracking dwcB versions of general keymaps")
(defvar dwcB--inter-buffer-map (make-sparse-keymap)
  "Prefix map for window, buffer, and frame commands.")

(defconst dwcB-major-prefix "C-e"
  "dwc-binding prefix used as default for major modes. Major mode commands may be
bound to keys outside of prefix (see dwcB-add-major-mode-map).")
(defconst dwcB-inter-buffer-prefix "C-w"
  "dwc-binding prefix used as default for inter-buffer navigation and editing.")

(define-key dwcB--global-map (kbd dwcB-inter-buffer-prefix) dwcB--inter-buffer-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; SAVES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwcB--major-alist '()
  "first searched when setting local-map. Then looks outside")
(defvar dwcB--minor-alist '())
(defvar dwcB--minor-alist--saved '() "list of keymaps to be appended to minor-mode-map-alist")
(defvar dwcB--major-alist--saved '() "list of tuples (mode, map) to be restored")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; DWCB ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode dwcB-mode
  "Organized alternative bindings to Emacs defaults."
  :lighter " dwcB"
  :global t
  (if dwcB-mode
      (dwcB--setup)
    (dwcB--teardown)
    )
  )

(defun dwcB--setup ()
  (progn
    (keyboard-translate ?\C-i ?\H-i)
    (dwcB--set-global-map)
    ;;     (add-hook 'after-change-major-mode-hook 'dwcB-major-mode-update)
    ))

(defun dwcB--teardown ()
  (progn
    (keyboard-translate ?\C-i ?\C-i)
    (dwcB--reset-global-map)
    ;;   (remove-hook 'after-change-major-mode-hook 'dwcB-major-mode-update)
   ))

(defun dwcB--set-global-map ()
  "Set the current global map to dwcB global map."
  (if (eq (current-global-map) global-map)
      (use-global-map dwcB--global-map))
  )

(defun dwcB--reset-global-map ()
  "Set the current global map to Emacs default, global-map."
  (if (eq global-map dwcB--global-map)
      (current-global-map)
    (use-global-map global-map))
  )

(defun dwcB--major-mode-update ()
  "Switch to corresponding dwcB keymap for the current major mode."
  (let ((mode-map (assoc major-mode dwcB--major-alist)))
    (when mode-map
      (use-local-map (cdr mode-map))
    ))
  )

(defun dwcB--save (MODE-MAP ALIST)
  (let ((entry-mode-map (assoc (car MODE-MAP) (symbol-value ALIST))))
    (if entry-mode-map
        ;; amend set value in place. Don't want this to just add to list.
        ;; this can always just go if it causes problems
        ;; (setf (cdr (assoc s(car MODE-MAP) ALIST))
        ;;       (make-composed-keymap `(,(cdr MODE-MAP)
        ;;                               ,(cdr entry-mode-map)))
        ;;       )
        (add-to-list ALIST
                     (cons (car MODE-MAP)
                           (make-composed-keymap `(,(cdr MODE-MAP)
                                                   ,(cdr entry-mode-map)))
                           ))
      (add-to-list ALIST MODE-MAP)
      )
    )
  )

(defun dwcB-configure (&rest args)
  (let* ((mode (plist-get args :mode))
         (base-map (plist-get args :base))
         (parent-map (plist-get args :parent))
         (gen-binds (plist-get args :gen-binds))
         (env-binds (plist-get args :env-binds))
         (wnd-binds (plist-get args :wnd-binds)))
    (unless (symbolp mode)
      (error ":mode must be a symbol"))
    (unless (or gen-binds env-binds wnd-binds)
      (error
       "Must provide general (:gen-binds), environment (:env-binds) or window (:wnd-binds) bindings."))
    (let ((dwcB-map (make-sparse-keymap))
          (build-map (lambda (map binding)
                       (define-key map (kbd (car binding)) (cdr binding)))))
      (when gen-binds
        (mapc (apply-partially (symbol-value 'build-map) dwcB-map) gen-binds)
        )
      (when wnd-binds
        (let ((wnd-map (make-sparse-keymap)))
          (mapc (apply-partially (symbol-value 'build-map) wnd-map) wnd-binds)
          (define-key dwcB-map (kbd dwcB-inter-buffer-prefix) 'wnd-map)
          )
        )
      (when env-binds
        (let ((env-map (make-sparse-keymap)))
          (mapc (apply-partially (symbol-value 'build-map) env-map) env-binds)
          (define-key dwcB-map (kbd dwcB-major-prefix) 'env-map)
          )
        )
      (when base-map
        (unless (keymapp base-map)
          (error ":base must be a keymap"))
        ;; This might be backwards. base-map might be overlappign dwcB-map, not sure
        (setq dwcB-map (make-composed-keymap base-map dwcB-map))
        )
      (if mode
          (let ((mode-map (cons mode dwcB-map)))
            (if (member mode minor-mode-list)
                (dwcB--save mode-map 'dwcB--minor-alist)
              (dwcB--save mode-map 'dwcB--major-alist))
            )
        (setq dwcB--global-map (make-composed-keymap dwcB-map dwcB--global-map))
        ))
  ))


;(require 'default-bindings)

(provide 'dwcB)


;;; dwcB.el ends here