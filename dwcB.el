(defconst dwcB-major-prefix "C-e"
  "dwc-binding prefix used as default for major modes. Major mode commands may be bound to keys outside of prefix (see dwcB-add-major-mode-map).")
(defconst dwcB-inter-buffer-prefix "C-w"
  "dwc-binding prefix used as default for inter-buffer navigation and editing.")


(defvar dwcB-mode-alist '()
  "alist for tracking dwcB versions of keymaps associated with major and minor modes")
(defvar dwcB-keymap-alist '() "alist for tracking dwcB versions of general keymaps")

(define-minor-mode dwcB-mode
  "Organized alternative bindings to Emacs defaults."
  :lighter " dwcB"
  :global t
  :keymap
  (if dwcB-mode
      (add-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
    (remove-hook 'after-change-major-mode-hook 'dwcB-update-major-map))
  )

(defun dwcB--get-keymaps (MAP MAP-ALIST)
  "return the maps consing associated with MAP in MAP-ALIST.
E.g., (assoc org-mode dwcB-mode-alist) returns the consing of the default org-mode map and the dwcB org-mode map."
  (cdr (assoc MAP MAP-ALIST)))

(defun dwcB--set-global-map ()
  "Set the current global map to dwcB global map."
  (if (eq (current-global-map) global-map)
      (use-global-map (cdr (dwcB--get-keymaps global-map dwcB--keymap-alist))))
  )

(defun dwcB--reset-global-map ()
  "Set the current global map to Emacs default, global-map."
  (if (eq (cdr (dwcB--get-keymaps global-map dwcB--keymap-alist))
          (current-global-map))
      (use-global-map global-map)
    ))


(defun dwcB--deactivate-minor-map (MINOR-MODE)
  (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
    (if (not (equal mode-map nil))
        (assq-delete-all (car mode-map) minor-mode-overriding-map-alist)
      (error "Minor mode %s has no dwcB binding."))
    )
  )

(defun dwcB--update-major-map ()
  "When dwcB-mode is on, switch to corresponding dwcB keymap for the current major mode. Else, set current local map to default map for major mode."
  (let (mode-maps (cdr (dwcB-get-keymaps major-mode dwcB-mode-alist)))
    (when mode-maps
        (if dwcB-mode
            (use-local-map (cdr mode-maps))  ; set to major-mode dwcB keymap
          (use-local-map (car mode-maps))))  ; reset to major-mode original keymap
    ))

(defun dwcB--add-to-keymap-alist (MAP-ASSOC DWCB-ALIST)
  "If (assoc MAP-ASSOC DWCB-ALIST) return false, add MAP-ASSOC to DWCB-ALIST.
Else, merge keymaps into DWCB-ALIST's. Return cdr of DWCB-ALIST's new association."
  ;; Place holder code...
  (let ((dwcB-map (cdr (cdr MAP-ASSOC))))
    (if (assoc (car MAP-ASSOC) DWCB-ALIST)
        (setf (cdr DWCB-ALIST) dwcB-map)
      (push 'dwcB-keymap-alist (cons base-map dwcB-map))))
    )


(defun dwcB-configure (&rest args)
  (let* ((mode (plist-get args :mode))
         (base-map (plist-get args :base))
         (parent-map (plist-get args :parent))
         (gen-binds (plist-get args :gen))
         (env-binds (plist-get args :env))
         (wnd-binds (plist-get args :wnd)))
    (unless (symbolp mode)
      (error ":mode must be a symbol"))
    (unless (or gen-binds env-binds wnd-binds)
      (error
       "Must provide general (:gen-binds), environment (:env-binds) or window (:wnd-binds) bindings."))

    (let ((dwcB-map (make-sparse-keymap))
          (build-map (lambda (map binding)
                       (define-key map (kbd (car binding)) (cdr binding)))))
      (when gen-binds
        (mapc (apply-partially 'build-map dwcB-map) gen-binds)
        )
      (when wnd-binds
        (let ((wnd-map (make-sparse-map)))
          (mapc (apply-partially 'build-map wnd-map) wnd-binds)
          (define-key dwcB-map (kbd inter-buffer-prefix) wnd-map))
        )
      (when env-binds
        (let ((env-map (make-sparse-map)))
          (mapc (apply-partially 'build-map env-map) env-binds)
          (define-key dwcB-map (kbd env-prefix) env-map))
        )
      (when base-map
        (unless (and (symbolp base-map) (keymapp base-map))
          (error ":base must be a keymap"))
        (let* ((entry (assoc dwcB-keymap-alist base-map))
               (parent (cdr (assoc dwcB-keymap-alist (keymap-parent base-map))))
               )
          (if entry
              (setq (cdr entry) (make-composed-keymap dwcB-map base-map))
              )
          (setq dwcB-map (make-composed-keymap dwcB-map base-map)))
      (when mode

        )
      )
    ))

(require 'default-bindings)


(provide 'dwcB)
;;; dwcB.el ends here
