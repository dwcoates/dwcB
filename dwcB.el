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

(defun dwcB--update-maps ()
  "If dwcB-mode is on, update minor maps, major map, and global map to be dwcB versions. Else, set maps to default states."
  (mapc
   (lambda ()
     (if dwcB-mode
         (setq) ;; return function that sets
       ()
       )
     )
   (append dwcB-mode-alist dwcB-keymap-alist)
   )
  (dwcB--update-major-map)
  )

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

    (let ((dwcB-map (make-sparse-keymap)))
      (when gen-binds
        (mapc (lambda (binding)
                (apply (apply-partially 'define-key dwcB-map) binding))
              gen-binds))
      (when base-map
        (setq dwcB-map (append base-map dwcB-map))
        (dwcB-add-to-keymap-alist (cons base-map dwcB-map) dwcB-keymap-alist)
        ;; set parent of dwcB-map to be base-map's parent's dwcB version,
        ;; otherwise simply base-map's parent.
        (set-keymap-parent dwcB-map (or
                                     (cdr (assoc (keymap-parent base-map)
                                                 dwcB-keymap-alist))
                                     (keymap-parent (keymap-parent base)))))
      (when mode
        ;; minor-mode
        ;; major-mode
        ))))

(require 'default-bindings)


(provide 'dwcB)
;;; dwcB.el ends here
