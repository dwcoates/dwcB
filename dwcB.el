(defconst dwcB-major-prefix "C-e"
  "dwc-binding prefix used as default for major modes. Major mode commands may be bound to keys outside of prefix (see dwcB-add-major-mode-map).")
(defconst dwcB-inter-buffer-prefix "C-w"
  "dwc-binding prefix used as default for inter-buffer navigation and editing.")


(defvar dwcB-global-map (let ((map (make-sparse-keymap))) (set-keymap-parent map global-map) map)
  "The dwc-bindings global map. Should contain inter- and general intra-buffer binds. Inherits from global-map.")
(defvar dwcB-inter-buffer-map (define-key dwcB-global-map (kbd dwcB-inter-buffer-prefix) (make-sparse-keymap))
  "dwcB keymap dedicated to inter-buffer editing and navigation. All binds under prefix key dwcB-inter-buffer-prefix")


(defvar dwcB-mode-alist '() "alist for tracking dwcB versions of major- and minor-mode keymaps")
(defvar dwcB-original-major-mode-alist '() "alist for remembering original major-mode keymaps.")



(define-minor-mode dwcB-mode
  "Organized alternative bindings to Emacs defaults."
  :lighter " dwcB"
  :global t
  :keymap
  (if dwcB-mode
      (progn
        (dwcB--set-global-map)                                               ; set global map
        ;(mapc 'dwcB-activate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; set minor maps
        ;(add-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
        ;(dwcB-update-major-map)
        )
    (dwcB--reset-global-map)                                               ; unset global map
    ;(mapc 'dwcB-deactivate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; unset minor maps
    ;(remove-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
    ;(dwcB-update-major-map)
    )
  )



(defun dwcB--reset-global-map ()
  "Set the current global map to Emacs default, global-map."
  (if (eq dwcB-global-map (current-global-map))
      (use-global-map global-map))
  )

(defun dwcB--set-global-map ()
  "Set the current global map to dwcB-global-map."
  (if (eq (current-global-map) global-map)
      (use-global-map dwcB-global-map))
  )


(defun dwcB-create-map (KEYMAP BASE &optional PARENT)
  "Create a new keymap composed of KEYMAP and BASE, and that inherits from PARENT. If PARENT is nil,
created map will not inherit any bindings."
  (let ((dwcB-keymap (or (append KEYMAP BASE) (make-sparse-keymap))))
    (set-keymap-parent dwcB-keymap PARENT)
    dwcB-keymap
    )
  )


(defun dwcB-activate-minor-map (MINOR-MODE)
  (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
    (if (not (equal mode-map nil))
        (add-to-list 'minor-mode-overriding-map-alist mode-map)
      (message "No dwcB-minor-mode-map for %s; Must be first added with dwcB-add-minor-map"
              MINOR-MODE)
      )
    (cdr mode-map)
    )
  )

(defun dwcB-deactivate-minor-map (MINOR-MODE)
  (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
    (if (not (equal mode-map nil))
        (assq-delete-all (car mode-map) minor-mode-overriding-map-alist)
      (error "Minor mode %s has no dwcB binding."))
    )
  )

(defun dwcB-update-major-map ()
  "When dwcB-mode is on, switch to corresponding dwcB keymap for the current major mode."
    (let ((mode-map (assoc major-mode dwcB-major-mode-alist)))
      (if (not (equal mode-map nil))
          (if dwcB-mode
              (use-local-map (cdr (cdr mode-map)))  ; set to major-mode dwcB keymap
            (use-local-map (car (cdr mode-map)))    ; reset to major-mode original keymap
            ))
      )
    )

(defun dwcB-add-to-keymap-alist (MAP-ASSOC DWCB-ALIST)
  "If (assoc MAP-ASSOC DWCB-ALIST) return false, add MAP-ASSOC to DWCB-ALIST.
Else, merge keymaps into DWCB-ALIST's. Return cdr of DWCB-ALIST's new association."
  ;; Place holder code...
  (let ((dwcB-map (cdr MAP-ASSOC)))
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
      (error "Must provide general (:gen-binds), environment (:env-binds) or window (:wnd-binds) bindings "))

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
