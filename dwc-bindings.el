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
  (if dwcB-mode
      (progn
        (dwcB--set-global-map)                                              ; set global map
        (mapc 'dwcB-activate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; set minor maps
        (add-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
        (dwcB-update-major-map)
        )
    (dwcB--reset-global-map)                                              ; unset global map
    (mapc 'dwcB-deactivate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; unset minor maps
    (remove-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
    (dwcB-update-major-map)
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
  (let* ((major-mode (plist-get args :major-mode))
         (minor-mode (plist-get args :minor-mode))
         (mode (if (not (and major-mode minor-mode))
                   (or major-mode minor-mode)
                 (error "Bindings may belong to either a major mode or a minor mode, but not both")
                 ))
         ;; needs to make sure it is a symbol. If it is a string, convert it to a symbol.
         (base-map (plist-get args :base-map))
         (general-binds (plist-get args :general-binds))
         (env-binds (plist-get args :environment-binds))
         )
    (unless (or major-mode minor-mode base-map)
      (error "Must be provide a base keymap (:base-keymap) or a mode (:major-mode, :minor-mode) or both"))
    (unless (or general-binds env-binds)
      (error "Must provide general (:general-binds) or environment (:environment-binds) bindings"))

    (let ((dwcB-map (make-sparse-keymap)))
      ;; if there's a designated base-map, update dwcB-keymap-alist and set dwcB parent accordingly
      (when base-map
        (setq dwcB-map (append base-map dwcB-map))
        (dwcB-add-to-keymap-alist (cons base-map dwcB-map) dwcB-keymap-alist)
        ;; set parent of dwcB-map to be base-map's parent's dwcB version, otherwise simply base-map's parent.
        (set-keymap-parent dwcB-map (or (cdr (assoc (keymap-parent base-map) dwcB-keymap-alist))
                                        (keymap-parent (keymap-parent base)))))
      (when mode
        (dwcB-add-to-mode-alist (cons mode dwcB-map) dwcB-mode-alist)
        )
      ))
  )


(require 'default-bindings)


(provide 'dwc-bindings)
