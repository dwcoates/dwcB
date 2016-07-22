;;; Code:

;; temp
(add-to-list 'load-path "~/workspace/dwcB")

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
;; Create
(defconst dwcB-yank-key "f")
(defconst dwcB-insert-key "v")
;; Transpose
(defconst dwcB-transpose-key "t")
;; Undo/Redo
(defconst dwcB-undo-key "r")
(defconst dwcB-redo-key "t")
;; Query
(defconst dwcB-query-key "?")
(defconst dwcB-description-key "/")
;; Compile/Interpret
(defconst dwcB-interpret-key ":")
(defconst dwcB-compile-key ";")

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

(defvar dwcB--primary-alist '()
  "first searched when setting local-map. Then looks outside")
(defvar dwcB--minor-alist '())
(defvar dwcB--minor-alist--saved '() "list of keymaps to be appended to minor-mode-map-alist")
(defvar dwcB--primary-alist--saved '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; DWCB ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode dwcB-mode
  "Organized alternative bindings to Emacs defaults."
  :lighter " dwcB"
  :global t
  (if dwcB-mode
      (dwcB--setup)
    (dwcB--teardown))
  )

(defun dwcB--setup ()
  (progn
    (keyboard-translate ?\C-i ?\H-i)
    (dwcB--set-global-map)
    (add-hook 'after-change-major-mode-hook 'dwcB--major-mode-update)
    (dwcB--major-mode-update)
    ;; added as workaround to helm's wild and whacky handing of keymaps. Oh well.
    (add-hook 'post-command-hook 'dwcB--setup-minor-maps)
    ))

(defun dwcB--teardown ()
  (progn
    (keyboard-translate ?\C-i ?\C-i)
    (dwcB--reset-global-map)
    (remove-hook 'after-change-major-mode-hook 'dwcB--major-mode-update)
    (dwcB--major-mode-clear)
    (remove-hook 'post-command-hook 'dwcB--setup-minor-maps)
    (dwcB--teardown-minor-maps)
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

(defun dwcB--setup-minor-maps ()
  (setq dwcB--minor-alist--saved minor-mode-overriding-map-alist)
  (setq minor-mode-overriding-map-alist dwcB--minor-alist)
  )

(defun dwcB--teardown-minor-maps ()
  (setq minor-mode-overriding-map-alist dwcB--minor-alist--saved)
  )

(defun dwcB--major-mode-update ()
  "Switch to corresponding dwcB keymap for the current major mode."
  (let ((mode-map (assoc major-mode dwcB--primary-alist)))
    (when mode-map
      (dwcB--save (cons major-mode (current-local-map)) 'dwcB--primary-alist--saved)
      (use-local-map (cdr mode-map))
      ))
  )

(defun dwcB--major-mode-clear ()
  (let ((mode-map  (assoc major-mode dwcB--primary-alist--saved)))
    (when mode-map
      (use-local-map (cdr mode-map))
      )
    )
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
  "Configures dwcB binding.
:key - not provided to map to dwcB global map. A minor mode or major
       mode name to configure bindings for a minor or major mode. A
       keymap name to define a dwcB keymap behind the scenes (useful
       if you want to build map hierarchies using :parent).
:base - A base map with which to compose the provided bindings
:parent - A parent map from which the produces bindings should inherit
:gen-binds - Bindings in the general namespace (i.e., not necessarily
             under a prefix key)
:env-binds - Bindings under the major prefix :wnd-binds - Bindings
             under the window prefix"
  (let* ((key (plist-get args :key))
         (base-map (plist-get args :base))
         (parent-map (plist-get args :parent))
         (gen-binds (plist-get args :gen-binds))
         (env-binds (plist-get args :env-binds))
         (wnd-binds (plist-get args :wnd-binds)))
    (unless (symbolp key)
      (error ":key must be a symbol"))
    (unless (symbolp parent-map)
      (error ":key must be a symbol"))
    (unless (or (or gen-binds env-binds wnd-binds)
                (and key (or parent-map base-map)))
      (error
       "Must provide general (:gen-binds), environment (:env-binds) or
       window (:wnd-binds) bindings."))
    (let ((dwcB-map (make-sparse-keymap))
          (build-map (lambda (map binding)
                       (define-key map (kbd (car binding)) (cdr binding)))))
      (when gen-binds
        (mapc (apply-partially build-map dwcB-map) gen-binds)
        )
      (when wnd-binds
        (let ((wnd-map (make-sparse-keymap)))
          (mapc (apply-partially build-map wnd-map) wnd-binds)
          (define-key dwcB-map (kbd dwcB-inter-buffer-prefix) wnd-map)
          )
        )
      (when env-binds
        (let ((env-map (make-sparse-keymap)))
          (mapc (apply-partially build-map env-map) env-binds)
          (define-key dwcB-map (kbd dwcB-major-prefix) env-map)
          )
        )
      (when base-map
        (unless (keymapp base-map)
          (error ":base must be a keymap"))
        (setq dwcB-map (make-composed-keymap dwcB-map base-map))
        )
      (when parent-map
        (if (and (boundp parent-map) (keymapp (symbol-value parent-map)))
            (set-keymap-parent dwcB-map (symbol-value parent-map))
          (let ((entry (assoc parent-map dwcB--primary-alist)))
            (if entry
                (set-keymap-parent dwcB-map (cdr entry))
              (error "parent-map must be a keymap or a known dwcB key")))
          ))

      (if key
          (let ((mode-map (cons key dwcB-map)))
            (if (member key minor-mode-list)
                (dwcB--save mode-map 'dwcB--minor-alist)
              (dwcB--save mode-map 'dwcB--primary-alist)))
        (setq dwcB--global-map (make-composed-keymap dwcB-map dwcB--global-map))
        ))
  ))


(require 'default-bindings)

(provide 'dwcB)


;;; dwcB.el ends here
