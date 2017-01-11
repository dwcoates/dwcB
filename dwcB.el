;;; Code:

(require 'cl)
(require 'hydra)

;; FIXME -- should be generic
(add-to-list 'load-path "~/workspace/dwcB")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; MAPS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwcB--global-map (make-sparse-keymap)
  "alist for tracking dwcB versions of general keymaps")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; PREFIXES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dwcB-major-prefix "c"
  "The `dwc-binding' prefix used as default for major modes.
Major mode commands may be bound to keys outside of prefix (see `dwcB-add-major-mode-map').")
(defconst dwcB-general-prefix "x"
  "The `dwc-binding' prefix used as default for general Emacs editing/navigation commands.")
(defconst dwcB-inter-buffer-prefix "z"
  "The `dwc-binding' prefix used as default for inter-buffer navigation and editing.")
(defconst dwcB-secondary-prefix "v"
  "The `dwc-binding' prefix used as a secondary prefix for a given namespace (major, general, or inter-buffer).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; SAVES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwcB--primary-alist '()
  "First searched when setting local-map, then looks outside.")
(defvar dwcB--minor-alist '())
(defvar dwcB--minor-alist--saved '()
  "List of keymaps to be appended to `minor-mode-map-alist'.")
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
    (keyboard-translate ?\C-i ?\H-i) ; because emacs won't let you bind C-i naturally
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
  (when (eq (current-global-map) global-map)
    (use-global-map (make-composed-keymap dwcB--global-map global-map)))
  )

(defun dwcB--reset-global-map ()
  "Set the current global map to Emacs default, `global-map'."
  (when (eq (current-global-map) dwcB--global-map)
    (use-global-map global-map))
  )

(defun dwcB--setup-minor-maps ()
  "Configure the minor mappings."
  (setq dwcB--minor-alist--saved minor-mode-overriding-map-alist)
  (setq minor-mode-overriding-map-alist dwcB--minor-alist)
  )

(defun dwcB--teardown-minor-maps ()
  "Remove the dwcB minor maps and restore originals."
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
  "Restore origal keymap for `major-mode'."
  (let ((mode-map  (assoc major-mode dwcB--primary-alist--saved)))
    (when mode-map
      (use-local-map (cdr mode-map))
      )
    )
  )

(defun dwcB--save (mode-map alist)
  "Save MODE-MAP to ALIST.  Used for saving default keymaps indiscriminantly."
  (let ((entry-mode-map (assoc (car mode-map) (symbol-value alist))))
    (if entry-mode-map
        (add-to-list alist
                     (cons (car mode-map)
                           (make-composed-keymap `(,(cdr mode-map)
                                                   ,(cdr entry-mode-map)))
                           ))
      (add-to-list alist mode-map)
      )
    )
  )

(defun read-binds (binding-config)
  "Accept BINDING-CONFIG and convert it into a key/command alist."

  ;; needs to handle the H-i problem (peculiarity with emacs key binding architecture leaves C-i broken)
  ;; HYDRA - needs to append C-modifier set into no-modifier set for hydra
  (let ((modifiers '(("" no-modifier)
                     ("C-" C-modifier)
                     ("M-" M-modifier)
                     ("C-S-" C-S-modifier)
                     ("M-S-" M-S-modifier)
                     ("C-M-" C-M-modifier)
                     ("C-M-S-" C-M-S-modifier))))
    (apply 'append
           (mapcar (apply-partially 'apply
                                    (lambda (modifier bindings)
                                      "Add a modifier to all in a binding-list"
                                      (mapcar (lambda (bind) (cons (concat modifier (car bind))
                                                                   (cdr bind)))
                                              bindings)))
                   (remove-if-not #'cdr (mapcar (lambda (modifier)
                                                  "Produce a modifier/binding-list association"
                                                  `(,(car modifier)
                                                    ,(plist-get binding-config (cadr modifier))))
                                                modifiers)
                                                 )))
    )
  )


(defun dwcB-configure (&rest args)
  "Configure `dwcB-binding' with ARGS.
:key - A minor mode or major mode name to configure bindings to.
       A keymap name to which to correlate a dwcB keymap.  (useful
       if you want to build map hierarchies using :parent).
       Ommit to bind globally.
:base - A base map with which to compose the provided bindings.
:parent - A parent map from which the produces bindings should inherit.
:gen-binds - Bindings in the general namespace (i.e., not necessarily
             under a prefix key)
:env-binds - Bindings under the major prefix
:wnd-binds - Bindings under the window prefix"

  (let* ((key (plist-get args :key))
         (base-map (plist-get args :base))
         (parent-map (plist-get args :parent))
         (clean-bind-conf (lambda (config)
                            "Read a binding config (normal and C-x subprefix) and return results"
                            `(,(read-binds (cdr config)) ,(read-binds (cdr config)))))
         (gen-binds (funcall clean-bind-conf (plist-get args :gen-binds)))
         (env-binds (funcall clean-bind-conf (plist-get args :env-binds)))
         (wnd-binds (funcall clean-bind-conf (plist-get args :wnd-binds))))
    (unless (symbolp key)
      (error ":key must be a symbol"))
    (unless (symbolp parent-map)
      (error ":key must be a symbol"))
    (unless (or (or gen-binds env-binds wnd-binds) (and key (or parent-map base-map)))
      (error
       "Must provide general (:gen-binds), environment (:env-binds) or
       window (:wnd-binds) bindings"))

    ;; build the gen-binds straight into the dwcB-map
    ;; apply each of the binds (including gen-binds) to defhydra into dwcB-map
    ;; in other words, all prefixing is now done via defhydra
    ;; each of these apply's will have to define the other shiftors to call a function that kills
    ;; hydra and then starts that shiftors hydra as a new isntance


    ; gen-binds + de-shifted C-binds get added to a hydra via dwcB-map
    ; env-binds get added to a hydra via dwcB-map
    ; wnd-binds get added to a hydra via dwcB-map


    (let* ((dwcB-map (make-sparse-keymap))
           (build-map (lambda (BINDS)
                        "Create a map from binding configuration BINDS and return it"
                        (let ((map (make-sparse-keymap)))
                          (mapc (lambda (binding)
                                  (define-key map (kbd (car binding)) (cdr binding)))
                                BINDS)
                          map)))
           )
      ; gen-binds shift mappings into dwcB-map
      (set dwcB-map (make-composed-keymap (funcall build-map (car gen-binds)) dwcB-map))
      (define-key dwcB-map (kbd "C-v") (funcall build-map (cadr gen-binds)))


      ;; --------------------------------------------------------------------------------------------
      ;; those three hydra versions for the binds then need to be mapped into the three global hydras
      ;; for each of the binds. e.g., the (none ctrl alt) versions attached to their corresponding keys
      ;; ("c"    ; the complete version. Has no-modifier, C-modifier, M-modifier.
      ;;         ; Equivalent to the env global hydra.
      ;;         ; Have capital C be equal to next hydra. This would be a future feature.
      ;;  "C-c"  ; C-modifier is now a no-modifier. No direct access to former no-modifier or M-modifier.
      ;;  "M-c"  ; M-modifier is now a no-modifier. No direct access to former no-modifier or C-modifier.
      ;;  )
      ;;
      ;; Each of the three hydra versions would feature these hydra attachments. THIS IS OKAY TO DO,
      ;; as per my scratch work:
      ; (defhydra my-up-hydra (global-map "<f3>")
      ;   ("x"  my-up-hydra/body "inner"))  ; my-up-hydra has an entry for itself. Works as expected.
      ;; --------------------------------------------------------------------------------------------


      ;; --------------------------------------------------------------------------------------------
      ;; Pseudo sample hydra:
      ;(defhydra env-hydra ()
      ;  (car (unpack env-binds))
      ;  ;; build and insert secondary env-binds here
      ;
      ;  ;; access other modals
      ;  (unpack wnd-hydra-versions)
      ;  (unpack env-hydra-versions)
      ;  (unpack gen-hydra-versions)
      ; )
      ;;
      ;; Pseudo sample for hydra versions
      ; (set env-hydra-versions
      ;  (dwcB-major-prefix env-hydra/body)
      ;  ((concat "C-" dwcB-major-prefix) env-hydra-ctrl/body)
      ;  ((concat "M-" dwcB-major-prefix) env-hydra-meta/body))
      ;; --------------------------------------------------------------------------------------------


      ;; --------------------------------------------------------------------------------------------
      ;; NOTE: It will have to save the binds that are used so that it can handle updated hydras by
      ;;       concatenating the saved bind and the new bind and using that to reset the hydra. UNLESS
      ;;       Hydra happens to already handle upgrades naturally.
      ;; -----------------------------------------------
      ;; NOTE: It has to handle the hydra names properly. Can't use generic hydra names, must name
      ;;       them according to the associated major-mode, for env-binds and secondary env-binds.
      ;;       If it doesn't, all env-hydras would overwrite eachother.
      ;; --------------------------------------------------------------------------------------------


      ;; If a base map, add those binds to dwcB-map
      (when base-map
        (unless (keymapp base-map)
          (error ":base must be a keymap"))
        (setq dwcB-map (make-composed-keymap dwcB-map base-map))
        )
      ;; Inherit dwcB-map from parent-map if it exists
      (when parent-map
        (if (and (boundp parent-map) (keymapp (symbol-value parent-map)))
            (set-keymap-parent dwcB-map (symbol-value parent-map))
          (let ((entry (assoc parent-map dwcB--primary-alist)))
            (if entry
                (set-keymap-parent dwcB-map (cdr entry))
              (error "`:parent' arg must be a keymap or a known dwcB key")))
          ))
      ;; Decide when this dwcB-map goes into effect (globally, during a certain mode, etc)
      (if key
          (let ((mode-map-assoc (cons key dwcB-map)))
            (if (member key minor-mode-list)
                (dwcB--save mode-map-assoc 'dwcB--minor-alist)
              (dwcB--save mode-map-assoc 'dwcB--primary-alist)))
        (setq dwcB--global-map (make-composed-keymap dwcB-map dwcB--global-map))
        )
      ))
  )


;(require 'default-bindings)

(provide 'dwcB)


;;; dwcB.el ends here
