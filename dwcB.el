;;; Code:

(require 'cl)
(require 'hydra)

;; FIXME -- should be generic
(add-to-list 'load-path "~/workspace/dwcB")


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
    (keyboard-translate ?\C-i ?\H-i) ; this is needed because emacs wont let you bind C-i naturally
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
  "Set the current global map to Emacs default, global-map."
  (when (eq (current-global-map) dwcB--global-map)
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

(defun read-binds (binding-config)
  "Accepts a binding configuration and converts it into a key/command alist"

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
  "Configures dwcB binding.
:key - A minor mode or major mode name to configure bindings to.
       A keymap name to which to correlate a dwcB keymap. (useful
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
       window (:wnd-binds) bindings."))

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

      ;; Add major, window, and general binds modals mappings into dwcB-map
      (mapc (apply-partially 'apply (lambda (BINDS PREFIX)
                                      "Add BINDS to dwcB-map. If PREFIX non-nil, add under PREFIX"
                                      (when BINDS
                                        (let* ((no-prefix-binds (car BINDS))
                                               (ctrl-x-binds (cadr BINDS))
                                               (map (funcall build-map no-prefix-binds)))
                                          (define-key map (kbd "C-v") )
                                          (setq dwcB-map (make-composed-keymap dwcB-map map)))
                                          ))))

            `((,gen-binds  nil)))

      (,env-binds  ,dwcB-major-prefix)
      (,wnd-binds  ,dwcB-inter-buffer-prefix)
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
              (error "parent-map must be a keymap or a known dwcB key")))
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
