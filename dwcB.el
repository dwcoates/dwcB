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

(defvar dwcB--global-map (copy-keymap global-map) "alist for tracking dwcB versions of general keymaps")
(defvar dwcB--inter-buffer-map (make-sparse-keymap)
  "Prefix map for window, buffer, and frame commands.")

(defvar dwcB--major-alist '() "first searched when setting local-map. Then looks outside")
(defvar dwcB--major-alist--saved '() "list of tuples (mode, map) to be restored")
(defvar dwcB--minor-alist--saved '() "list of keymaps to be appended to minor-mode-map-alist")


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwcB--global-map (kbd (concat "C-" dwcB-backward-key)) 'backward-char)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-forward-key)) 'forward-char)
(define-key dwcB--global-map (kbd (concat "H-" dwcB-upward-key)) 'previous-line)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-downward-key)) 'next-line)
;
(define-key dwcB--global-map (kbd (concat "M-" dwcB-backward-key)) 'backward-word)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-forward-key)) 'forward-word)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-downward-key)) 'forward-paragraph)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-upward-key)) 'backward-paragraph)
;
(define-key dwcB--global-map (kbd (concat "C-S-" dwcB-forward-key)) 'scroll-left)
(define-key dwcB--global-map (kbd (concat "C-S-" dwcB-backward-key)) 'scroll-right)
(define-key dwcB--global-map (kbd (concat "C-S-" dwcB-upward-key)) 'scroll-down-command)
(define-key dwcB--global-map (kbd (concat "C-S-" dwcB-downward-key)) 'scroll-up-command)
;; Beginning/End
(define-key dwcB--global-map (kbd (concat "C-M-" dwcB-upward-key)) 'scroll-other-window-down)
(define-key dwcB--global-map (kbd (concat "C-M-" dwcB-downward-key)) 'scroll-other-window)
;
(define-key dwcB--global-map (kbd (concat "C-" dwcB-beginning-key)) 'beginning-of-line)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-end-key)) 'end-of-line)
;
(define-key dwcB--global-map (kbd (concat "M-" dwcB-end-key)) 'forward-to-indentation)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-beginning-key)) 'backward-to-indentation)
;
(define-key dwcB--global-map (kbd (concat "C-M-" dwcB-beginning-key)) 'beginning-of-buffer)
(define-key dwcB--global-map (kbd (concat "C-M-" dwcB-end-key)) 'end-of-buffer)
;; Bigger/Smaller
(define-key dwcB--global-map (kbd (concat "C-" dwcB-smaller-key)) 'narrow-to-region)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-smaller-key)) 'narrow-to-page)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-bigger-key)) 'widen)
;; Search
(define-key dwcB--global-map (kbd (concat "C-" dwcB-search-alpha-key)) 'isearch-forward)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-search-beta-key)) 'isearch-backward)
;; Misc
(define-key dwcB--global-map (kbd "C-'") 'recenter-top-bottom)

;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwcB--global-map (kbd (concat "C-S-M-" dwcB-upward-key)) 'upcase-region)
(define-key dwcB--global-map (kbd (concat "C-S-M-" dwcB-downward-key)) 'downcase-region)
;; Kill
(define-key dwcB--global-map (kbd (concat "C-" dwcB-kill-big-key)) 'kill-line)
(define-key dwcB--global-map (kbd (concat "C-" dwcB-kill-or-save-key)) 'kill-region)
;
(define-key dwcB--global-map (kbd (concat "M-" dwcB-kill-big-key)) 'kill-sentence)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-kill-or-save-key)) 'kill-ring-save)
;
(define-key dwcB--global-map (kbd "C-z") 'zap-to-char)
;; Whitespace
(define-key dwcB--global-map (kbd "C-v") 'delete-horizontal-space)
(define-key dwcB--global-map (kbd "M-v") 'delete-indentation)
;; Transpose
(define-key dwcB--global-map (kbd (concat "C-" dwcB-transpose-key)) 'transpose-chars)
(define-key dwcB--global-map (kbd (concat "M-" dwcB-transpose-key)) 'transpose-words)
;; Yank
(define-key dwcB--global-map (kbd (concat "C-" dwcB-yank-key)) 'yank)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; INTER-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-key dwcB--inter-buffer-map (kbd dwcB-forward-key) 'next-buffer)
(define-key dwcB--inter-buffer-map (kbd dwcB-backward-key) 'previous-buffer)
;
(define-key dwcB--inter-buffer-map (kbd "U") 'switch-to-buffer-other-window)
(define-key dwcB--inter-buffer-map (kbd "u") 'switch-to-buffer)
(define-key dwcB--inter-buffer-map (kbd "C-u") 'helm-buffers-list)
;
(define-key dwcB--inter-buffer-map (kbd "o") 'other-frame)
(define-key dwcB--inter-buffer-map (kbd "O") 'other-window)
;; DIRED
(define-key dwcB--inter-buffer-map (kbd "/") 'dired-jump)
(define-key dwcB--inter-buffer-map (kbd "?") 'dired-jump-other-window)
;; FIND FILE
(define-key dwcB--inter-buffer-map (kbd dwcB-search-alpha-key) 'ido-find-file)
(define-key dwcB--inter-buffer-map (kbd (concat "C-" dwcB-search-alpha-key)) 'ido-find-file-other-window)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-search-alpha-key)) 'ido-find-file-other-frame)
;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; RESIZE
(define-key dwcB--inter-buffer-map (kbd dwcB-bigger-key) 'enlarge-window-horizontally)
(define-key dwcB--inter-buffer-map (kbd dwcB-smaller-key) 'shrink-window-horizontally)
(define-key dwcB--inter-buffer-map (kbd (concat "C-" dwcB-bigger-key)) 'enlarge-window)
(define-key dwcB--inter-buffer-map (kbd (concat "M-" dwcB-smaller-key)) 'shrink-window)
;
(define-key dwcB--inter-buffer-map (kbd "b") 'balance-windows)
(define-key dwcB--inter-buffer-map (kbd "B") 'shrink-window-if-larger-than-buffer)
(define-key dwcB--inter-buffer-map (kbd "v") 'golden-ratio)
(define-key dwcB--inter-buffer-map (kbd "V") 'golden-ratio-mode)
;
(define-key dwcB--inter-buffer-map (kbd "z") 'suspend-frame)
;; SPLIT
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-forward-key)) 'split-window-right)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-backward-key)) 'split-horizontally)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-upward-key)) 'split-window-vertically)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-downward-key)) 'split-window-below)
;; KILL
(define-key dwcB--inter-buffer-map (kbd dwcB-kill-element-key) 'kill-buffer-and-window)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-kill-element-key)) 'kill-some-buffers)
(define-key dwcB--inter-buffer-map (kbd (concat "C-" dwcB-kill-element-key)) 'kill-buffer)
;
(define-key dwcB--inter-buffer-map
  (kbd (concat "3 " (upcase dwcB-kill-element-key))) 'delete-other-frames)
(define-key dwcB--inter-buffer-map (kbd (concat "3 " dwcB-kill-element-key)) 'delete-frame)
;; SAVE
(define-key dwcB--inter-buffer-map (kbd (concat "C-" dwcB-kill-or-save-key)) 'save-buffer)
(define-key dwcB--inter-buffer-map (kbd (upcase dwcB-kill-or-save-key)) 'write-file)
(define-key dwcB--inter-buffer-map (kbd (concat "M-" dwcB-kill-or-save-key)) 'save-some-bueffer)


(defconst dwcB-major-prefix "C-e"
  "dwc-binding prefix used as default for major modes. Major mode commands may be
bound to keys outside of prefix (see dwcB-add-major-mode-map).")
(defconst dwcB-inter-buffer-prefix "C-w"
  "dwc-binding prefix used as default for inter-buffer navigation and editing.")

(define-key dwcB--global-map (kbd dwcB-inter-buffer-prefix) dwcB--inter-buffer-map)

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
    ;;     (add-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
    ))

(defun dwcB--teardown ()
  (progn
    (keyboard-translate ?\C-i ?\C-i)
    (dwcB--reset-global-map)
    ;;   (remove-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
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

;; (defun dwcB--deactivate-minor-map (MINOR-MODE)
;;   (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
;;     (if (not (equal mode-map nil))
;;         (assq-delete-all (car mode-map) minor-mode-overriding-map-alist)
;;       (error "Minor mode %s has no dwcB binding."))
;;     )
;;   )

;; (defun dwcB--update-major-map ()
;;   "When dwcB-mode is on, switch to corresponding dwcB keymap for the current major
;; mode. Else, set current local map to default map for major mode."
;;   (let (mode-maps (cdr (dwcB-get-keymaps major-mode dwcB-mode-alist)))
;;     (when mode-maps
;;         (if dwcB-mode
;;             (use-local-map (cdr mode-maps))  ; set to major-mode dwcB keymap
;;           (use-local-map (car mode-maps))))  ; reset to major-mode original keymap
;;     ))

;; (defun dwcB--add-to-keymap-alist (MAP-ASSOC DWCB-ALIST)
;;   "If (assoc MAP-ASSOC DWCB-ALIST) return false, add MAP-ASSOC to DWCB-ALIST.
;; Else, merge keymaps into DWCB-ALIST's. Return cdr of DWCB-ALIST's new association."
;;   ;; Place holder code...
;;   (let ((dwcB-map (cdr (cdr MAP-ASSOC))))
;;     (if (assoc (car MAP-ASSOC) DWCB-ALIST)
;;         (setf (cdr DWCB-ALIST) dwcB-map)
;;       (push 'dwcB-keymap-alist (cons base-map dwcB-map))))
;;     )


;; (defun dwcB-configure (&rest args)
;;   (let* ((mode (plist-get args :mode))
;;          (base-map (plist-get args :base))
;;          (parent-map (plist-get args :parent))
;;          (gen-binds (plist-get args :gen))
;;          (env-binds (plist-get args :env))
;;          (wnd-binds (plist-get args :wnd)))
;;     (unless (symbolp mode)
;;       (error ":mode must be a symbol"))
;;     (unless (or gen-binds env-binds wnd-binds)
;;       (error
;;        "Must provide general (:gen-binds), environment (:env-binds) or window
;; (:wnd-binds) bindings."))

;;     (let ((dwcB-map (make-sparse-keymap))
;;           (build-map (lambda (map binding)
;;                        (define-key map (kbd (car binding)) (cdr binding)))))
;;       (when gen-binds
;;         (mapc (apply-partially 'build-map dwcB-map) gen-binds)
;;         )
;;       (when wnd-binds
;;         (let ((wnd-map (make-sparse-map)))
;;           (mapc (apply-partially 'build-map wnd-map) wnd-binds)
;;           (define-key dwcB-map (kbd inter-buffer-prefix) wnd-map))
;;         )
;;       (when env-binds
;;         (let ((env-map (make-sparse-map)))
;;           (mapc (apply-partially 'build-map env-map) env-binds)
;;           (define-key dwcB-map (kbd env-prefix) env-map))
;;         )
;;       (when base-map
;;         (unless (and (symbolp base-map) (keymapp base-map))
;;           (error ":base must be a keymap"))
;;         (let* ((entry (assoc dwcB-keymap-alist base-map))
;;                (parent (cdr (assoc dwcB-keymap-alist (keymap-parent base-map))))
;;                )
;;           (if entry
;;               (setq (cdr entry) (make-composed-keymap dwcB-map base-map))
;;               )
;;           (setq dwcB-map (make-composed-keymap dwcB-map base-map)))
;;       (when mode

;;         )
;;       )
;;     ))

;; (require 'default-bindings)


(provide 'dwcB)


;;; dwcB.el ends here
