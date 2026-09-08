;;; funcs.el --- personal-shell layer functions file for Spacemacs. -*- lexical-binding: t -*-

;;; Code:

(defun spacemacs/projectile-shell-pop ()
  "Pop-up a shell buffer at the project root.
Customize `shell-default-shell' to control what type of shell
buffer you create. This function will pop-up a full-width buffer
and move your focus to it; to switch the current buffer view, use
`spacemacs/projectile-shell'."
  (interactive)
  (let ((default-directory (projectile-acquire-root)))
    (call-interactively 'spacemacs/default-pop-shell)))

(defun spacemacs/projectile-shell ()
  "Create a shell buffer at the project root and switch to it.
Customize `shell-default-shell' to control what type of shell
buffer you create. This function switches the current buffer
view; to pop-up a full width buffer, use
`spacemacs/projectile-shell-pop'."
  (interactive)
  (pcase shell-default-shell
    ((or 'multi-term 'multi-vterm)
     (projectile-with-default-dir (projectile-acquire-root)
       (call-interactively shell-default-shell)))
    ('eat (call-interactively #'eat-project))
    (_ (call-interactively (or (intern-soft (format "projectile-run-%s" shell-default-shell))
                               #'projectile-run-shell)))))

(defun spacemacs/disable-hl-line-mode ()
  "Locally disable global-hl-line-mode"
  (interactive)
  (setq-local global-hl-line-mode nil))

(defun spacemacs/default-pop-shell ()
  "Open the default shell in a popup using `shell-pop'.
Additionally changes to working directory when the value of
`shell-pop-autocd-to-working-dir' is non-nil (default)."
  (interactive)
  (let ((shell (cl-case shell-default-shell
                 (multi-vterm 'multivterm)
                 (multi-term 'multiterm)
                 (shell 'inferior-shell)
                 (t shell-default-shell))))
    (call-interactively (intern (format "spacemacs/shell-pop-%S" shell)))))

(defun spacemacs/resize-shell-to-desired-width ()
  ;; `shell-pop--is-shell-buffer' is a buffer-local flag set by shell-pop on
  ;; managed buffers (replaces the removed `shell-pop-last-shell-buffer-name'
  ;; comparison). `bound-and-true-p' keeps this safe on shell-pop versions
  ;; that predate the rewrite.
  (when (and (bound-and-true-p shell-pop--is-shell-buffer)
             (memq shell-pop-window-position '(left right)))
    (enlarge-window-horizontally (- (/ (* (frame-width) shell-default-width)
                                       100)
                                    (window-width)))))

(defmacro make-shell-pop-command (name func &optional shell)
  "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
  `(defun ,(intern (concat "spacemacs/shell-pop-" name)) (index)
     ,(format (concat "Toggle a popup window with `%S'.\n"
                      "Multiple shells can be opened with a numerical prefix "
                      "argument. Using the universal prefix argument will "
                      "open the shell in the current buffer instead of a "
                      "popup buffer.")
              func)
     (interactive "P")
     (require 'shell-pop)
     (if (equal '(4) index)
         ;; no popup
         (,func ,shell)
       (shell-pop--set-shell-type
        'shell-pop-shell-type
        (list ,name
              ,(if (bound-and-true-p layouts-enable-local-variables)
                   `(concat "*" (spacemacs//current-layout-name) "-"
                            (if (file-remote-p default-directory)
                                "remote-"
                              "")
                            ,name "*")
                 (concat "*" name "*"))
              (lambda nil (,func ,shell))))
       (shell-pop index)
       (spacemacs/resize-shell-to-desired-width))))

(defun spacemacs//toggle-shell-auto-completion-based-on-path ()
  "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
  (if (file-remote-p default-directory)
      (setq-local company-idle-delay nil)
    (setq-local company-idle-delay auto-completion-idle-delay)))

(defun spacemacs/pcomplete-std-complete ()
  (interactive)
  (pcomplete-std-complete)
  (evil-insert-state))

;; Wrappers for non-standard shell commands
(defun multighostel (&optional _)
  "Wrapper to call ghostel with `t' to create a new ghostel frame"
  (interactive)
  (ghostel t))

;; https://stackoverflow.com/a/6839968
(defun spacemacs//inhibit-global-centered-cursor-mode ()
  "Counter-act `global-centered-cursor-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (centered-cursor-mode 0))
            :append
            :local))

;;; funcs.el ends here
