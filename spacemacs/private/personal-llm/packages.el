;;; packages.el --- personal-llm layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Joe Tague <joetague@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst personal-llm-packages
  '(embark
    gptel
    gptel-agent
    (gptel-quick :location (recipe
                            :fetcher github
                            :repo "karthink/gptel-quick"))
    (ob-gptel :location (recipe
                         :fetcher github
                         :repo "jwiegley/ob-gptel"))
    org
    window-purpose)
  "The list of Lisp packages required by the personal-llm layer.")

(defun personal-llm/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode
      :bindings
      "C-c C-c" #'gptel-context-confirm
      "C-c C-k" #'gptel-context-quit
      "RET"     #'gptel-context-visit
      "n"       #'gptel-context-next
      "p"       #'gptel-context-previous
      "d"       #'gptel-context-flag-deletion)
    ;; set up keybindings
    (spacemacs/declare-prefix "$g" "gptel")
    (spacemacs/set-leader-keys
      "$gg" 'gptel                          ; Start a new gptel session
      "$gs" 'personal-llm//gptel-send-wrapper  ; Send a message to gptel
      "$gq" 'personal-llm//gptel-abort-wrapper ; Abort any active gptel process
      "$gm" 'gptel-menu                     ; Open the gptel menu
      "$gc" 'gptel-add                      ; Add context
      "$gf" 'gptel-add-file                 ; Add a file
      "$go" 'gptel-org-set-topic            ; Set topic in Org-mode
      "$gp" 'gptel-org-set-properties       ; Set properties in Org-mode
      "$gr" 'gptel-rewrite)))               ; Rewrite or refactor test region

(defun personal-llm/init-gptel-agent ()
  "Initialize the `gptel-agent` package and set up keybindings."
  (use-package gptel-agent
    :defer t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode)
    ;; set up keybindings
    (spacemacs/set-leader-keys
      "$ga" 'gptel-agent                          ; Start a new gptel-agent session
      "$gu" 'gptel-agent-update)                  ; Updates the gptel-agent database
    ;; Config for =gptel-agent=
    :config (gptel-agent-update)))

(defun personal-llm/init-gptel-quick ()
  "Initialize gptel-quick."
  (use-package gptel-quick
    :defer t))

(defun personal-llm/init-ob-gptel ()
  "Initialize ob-gptel."
  (use-package ob-gptel
    :defer t
    :after org
    :config
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-mode)
          (personal-llm//enable-ob-gptel-capf))))))

(defun personal-llm/post-init-embark ()
  "Set up Embark keybindings for gptel."
  (with-eval-after-load 'embark
    (keymap-set embark-general-map "?" #'gptel-quick)))

(defun personal-llm/post-init-org ()
  "Set up Org-mode integration for gptel."
  (with-eval-after-load 'org
    (require 'ob-core)
    (add-to-list 'org-babel-load-languages '(gptel . t))
    (add-hook 'org-mode-hook #'personal-llm//org-mode-setup)
    (advice-add 'org-babel-execute-src-block :before #'personal-llm//ensure-ob-gptel))
  (spacemacs/declare-prefix-for-mode 'org-mode "m$g" "gptel")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "$go" 'gptel-org-set-topic
    "$gp" 'gptel-org-set-properties))

(defun personal-llm/post-init-window-purpose ()
  ;; TODO: Temporary fix to avoid the error when using window-purpose
  ;; see https://github.com/karthink/gptel/issues/237 for details
  ;; (purpose-set-extension-configuration
  ;;  :personal-llm-layer
  ;;  (purpose-conf :mode-purposes '((gptel-mode . chat))))
  (defun personal-llm/disable-purpose-mode-around-for-gptel (orig-func &rest args)
    "Advice function to disable purpose-mode before calling ORIG-FUNC with ARGS."
    (let ((purpose-mode-was-enabled (bound-and-true-p purpose-mode)))
      (when purpose-mode-was-enabled
        (purpose-mode -1))
      (unwind-protect
          (apply orig-func args)
        (when purpose-mode-was-enabled
          (purpose-mode 1)))))
  (advice-add 'gptel :around #'personal-llm/disable-purpose-mode-around-for-gptel))

;;; packages.el ends here
