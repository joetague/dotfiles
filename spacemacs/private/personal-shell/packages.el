;;; packages.el --- personal-shell layer packages file for Spacemacs. -*- lexical-binding: t -*-

;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
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

;; The `shell' layer owns `ghostel' and `evil-ghostel' as of upstream
;; a10be2cb1, so this layer defines no `init-' functions: ownership is
;; `(fboundp personal-shell/init-<pkg>)', and declaring one here would
;; shadow the upstream setup instead of extending it.  No `:toggle'
;; either -- the last declared toggle wins, which would override
;; `shell-enable-ghostel-support'.
;; (defconst personal-shell-packages
;;   '(ghostel
;;     hungry-delete)
;;   "The list of Lisp packages required by the personal-shell layer.")

(defconst personal-shell-packages
  '(
    evil-collection
    magit
    org
    projectile
    terminal-here
    window-purpose
    shell-pop
    ghostel
    evil-ghostel
    ))

(defun personal-shell/pre-init-evil-collection ()
  (add-to-list 'spacemacs-evil-collection-allowed-list 'evil-ghostel))

(defun personal-shell/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-init
    (defalias 's 'magit-status)))

(defun personal-shell/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ghostel . t))))

(defun personal-shell/post-init-projectile ()
  (spacemacs/set-leader-keys
    "p'" #'spacemacs/projectile-shell-pop
    "p$" #'spacemacs/projectile-shell))

(defun personal-shell/init-terminal-here ()
  (use-package terminal-here
    :defer t
    :commands (terminal-here-launch terminal-here-project-launch)
    :init
    (spacemacs/register-repl 'terminal-here 'terminal-here)
    (spacemacs/set-leader-keys
      "\"" 'terminal-here-launch
      "p \"" 'terminal-here-project-launch)))

(defun personal-shell/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :init
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-size     shell-default-height
          shell-pop-term-shell      shell-default-term-shell
          shell-pop-full-span       shell-default-full-span)

    (let* ((initial-shell-mode-name (format "%S-mode" shell-default-shell))
           (initial-shell-mode (intern initial-shell-mode-name)))
      (evil-set-initial-state initial-shell-mode 'insert))

    (when (fboundp 'spacemacs/make-variable-layout-local)
      (spacemacs/make-variable-layout-local 'shell-pop-last-shell-buffer-index 1))

    (spacemacs/set-leader-keys
      "'"   'spacemacs/default-pop-shell)
    (spacemacs/declare-prefix "'" "open shell")))

(defun personal-shell/init-ghostel ()
  (use-package ghostel
    :defer t
    :commands (ghostel ghostel-other-window)
    :init
    (make-shell-pop-command "ghostel" ghostel)
    (spacemacs/set-leader-keys "atsg" 'spacemacs/shell-pop-ghostel)
    (spacemacs/register-repl 'ghostel 'ghostel)
    :config
    (setq ghostel-shell shell-default-term-shell)
    (add-hook 'ghostel-mode-hook 'spacemacs/disable-hl-line-mode)
    (with-eval-after-load 'centered-cursor-mode
      (add-hook 'ghostel-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode))
    (spacemacs/set-leader-keys-for-major-mode 'ghostel-mode
      "c" 'multighostel
      "n" 'ghostel-next
      "N" 'ghostel-previous
      "p" 'ghostel-previous)))

(defun personal-shell/init-evil-ghostel ()
  (use-package evil-ghostel
    :defer t
    :after (ghostel evil)
    :hook (ghostel-mode . evil-ghostel-mode)))

(defun personal-shell/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :shell-layer
   (purpose-conf :mode-purposes '((ghostel-mode . terminal)))))

;;; packages.el ends here
