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

(defconst personal-shell-packages
  '(ghostel
    (evil-ghostel :toggle personal-shell-enable-evil-ghostel)
    hungry-delete
    window-purpose)
  "The list of Lisp packages required by the personal-shell layer.")

(defun personal-shell/init-ghostel ()
  "Initialize Ghostel."
  (use-package ghostel
    :defer t
    :commands (ghostel
               ghostel-project
               ghostel-other
               ghostel-next
               ghostel-previous
               ghostel-list-buffers
               ghostel-project-next
               ghostel-project-previous
               ghostel-project-list-buffers)
    :init
    (make-shell-pop-command "ghostel" ghostel)
    (spacemacs/set-leader-keys "atsg" #'spacemacs/shell-pop-ghostel)
    (spacemacs/register-repl 'ghostel 'ghostel)
    (defalias 'projectile-run-ghostel #'ghostel-project)
    :config
    (setq ghostel-shell shell-default-term-shell)
    (spacemacs/set-leader-keys-for-major-mode 'ghostel-mode
      "c" #'ghostel
      "l" #'ghostel-list-buffers
      "n" #'ghostel-next
      "N" #'ghostel-previous
      "p" #'ghostel-previous
      "r" #'rename-buffer)
    (when personal-shell-enable-ghostel-compile-global-mode
      (require 'ghostel-compile)
      (ghostel-compile-global-mode 1))
    (when personal-shell-enable-ghostel-eshell-visual-command-mode
      (require 'ghostel-eshell)
      (add-hook 'eshell-load-hook #'ghostel-eshell-visual-command-mode))))

(defun personal-shell/init-evil-ghostel ()
  "Initialize evil-ghostel."
  (use-package evil-ghostel
    :defer t
    :hook (ghostel-mode . evil-ghostel-mode)))

(defun personal-shell/post-init-hungry-delete ()
  "Keep hungry-delete out of Ghostel terminal buffers."
  (with-eval-after-load 'hungry-delete
    (add-to-list 'hungry-delete-except-modes 'ghostel-mode)))

(defun personal-shell/post-init-window-purpose ()
  "Treat Ghostel buffers as terminal buffers for window-purpose."
  (purpose-set-extension-configuration
   :personal-shell
   (purpose-conf :mode-purposes '((ghostel-mode . terminal)))))

;;; packages.el ends here
