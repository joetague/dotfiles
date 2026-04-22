;;; config.el --- personal-devtools layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;;; Commentary:

;; Personal development tooling defaults.

;;; Code:

;; tree-sitter grammar sources not provided by Emacs built-in *-ts-modes
;; Built-in modes define their own sources for: bash, css, dockerfile, html,
;; java, javascript, jsdoc, json, markdown, markdown-inline, toml, tsx,
;; typescript, yaml (and doxygen). Only add grammars Emacs doesn't provide.
(with-eval-after-load 'treesit
  (dolist (source '((elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.5.0")
                    (make "https://github.com/alemuller/tree-sitter-make")
                    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")))
    (add-to-list 'treesit-language-source-alist source)))

;; Spacemacs uses bind-map to create per-mode evil leader keymaps (the ","
;; prefix).  These are keyed by exact mode symbol, so *-ts-modes get nothing.
;; Fix: (1) re-parent ts-modes so hooks and derived-mode-p checks work, and
;;      (2) use spacemacs/inherit-leader-keys-from-parent-mode to give each
;;          ts-mode a leader keymap that inherits from the classic mode's.
(defvar personal-devtools--ts-mode-parents
  '((python-ts-mode     . python-mode)
    (js-ts-mode         . js2-mode)
    (typescript-ts-mode . typescript-mode)
    (tsx-ts-mode        . typescript-tsx-mode)
    (json-ts-mode       . json-mode)
    (yaml-ts-mode       . yaml-mode)
    (bash-ts-mode       . sh-mode)
    (java-ts-mode       . java-mode)
    (css-ts-mode        . css-mode)
    (dockerfile-ts-mode . dockerfile-mode)
    (markdown-ts-mode   . markdown-mode)
    (html-ts-mode       . html-mode)
    (toml-ts-mode       . conf-toml-mode))
  "Alist mapping tree-sitter modes to their classic Spacemacs counterparts.")

(defun personal-devtools--setup-ts-modes ()
  "Re-parent all ts-modes and inherit Spacemacs leader keys."
  (dolist (pair personal-devtools--ts-mode-parents)
    (let ((ts-mode (car pair))
          (classic-mode (cdr pair)))
      (derived-mode-set-parent ts-mode classic-mode)
      (spacemacs/inherit-leader-keys-from-parent-mode ts-mode classic-mode))))

;; Run after all layers have configured their keybindings.
(add-hook 'after-init-hook #'personal-devtools--setup-ts-modes)

(with-eval-after-load 'projectile
  (setopt projectile-project-search-path '(("~/proj/" . 2))))
;; (setq projectile-create-missing-test-files t)
;; (setq projectile-enable-caching t)
;; (setq projectile-indexing-method 'native)


;;; config.el ends here
