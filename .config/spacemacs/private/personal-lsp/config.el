;;; config.el --- personal-lsp layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; Personal LSP configuration and performance tweaks.

;;; Code:

(with-eval-after-load 'json
  (advice-add (if (fboundp 'json-parse-buffer)
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;; See https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el#L344 for default ignores
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\bin\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\coverage\\'")
  (setopt
   ;; Formatting and indentation - use Cider instead
   lsp-enable-on-type-formatting nil
   ;; Set to nil to use CIDER features instead of LSP UI
   lsp-enable-indentation nil
   lsp-enable-snippet nil  ;; to test again

   ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
   ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
   lsp-enable-symbol-highlighting t

   ;; Show lint error indicator in the mode line
   lsp-modeline-diagnostics-enable nil

   ;; popup documentation boxes
   lsp-ui-doc-alignment 'window      ;; frame window

   ;; reference count for functions (assume their maybe other lenses in future)
   lsp-lens-enable t

   ;; Efficient use of space in treemacs-lsp display
   treemacs-space-between-root-nodes nil
   lsp-treemacs-error-list-current-project-only t  ; limit errors to current project

   ;; Optimization for large files
   lsp-file-watch-threshold 10000
   ;; Turn off as much debug/logging to improve performance
   lsp-log-io nil
   lsp-use-plists t))

(with-eval-after-load 'lsp-treemacs
  (lsp-treemacs-sync-mode 1))

(with-eval-after-load 'lsp-java
  (setopt lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.53.0/jdt-language-server-1.53.0-202511192211.tar.gz")
  (setopt lsp-java-vmargs
          `("-XX:+UseParallelGC"
            "-XX:GCTimeRatio=4"
            "-XX:AdaptiveSizePolicyWeight=90"
            "-Dsun.zip.disableMemoryMapping=true"
            "-Xmx4G")
          lsp-java-progress-reports-enabled nil
          lsp-java-completion-max-results 50
          lsp-java-progress-reports nil
          lsp-java-autobuild-enabled nil)
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

(with-eval-after-load 'lsp-pyright
  (setopt lsp-pyright-multi-root nil))

;;; config.el ends here
