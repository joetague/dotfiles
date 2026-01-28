;;; config.el --- personal-llm layer config file for Spacemacs. -*- lexical-binding: t -*-
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

;; gptel and LLM integrations.

;;; Code:

(with-eval-after-load 'gptel
  (require 'gptel-integrations)

  (gptel-make-kagi "Kagi" :key (auth-source-pick-first-password :host "api.kagi.com"))

  (setopt
   gptel-model 'qwen/qwen3-14b
   gptel-backend (gptel-make-openai "LMStudio"
                   :host "localhost:1234"
                   :protocol "http"
                   :endpoint "/v1/chat/completions"
                   :stream t
                   :key "not-needed"
                   :models '("qwen/qwen3-14b")))  ;; Default model, refresh with jpt/refresh-lmstudio-models

  ;; Use the system prompt builder function
  (let ((build-directives-fun "~/proj/llm-prompts/gptel-build-directives.el"))
    (when (f-exists-p build-directives-fun)
      (load build-directives-fun)
      (setq gptel-directives (jpt/gptel-build-directives "~/proj/llm-prompts/system-prompts/")
            gptel-system-message (alist-get 'default gptel-directives)))))


;; LLM
;; Agents
;; (require 'shell-maker)
;; (require 'acp)
;; (require 'agent-shell)

;; claude-code.el with Monet
;; (setopt claude-code-terminal-backend 'vterm)
;; (setopt claude-code-notification-function #'jpt/claude-notify)
;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;; (monet-mode 1)
;; ;; set up keybindings
;; (spacemacs/declare-prefix "$d" "Claude")
;; (spacemacs/set-leader-keys
;;   "$dm" 'claude-code-transient           ;
;;   "$dc" 'claude-code                     ;
;;   "$dS" 'claude-code-sandbox             ;
;;   "$dd" 'claude-code-start-in-directory  ;
;;   "$dC" 'claude-code-continue            ;
;;   "$dR" 'claude-code-resume              ;
;;   "$di" 'claude-code-new-instance        ;
;;   "$dk" 'claude-code-kill                ;
;;   "$dK" 'claude-code-kill-all)           ;

;; claude-code-ide.el
;; Set up the built-in Emacs tools
;; Define a context-aware function that operates in the session's project
;; Functions here
;; Define and register the tool (automatically added to claude-code-ide-mcp-server-tools)
;; (claude-code-ide-emacs-tools-setup)
;; Claude Code with Monet setup
;; (setopt claude-code-terminal-backend 'vterm)
;; (setopt claude-code-notification-function #'jpt/claude-notify)
;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;; (monet-mode 1)
;;; config.el ends here
