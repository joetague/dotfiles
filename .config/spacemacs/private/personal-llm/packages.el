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
  '(agent-shell
    mcp)
  "The list of Lisp packages required by the personal-llm layer.")

(defun jpt/init-personal-llm ()
  "Initialize agent-shell package."

  ;; xenodium based tooling
  ;; Consider https://github.com/xenodium/agent-shell?tab=readme-ov-file#running-agents-in-devcontainers--docker-containers-experimental
  (use-package acp
    :config
    (use-package agent-shell
      :commands
      (agent-shell-anthropic-start-claude-code
       agent-shell-openai-start-codex)
      :config

      (setq agent-shell-anthropic-authentication
            (agent-shell-anthropic-make-authentication
             :api-key (lambda () (nth 0 (process-lines "pass" "show" "anthropic-key")))))
      (setq agent-shell-openai-authentication
            (agent-shell-openai-make-authentication
             :api-key (lambda () (nth 0 (process-lines "pass" "show" "openai-key")))))
      ))


  ;; (acp :location (recipe :fetcher github :repo "xenodium/acp.el"))
  ;; (agent-shell :location (recipe :fetcher github :repo "xenodium/agent-shell"))
  ;; (claude-code :location (recipe
  ;;                         :fetcher github
  ;;                         :repo "stevemolitor/claude-code.el"))
  ;;(gptel-quick :location (recipe
  ;;                        :fetcher github
  ;;                        :repo "karthink/gptel-quick"))
  ;; (mcp :location (recipe
  ;;                 :fetcher github
  ;;                 :repo "lizqwerscott/mcp.el"))
  ;; (monet :location (recipe
  ;;                   :fetcher github
  ;;                   :repo "stevemolitor/monet"))
  ;; (gptel-project :location (recipe
  ;;                  :fetcher github
  ;;                  :repo "cvdub/gptel-project"))
  ;; shell-maker

  )

;;; packages.el ends here
