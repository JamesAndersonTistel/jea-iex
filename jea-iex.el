;;; jea-iex.el --- improve the readability of the elxir iex tool

;; Copyright © 2025 James Anderson
;;
;; Author: James Anderson <james@tisteltech.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;; software and associated documentation files (the "Software"), to deal in the Software
;; without restriction, including without limitation the rights to use, copy, modify,
;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; When iex is really active it can have lots of warnings that make
;;; interactive use difficult, lets add the option to filter some of
;;; them out.
;;;
;;; in the mode you can call `jea-iex-cli-filter-on-toggle` or hit F5 to
;;; toggle on/off the filter.
;;;
;;; based on:
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
;;;

;;; Code:
(require 'comint)

(defvar jea-iex-cli-file-path "iex"
	"Path to iex program.  This assumes its on the PATH, if not, set to full path.")

(defvar jea-iex-cli-arguments '("--dbg" "pry" "-S" "mix" "phx.server")
	"Command line arguments to iex.")

(defvar jea-iex-cli-filter-state "off"
	"Toggle which lines of text are filtered out of the `iex` output.

Can be one of: off, warnings_and_errors or all_prints.")

(defun jea-iex-cli-filter--get-next-state (current)
	"Simple state machine transition table from the CURRENT state."
	(cond
	 ((string-equal current "off") "warnings_and_errors")
	 ((string-equal current "warnings_and_errors") "all_prints")
	 ((string-equal current "all_prints") "off")
	 (t "off")))

(defun jea-iex-cli-filter--goto-next-state ()
	"Here we actually do the state transition for the filter."
	(let ((prev jea-iex-cli-filter-state)
				(next (jea-iex-cli-filter--get-next-state jea-iex-cli-filter-state)))
		(setq jea-iex-cli-filter-state next)
		(message "jea-filter state change from %s to %s." prev next)))

(defun jea-iex-cli-filter-on-toggle()
	"Toggle on/off the filter because filter can be too aggressive."
	(interactive)
	(jea-iex-cli-filter--goto-next-state))

(defvar jea-iex-mode-map
	(let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)  ;; TODO: grab the `binding` output of the var in the context
		(define-key map [(f5)] 'jea-iex-cli-filter-on-toggle)
    map)
  "Basic mode map for `jea-iex'.")

(defvar jea-iex-prompt-regexp "^iex\\([0-9]+\\)>"
  "Prompt for `run-cassandra'.")

(defvar jea-iex-filter-line '(jea-iex-filter-line-process)
	"Function used to filter out unwanted output lines.")

(defun run-jea-iex (arg buffer-name)
  "Run an inferior instance of `jea-iex-cli' inside Emacs named BUFFER-NAME.
ARG is prefix."
  (interactive "p\nsproject name: ")
  (let* ((jea-iex-program jea-iex-cli-file-path)
         (buffer (get-buffer-create (concat "*jea-iex-" buffer-name "*")))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "jea-iex" buffer
               jea-iex-program nil jea-iex-cli-arguments)
        (jea-iex-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun jea-iex--initialize ()
  "Helper function to initialize jea-iex."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode jea-iex-mode comint-mode "jea-iex"
  "Major mode for `run-jea-iex'.

\\<jea-iex-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp jea-iex-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(jea-iex-font-lock-keywords t))
  (set (make-local-variable 'comint-preoutput-filter-functions) jea-iex-filter-line)
  (set (make-local-variable 'comint-prompt-read-only) nil)
  (set (make-local-variable 'paragraph-start) jea-iex-prompt-regexp))

(add-hook 'jea-iex-mode-hook 'jea-iex--initialize)

(defconst jea-iex-keywords
  '("Float" "Decimal" "String")
  "List of keywords to highlight in `jea-iex-font-lock-keywords'.")

(defvar jea-iex-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt jea-iex-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `jea-iex-mode'.")

(defun strip-ansi-chars (str)
  "Strips ANSI escape sequences from STR."
  (let ((clean-str (ansi-color-apply str)))
    (set-text-properties 0 (length clean-str) nil clean-str)
    clean-str))

(defun jea-iex-filter--line-process-warnings_and_errors (line)
	"Filter out some LINE if it matchines a prefix."
	(cond
	 ((string-prefix-p "[warning]" line) "") ;; TODO make this into a filter list
	 ((string-prefix-p "[error]" line) "")
	 (t
		line)))

(defun jea-iex-filter--line-process-all-prints (line)
	"Filter out some LINE if it matchines a prefix."
	(cond
	 ((string-prefix-p "[warning]" line) "") ;; TODO make this into a filter list
	 ((string-prefix-p "[error]" line) "")
	 ((string-prefix-p "[info]" line) "")
	 (t
		line)))

(defun jea-iex-filter-line-process (line)
	"Process LINE to remove unwanted output.  Should probably be a var."
  (let ((clean-line (strip-ansi-chars line)))
		(cond
		 ((string-equal jea-iex-cli-filter-state "off")
			line)
		 ((string-equal jea-iex-cli-filter-state "warnings_and_errors")
			(jea-iex-filter--line-process-warnings_and_errors line))
		 ((string-equal jea-iex-cli-filter-state "all_prints")
			(jea-iex-filter--line-process-all-prints line))
		 (t
			line))))

;;; jea-iex.el ends here
