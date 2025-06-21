;;; prompt-binder.el --- Bind LLM prompts to key chords and editor context   -*- lexical-binding:t -*-
;; Keywords: llm, tools, prompt
;; Version: 0.1.0
;; URL: http://github.com/tracym
;; Package-Requires: ((emacs "24.3") (llm "0.26.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A lightweight Emacs package that lets you quickly invoke Large
;; Language Model (LLM) prompts with simple key combinations.
;; Pass context from Emacs into your LLM Prompts.
;; Stream responses directly into dedicated buffers with visual
;; feedback

;;; Code:

(require 'llm)
(require 'cl-lib)

(defvar prompt-binder-spinner-index 0)
(defvar prompt-binder-spinner-timer nil)

(defun prompt-binder-update-spinner (spinner-marker response-buffer spinner-chars)
  "Start the visual feedback spinner.
Argument SPINNER-MARKER Location of the spinner in RESPONSE-BUFFER.
Argument SPINNER-CHARS Array of chars used for representing the
spinner."
  (when (and spinner-marker (buffer-live-p response-buffer))
    (with-current-buffer response-buffer
      (save-excursion
        (goto-char spinner-marker)
        (delete-char 1)
        (insert (aref spinner-chars prompt-binder-spinner-index))
        (setq prompt-binder-spinner-index (mod (1+ prompt-binder-spinner-index) (length spinner-chars)))))))


(defun prompt-binder-start-spinner (spinner-marker response-buffer spinner-chars)
  "Update spinner animation.
Argument SPINNER-MARKER Location of the spinner in RESPONSE-BUFFER.
Argument SPINNER-CHARS Array of chars used for representing the
spinner."
  (with-current-buffer response-buffer
    (save-excursion
      (goto-char (point-max))
      (insert " Waiting for response  ")
      (setq spinner-marker (1- (point)))))
  (setq prompt-binder-spinner-timer (run-with-timer 0.1 0.1 (lambda () (prompt-binder-update-spinner spinner-marker response-buffer spinner-chars)))))


(defun prompt-binder-stop-spinner (spinner-marker response-buffer)
  "Stops and cleans up spinner.
Argument SPINNER-MARKER Location of the spinner in RESPONSE-BUFFER."
  (when prompt-binder-spinner-timer
    (cancel-timer prompt-binder-spinner-timer)
    (setq prompt-binder-spinner-timer nil))
  (when (and spinner-marker (buffer-live-p response-buffer))
    (with-current-buffer response-buffer
      (save-excursion
        (goto-char spinner-marker)
        (delete-region spinner-marker (line-end-position))))))

(defun prompt-binder-llm-stream-to-buffer (content context name provider)
  "Passes CONTENT and CONTEXT to LLM defined by PROVIDER.
Handles streaming LLM response to buffer NAME."
  (let* ((buffer-name (or name "*LLM Response*"))
         (response-buffer (get-buffer-create buffer-name)))

    ;; Set up the response buffer
    (with-current-buffer response-buffer
      (erase-buffer)
      (insert (format "Prompt: %s\n\n" content)))

    ;; Display the buffer
    (pop-to-buffer response-buffer)

    (let ((spinner-chars ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
          (spinner-marker nil))

      (progn
        ;; Start the spinner
        (prompt-binder-start-spinner spinner-marker response-buffer spinner-chars)

        (llm-chat-streaming provider
                            (llm-make-chat-prompt content :context context)
                            (lambda ()
                              (with-current-buffer response-buffer
                                (save-excursion (prompt-binder-update-spinner spinner-marker
                                                                              response-buffer spinner-chars))))
                            (lambda (text)
                              ;; Success callback - called when streaming is complete
                              (with-current-buffer response-buffer
                                (save-excursion
                                  (prompt-binder-stop-spinner spinner-marker response-buffer)
                                  (goto-char (point-max))
                                  (insert "\n\nResponse:\n\n")
                                  (insert text)
                                  (insert "\n\n--- Response Complete ---"))))
                            (lambda (type message)
                              ;; Error callback
                              (with-current-buffer response-buffer
                                (save-excursion
                                  (prompt-binder-stop-spinner spinner-marker response-buffer)
                                  (goto-char (point-max))
                                  (insert (format "\n\nError (%s): %s" type message))))))))))


(cl-defmacro prompt-binder-define-binding (&key function-name content context provider key-combo)
  "Define an interactive function that call prompt-lib-llm-stream-to-buffer.
FUNCTION-NAME: name of the function to create
CONTENT: the user prompt content
CONTEXT: the system prompt/context
PROVIDER: LLM provider to use
KEY-COMBO: key binding string for the function"
  (let ((func-name (if (symbolp function-name)
                       function-name
                     (intern (format "%s" function-name))))
        (buffer-name (format "*%s*" function-name)))
    `(progn
       ;; Define the interactive function
       (defun ,func-name ()
         ,(format "Interactive function to run prompt: %s" content)
         (interactive)
         (prompt-binder-llm-stream-to-buffer ,content ,context ,buffer-name ,provider))

       ;; Bind the key combination if provided
       ,(when key-combo
          `(global-set-key (kbd ,key-combo) ',func-name))

       ;; Return the function name for confirmation
       ',func-name)))

(provide 'prompt-binder)
;;; prompt-binder.el ends here
