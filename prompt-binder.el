;;; prompt-binder.el --- Bind LLM prompts to key chords.   -*- lexical-binding:t -*-
;;; Commentary:
;;; A lightweight Emacs package that lets you quickly invoke Large Language
;;; Model (LLM) prompts with simple key combinations.  Stream responses directly into de;;; dicated buffers with visual feedback.


(require 'llm)
(require 'llm-ollama)
(require 'cl-lib)

;;;; Code:
(setq llm-provider (make-llm-ollama :chat-model "devstral:latest"))

(defun prompt-binder-update-spinner (spinner-marker response-buffer spinner-chars)
  "Starts the visual feedback spinner"
  (when (and spinner-marker (buffer-live-p response-buffer))
    (with-current-buffer response-buffer
      (save-excursion
        (goto-char spinner-marker)
        (delete-char 1)
        (insert (aref spinner-chars spinner-index))
        (setq spinner-index (mod (1+ spinner-index) (length spinner-chars)))))))


(defun prompt-binder-start-spinner (spinner-marker response-buffer spinner-chars)
  "Updates spinner animation"
  (with-current-buffer response-buffer
    (save-excursion
      (goto-char (point-max))
      (insert " Waiting for response  ")
      (setq spinner-marker (1- (point)))))
  (setq spinner-timer (run-with-timer 0.1 0.1 (lambda () (prompt-binder-update-spinner spinner-marker response-buffer spinner-chars)))))


(defun prompt-binder-stop-spinner (spinner-marker response-buffer)
  "Stops and cleans up spinner"
  (when spinner-timer
    (cancel-timer spinner-timer)
    (setq spinner-timer nil))
  (when (and spinner-marker (buffer-live-p response-buffer))
    (with-current-buffer response-buffer
      (save-excursion
        (goto-char spinner-marker)
        (delete-region spinner-marker (line-end-position))))))

(defun prompt-binder-llm-stream-to-buffer (content context name provider)
  "Passes CONTENT and CONTEXT to LLM defined by PROVIDER. Handles streaming LLM response to buffer NAME."
  (let* ((buffer-name (or name "*LLM Response*"))
         (response-buffer (get-buffer-create buffer-name))
         (start-marker nil))

    ;; Set up the response buffer
    (with-current-buffer response-buffer
      (erase-buffer)
      (insert (format "Prompt: %s\n\n" content))
      (setq start-marker (point-marker)))

    ;; Display the buffer
    (pop-to-buffer response-buffer)

    (let ((last-inserted-length 0)
          (spinner-chars ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
          (spinner-timer nil)
          (spinner-marker nil)
          (first-response-received nil))

      (progn
        (setq spinner-index 0)
        ;; Start the spinner
        (prompt-binder-start-spinner spinner-marker response-buffer spinner-chars)

        (llm-chat-streaming provider
                            (llm-make-chat-prompt content :context context)
                            (lambda (text)
                              (with-current-buffer response-buffer
                                (save-excursion
                                  (prompt-binder-update-spinner spinner-marker
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


(cl-defmacro defprompt (&key function-name content context provider key-combo)
  "Define an interactive function that calls prompt-lib-llm-stream-to-buffer.
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
