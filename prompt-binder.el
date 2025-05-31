;;; prompt-binder.el --- Bind LLM prompts to key chords.   -*- lexical-binding:t -*-

(require 'llm)
(require 'llm-ollama)
(require 'cl-lib)

(setq llm-provider (make-llm-ollama :chat-model "devstral:latest"))


(defun prompt-lib-llm-stream-to-buffer (content context name provider)
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
          (spinner-index 0)
          (spinner-timer nil)
          (spinner-marker nil)
          (first-response-received nil))

      ;; Function to update spinner
      (defun update-spinner ()
        (when (and spinner-marker (buffer-live-p response-buffer))
          (with-current-buffer response-buffer
            (save-excursion
              (goto-char spinner-marker)
              (delete-char 1)
              (insert (aref spinner-chars spinner-index))
              (setq spinner-index (mod (1+ spinner-index) (length spinner-chars)))))))

      ;; Function to start spinner
      (defun start-spinner ()
        (with-current-buffer response-buffer
          (save-excursion
            (goto-char (point-max))
            (insert (aref spinner-chars 0) " Waiting for response...")
            (setq spinner-marker (1- (point)))))
        (setq spinner-timer (run-with-timer 0.1 0.1 #'update-spinner)))

      ;; Function to stop spinner
      (defun stop-spinner ()
        (when spinner-timer
          (cancel-timer spinner-timer)
          (setq spinner-timer nil))
        (when (and spinner-marker (buffer-live-p response-buffer))
          (with-current-buffer response-buffer
            (save-excursion
              (goto-char spinner-marker)
              (delete-region spinner-marker (line-end-position))))))

      ;; Start the spinner
      (start-spinner)

      (llm-chat-streaming provider
                          (llm-make-chat-prompt content :context context)
                          (lambda (text)
                            ;;(erase-buffer)
                            ;;(insert "Waiting for llm response...")
                            (unless first-response-received
                              (stop-spinner)
                              (setq first-response-received t))
                            )
                          (lambda (text)
                            ;; Success callback - called when streaming is complete
                            (with-current-buffer response-buffer
                              (save-excursion
                                (goto-char (point-max))
                                (insert "\n\nResponse:\n\n")
                                (insert text)
                                (insert "\n\n--- Response Complete ---"))))
                          (lambda (type message)
                            ;; Error callback
                            (with-current-buffer response-buffer
                              (save-excursion
                                (goto-char (point-max))
                                (insert (format "\n\nError (%s): %s" type message)))))))))




(fmakunbound 'prompt-lib-llm-stream-to-buffer)



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
         (prompt-lib-llm-stream-to-buffer ,content ,context ,buffer-name ,provider))

       ;; Bind the key combination if provided
       ,(when key-combo
          `(global-set-key (kbd ,key-combo) ',func-name))

       ;; Return the function name for confirmation
       ',func-name)))



(defprompt :function-name code-reviewer
           :content "Please review this code for potential issues, bugs, and improvements."
           :context (format "You are an experienced software engineer conducting a thorough code review. Focus on correctness, performance, security, and maintainability. Use the following code for your review %s" (buffer-substring-no-properties (point-min) (point-max)))
           :provider (make-llm-ollama :chat-model "devstral:latest")
           :key-combo "C-c n r")
