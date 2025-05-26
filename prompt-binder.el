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
      (insert "Response:\n")
      (setq start-marker (point-marker)))

    ;; Display the buffer
    (pop-to-buffer response-buffer)

    (llm-chat-streaming provider
                        (llm-make-chat-prompt content :context context)
                        (lambda (text)
                          ;; This callback is called for each chunk of streamed text
                          (with-current-buffer response-buffer
                            (save-excursion
                              (goto-char (point-max))
                              (insert text))
                            ;; Auto-scroll to show new content
                            (when (get-buffer-window response-buffer)
                              (with-selected-window (get-buffer-window response-buffer)
                                (goto-char (point-max))))))
                        (lambda (text)
                          ;; Success callback - called when streaming is complete
                          (with-current-buffer response-buffer
                            (save-excursion
                              (goto-char (point-max))
                              (insert "\n\n--- Response Complete ---"))))
                        (lambda (type message)
                          ;; Error callback
                          (with-current-buffer response-buffer
                            (save-excursion
                              (goto-char (point-max))
                              (insert (format "\n\nError (%s): %s" type message))))))))




;;(fmakunbound 'prompt-lib-llm-stream-to-buffer)



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
