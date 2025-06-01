;;; test-prompt-binder.el --- Tests for prompt-binder.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; Mock the llm library since we don't want actual LLM calls in tests
(defvar test-llm-streaming-callback nil)
(defvar test-llm-success-callback nil)
(defvar test-llm-error-callback nil)
(defvar test-llm-prompt nil)
(defvar test-llm-provider nil)

(defun llm-chat-streaming (provider prompt streaming-callback success-callback error-callback)
  "Mock implementation for testing"
  (setq test-llm-provider provider
        test-llm-prompt prompt
        test-llm-streaming-callback streaming-callback
        test-llm-success-callback success-callback
        test-llm-error-callback error-callback))

(defun llm-make-chat-prompt (content &rest args)
  "Mock implementation for testing"
  (list :content content :args args))

(defun make-llm-ollama (&rest args)
  "Mock implementation for testing"
  (list :type 'ollama :args args))

;; Load the code under test
(load-file "prompt-binder.el")

;;; Buffer Management Tests

(ert-deftest test-prompt-binder-creates-buffer ()
  "Test that prompt-binder-llm-stream-to-buffer creates the expected buffer."
  (let ((buffer-name "*Test Buffer*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (prompt-binder-llm-stream-to-buffer "test content" "test context" buffer-name nil)
    
    (should (get-buffer buffer-name))
    (with-current-buffer buffer-name
      (should (string-match-p "Prompt: test content" (buffer-string))))
    
    (kill-buffer buffer-name)))

(ert-deftest test-prompt-binder-buffer-content-format ()
  "Test that the buffer content is formatted correctly."
  (let ((buffer-name "*Format Test*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (prompt-binder-llm-stream-to-buffer "my prompt" "my context" buffer-name nil)
    
    (with-current-buffer buffer-name
      (let ((content (buffer-string)))
        (should (string-match-p "^Prompt: my prompt\n\n" content))
        (should (string-match-p "Waiting for response" content))))
    
    (kill-buffer buffer-name)))

(ert-deftest test-prompt-binder-default-buffer-name ()
  "Test that default buffer name is used when name is nil."
  (when (get-buffer "*LLM Response*")
    (kill-buffer "*LLM Response*"))
  
  (prompt-binder-llm-stream-to-buffer "test" "context" nil nil)
  
  (should (get-buffer "*LLM Response*"))
  (kill-buffer "*LLM Response*"))

;;; Spinner Tests

(ert-deftest test-spinner-functions-exist ()
  "Test that spinner helper functions are defined."
  (should (fboundp 'prompt-binder-update-spinner))
  (should (fboundp 'prompt-binder-start-spinner))
  (should (fboundp 'prompt-binder-stop-spinner)))

(ert-deftest test-spinner-marker-handling ()
  "Test spinner marker creation and cleanup."
  (let ((test-buffer (get-buffer-create "*Spinner Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "Initial content"))
    
    ;; Test spinner start
    (let ((spinner-marker nil))
      (prompt-binder-start-spinner spinner-marker test-buffer)
      (with-current-buffer test-buffer
        (should (string-match-p "Waiting for response" (buffer-string)))))
    
    (kill-buffer test-buffer)))

;;; LLM Integration Tests

(ert-deftest test-llm-chat-streaming-called ()
  "Test that llm-chat-streaming is called with correct parameters."
  (setq test-llm-provider nil
        test-llm-prompt nil
        test-llm-streaming-callback nil
        test-llm-success-callback nil
        test-llm-error-callback nil)
  
  (let ((provider (make-llm-ollama :chat-model "test-model")))
    (prompt-binder-llm-stream-to-buffer "test content" "test context" "*Test*" provider)
    
    (should (equal test-llm-provider provider))
    (should (equal (plist-get test-llm-prompt :content) "test content"))
    (should (functionp test-llm-streaming-callback))
    (should (functionp test-llm-success-callback))
    (should (functionp test-llm-error-callback)))
  
  (when (get-buffer "*Test*")
    (kill-buffer "*Test*")))

(ert-deftest test-success-callback-behavior ()
  "Test that success callback properly updates buffer."
  (let ((buffer-name "*Success Test*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (prompt-binder-llm-stream-to-buffer "test" "context" buffer-name nil)
    
    ;; Simulate success callback
    (funcall test-llm-success-callback "This is the response text")
    
    (with-current-buffer buffer-name
      (let ((content (buffer-string)))
        (should (string-match-p "Response:" content))
        (should (string-match-p "This is the response text" content))
        (should (string-match-p "Response Complete" content))))
    
    (kill-buffer buffer-name)))

(ert-deftest test-error-callback-behavior ()
  "Test that error callback properly handles errors."
  (let ((buffer-name "*Error Test*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (prompt-binder-llm-stream-to-buffer "test" "context" buffer-name nil)
    
    ;; Simulate error callback
    (funcall test-llm-error-callback "network" "Connection failed")
    
    (with-current-buffer buffer-name
      (let ((content (buffer-string)))
        (should (string-match-p "Error (network): Connection failed" content))))
    
    (kill-buffer buffer-name)))

;;; Macro Tests

(ert-deftest test-defprompt-macro-creates-function ()
  "Test that defprompt macro creates the expected function."
  (defprompt :function-name test-prompt-function
             :content "test content"
             :context "test context"
             :provider nil)
  
  (should (fboundp 'test-prompt-function))
  (should (commandp 'test-prompt-function))
  
  ;; Clean up
  (fmakunbound 'test-prompt-function))

(ert-deftest test-defprompt-key-binding ()
  "Test that defprompt creates key bindings when specified."
  ;; Save current binding
  (let ((old-binding (global-key-binding (kbd "C-c t t"))))
    
    (defprompt :function-name test-keybind-function
               :content "test"
               :context "test"
               :provider nil
               :key-combo "C-c t t")
    
    (should (eq (global-key-binding (kbd "C-c t t")) 'test-keybind-function))
    
    ;; Clean up
    (global-unset-key (kbd "C-c t t"))
    (when old-binding
      (global-set-key (kbd "C-c t t") old-binding))
    (fmakunbound 'test-keybind-function)))

(ert-deftest test-defprompt-function-name-conversion ()
  "Test that defprompt handles string function names."
  (defprompt :function-name "string-function-name"
             :content "test"
             :context "test"
             :provider nil)
  
  (should (fboundp 'string-function-name))
  
  ;; Clean up
  (fmakunbound 'string-function-name))

;;; Edge Cases and Error Handling

(ert-deftest test-buffer-already-exists ()
  "Test behavior when target buffer already exists."
  (let ((buffer-name "*Existing Buffer*"))
    ;; Create buffer with some content
    (with-current-buffer (get-buffer-create buffer-name)
      (insert "Previous content"))
    
    (prompt-binder-llm-stream-to-buffer "new content" "context" buffer-name nil)
    
    (with-current-buffer buffer-name
      ;; Should erase previous content
      (should-not (string-match-p "Previous content" (buffer-string)))
      (should (string-match-p "Prompt: new content" (buffer-string))))
    
    (kill-buffer buffer-name)))

(ert-deftest test-nil-content-handling ()
  "Test handling of nil content parameter."
  (let ((buffer-name "*Nil Test*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (should-error (prompt-binder-llm-stream-to-buffer nil "context" buffer-name nil))
    
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

;;; Integration Test for code-reviewer

(ert-deftest test-code-reviewer-function-exists ()
  "Test that the code-reviewer function is properly defined."
  (should (fboundp 'code-reviewer))
  (should (commandp 'code-reviewer)))

(ert-deftest test-code-reviewer-key-binding ()
  "Test that code-reviewer has the expected key binding."
  (should (eq (global-key-binding (kbd "C-c n r")) 'code-reviewer)))

;;; Performance and Memory Tests

(ert-deftest test-multiple-concurrent-requests ()
  "Test behavior with multiple concurrent requests."
  (let ((buffers '("*Test1*" "*Test2*" "*Test3*")))
    ;; Clean up any existing buffers
    (dolist (buf buffers)
      (when (get-buffer buf)
        (kill-buffer buf)))
    
    ;; Create multiple requests
    (dolist (buf buffers)
      (prompt-binder-llm-stream-to-buffer "test content" "context" buf nil))
    
    ;; All buffers should exist
    (dolist (buf buffers)
      (should (get-buffer buf)))
    
    ;; Clean up
    (dolist (buf buffers)
      (kill-buffer buf))))

(ert-deftest test-marker-cleanup ()
  "Test that markers are properly managed."
  (let ((buffer-name "*Marker Test*")
        (initial-marker-count (length (buffer-list))))
    
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (prompt-binder-llm-stream-to-buffer "test" "context" buffer-name nil)
    
    ;; Buffer should exist
    (should (get-buffer buffer-name))
    
    ;; Clean up
    (kill-buffer buffer-name)
    
    ;; Should not leak buffers
    (should (<= (length (buffer-list)) (+ initial-marker-count 1)))))

;;; Test Runner Function

(defun run-prompt-binder-tests ()
  "Run all prompt-binder tests and display results."
  (interactive)
  (ert-run-tests-interactively "test-prompt-binder"))

(provide 'test-prompt-binder)
;;; test-prompt-binder.el ends here