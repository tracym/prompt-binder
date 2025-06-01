# prompt-binder.el

**Bind LLM prompts to key chords in Emacs**

A lightweight Emacs package that lets you quickly invoke Large Language Model (LLM) prompts with simple key combinations. Stream responses directly into dedicated buffers with visual feedback.

## Features

- 🚀 **Quick Access**: Bind any LLM prompt to custom key combinations
- 📡 **Streaming Responses**: Real-time response streaming with visual spinner feedback
- 🎯 **Context-Aware**: Include buffer content or custom context in prompts
- 🔧 **Extensible**: Easy macro-based prompt definition system

## Installation

### Prerequisites

- Emacs 25.1+ with `lexical-binding` support
- [llm.el](https://github.com/ahyatt/llm) package
- [Ollama](https://ollama.ai/) For running locally

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/prompt-binder.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/prompt-binder")
   (require 'prompt-binder)
   ```

### Package Manager Installation

*Coming soon: MELPA submission planned*

## Configuration

### Setting Up Your LLM Provider

```elisp
(require 'llm)

;; Use an Ollama model
(require 'llm-ollama)
(setq llm-provider (make-llm-ollama :chat-model "codellama:13b"))

;; Or use OpenAI (requires additional setup)
(require 'llm-openai)
(setq llm-provider (make-llm-openai :key "your-api-key"))
```

### Creating Custom Prompts

Use the `defprompt` macro to create your own AI-powered commands:

```elisp
(defprompt :function-name explain-code
           :content "Explain what this code does in simple terms"
           :context (format "You are a helpful programming tutor. Explain the following code 
                             clearly and concisely. %s" (buffer-substring-no-properties (point-min) (point-max)))
           :provider llm-provider
           :key-combo "C-c e x")
```

### Other Examples

**Documentation Generator:**
```elisp
(defprompt :function-name generate-docs
           :content "Generate comprehensive documentation for this code"
           :context (format "Write detailed documentation including purpose, parameters, return values, and usage examples for: %s" 
                           (buffer-substring-no-properties (point-min) (point-max)))
           :provider llm-provider
           :key-combo "C-c d g")
```

**Bug Hunter:**
```elisp
(defprompt :function-name find-security-bugs
           :content "Analyze this code for potential bugs and security issues"
           :context (format "You are a security-focused code auditor. Look for bugs, 
                             vulnerabilities, and edge cases in the following code snippets %s" (a-custom-vector-search "delete_all('id = #{params[:user_id]}')"))
           :provider llm-provider
           :key-combo "C-c b f")
```

**Code Reviewer:**
```elisp
(defprompt :function-name code-reviewer
           :content "Please review this code for potential issues, bugs, and improvements."
           :context (format "You are an experienced software engineer conducting a thorough code review. Focus on correctness, performance, security, and maintainability. Use the following code for your review %s" (buffer-substring-no-properties (point-min) (point-max)))
           :provider (make-llm-ollama :chat-model "devstral:latest")
           :key-combo "C-c n r")
``


## API Reference

### `defprompt` Macro

```elisp
(defprompt &key function-name content context provider key-combo)
```

**Parameters:**
- `:function-name` - Symbol or string for the function name
- `:content` - The user prompt to send to the LLM
- `:context` - System prompt or additional context
- `:provider` - LLM provider instance (defaults to `llm-provider`)
- `:key-combo` - Key binding string (e.g., "C-c p r")

### Core Functions

#### `prompt-binder-llm-stream-to-buffer`
```elisp
(prompt-binder-llm-stream-to-buffer content context name provider)
```

Main function that handles LLM streaming and buffer management.

#### Spinner Functions
- `prompt-binder-start-spinner` - Starts the visual feedback spinner
- `prompt-binder-update-spinner` - Updates spinner animation
- `prompt-binder-stop-spinner` - Stops and cleans up spinner

## Buffer Management

Responses appear in dedicated buffers named `*function-name*`. Each buffer shows:

- **Prompt**: The original prompt sent to the LLM
- **Spinner**: Visual feedback during response generation
- **Response**: The complete LLM response
- **Status**: Completion or error messages

Buffers are reused for subsequent calls to the same prompt function.

## Troubleshooting

### Common Issues

**"Symbol's function definition is void: llm-chat-streaming"**
- Ensure `llm.el` is properly installed and loaded
- Check that `(require 'llm)` and `(require 'llm-ollama)` are evaluated

**"Connection refused" errors**
- Verify Ollama is running: `ollama serve`
- Check that your model is available: `ollama list`
- Try pulling the model: `ollama pull devstral:latest`

**Spinner stuck or response not appearing**
- The LLM request may have failed silently
- Check the `*Messages*` buffer for error details
- Try a different model or restart Ollama

### Performance Tips

- Use smaller models for faster responses
- Consider local models for privacy
- Limit context size for large buffers

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

### Development Setup

```bash
git clone https://github.com/yourusername/prompt-binder.git
cd prompt-binder

# Run tests
emacs -batch -l ert -l test-prompt-binder.el -f ert-run-tests-batch-and-exit
```

## License

GPL-3.0-or-later - see [LICENSE](LICENSE) file for details.

## Related Projects

- [llm.el](https://github.com/ahyatt/llm) - Emacs LLM interface library
- [gptel](https://github.com/karthink/gptel) - ChatGPT client for Emacs
- [ellama](https://github.com/s-kostyaev/ellama) - Tool for interacting with LLMs

## Changelog

### v0.1.0 (Current)
- Initial release
- Basic prompt binding functionality
- Streaming response support

---

*Happy prompting! 🤖*
