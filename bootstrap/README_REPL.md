# Habu Lisp REPL

An interactive Read-Eval-Print Loop for Habu Lisp with full line editing support.

## Features

### Line Editing
- **Arrow keys**: Navigate left/right through the line, up/down through history
- **Home/End**: Jump to beginning/end of line
- **Ctrl-A**: Move to beginning of line
- **Ctrl-E**: Move to end of line
- **Ctrl-K**: Kill (delete) to end of line
- **Ctrl-U**: Kill to beginning of line
- **Ctrl-L**: Clear screen
- **Ctrl-D**: Delete character or EOF if line is empty
- **Backspace**: Delete character before cursor

### Tab Completion
- Press **Tab** to complete symbols
- Shows all available operators, commands, and user-defined functions/macros
- If multiple completions exist, displays all options

### Command History
- **Up arrow**: Previous command in history
- **Down arrow**: Next command in history
- History persists across sessions in `~/.habu_history`
- Stores up to 1000 most recent commands

### REPL Commands
- `:quit` or `:q` - Exit REPL
- `:help` or `:h` - Show help
- `:clear` - Clear function and macro tables
- `:macros` - List defined macros
- `:functions` - List defined functions
- `:history` - Show command history
- `:complete <symbol>` - Show completions for symbol

## Usage

### Interactive Mode
```bash
sbcl --script repl.lisp
```

When running interactively, the REPL automatically enables:
- Full line editing with arrow keys
- Tab completion
- Command history navigation
- Terminal raw mode for character-by-character input

### Non-Interactive Mode (Scripted)
```bash
sbcl --script repl.lisp < script.lisp
```

When input is piped or redirected, the REPL automatically disables line editing
and runs in simple mode, suitable for automated testing and scripts.

## Examples

### Basic Arithmetic
```
habu> (+ 10 20)
=> 30

habu> (* 6 7)
=> 42
```

### Let Bindings
```
habu> (let ((x 100) (y 200)) (+ x y))
=> 300
```

### Macros
```
habu> (defmacro square (n) (* n n))
=> 0

habu> (square 5)
=> 25

habu> :macros
Defined macros:
  SQUARE (N)
```

### Tab Completion
Type `(lo` and press Tab to see:
```
Completions:
  LOGAND
  LOGIOR
  LOGXOR
  LOGNOT
```

### History Navigation
- Press **Up arrow** to recall previous commands
- Edit and re-execute with Enter
- Press **Down arrow** to move forward in history

## Implementation

The REPL consists of two main components:

### readline.lisp
Pure Common Lisp implementation of readline-style line editing:
- Raw terminal mode using `stty`
- ANSI escape code handling for cursor control
- Arrow key detection (ESC sequences)
- Edit buffer management
- History navigation
- Tab completion hooks

### repl.lisp
The REPL logic:
- Expression parser integration
- Interpreter for fixnum expressions
- Command handling
- History persistence
- Macro system integration

## Supported Operations

### Arithmetic
`+`, `-`, `*`, `/`, `mod`, `min`, `max`, `abs`, `1+`, `1-`

### Comparison
`<`, `>`, `=`, `<=`, `>=`, `/=`, `equal`

### Logic
`and`, `or`, `not`

### Bitwise
`logand`, `logior`, `logxor`, `lognot`, `ash`

### Predicates
`zerop`, `plusp`, `minusp`, `evenp`, `oddp`, `null`

### Control Flow
`if`, `cond`, `case`, `when`, `unless`, `progn`, `begin`, `let`, `let*`

### Functions and Macros
`lambda`, `defun`, `defmacro`, `setq`, `incf`, `decf`

## Limitations

- Currently supports fixnum operations only
- No runtime integration yet (cons, symbols, strings, arrays)
- Interpreter-based evaluation (not compiled)
- Terminal must support ANSI escape codes

## Future Enhancements

- Integration with runtime (heap-allocated objects)
- Pretty-printing of results
- Syntax highlighting
- Parenthesis matching
- Multi-line input support
- Debugger integration
- File loading commands
