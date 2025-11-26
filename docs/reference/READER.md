# Habu Reader

The Habu reader parses Lisp source code into S-expressions. It is implemented entirely in Habu Lisp (common/reader.lisp).

## Quick Start

```lisp
;; Read a single expression from a string
(read-from-string "(+ 1 2)")
;; => (+ 1 2)

;; Read all expressions from a string
(read-all-from-string "(defun foo () 1) (foo)")
;; => ((DEFUN FOO NIL 1) (FOO))

;; Read all expressions from a file
(read-source-file "my-program.lisp")
;; => (list of forms)
```

## API Reference

### `(read-from-string string)`
Parse and return the first S-expression from STRING.

```lisp
(read-from-string "42")        ;; => 42
(read-from-string "'foo")      ;; => (QUOTE FOO)
(read-from-string "(a b c)")   ;; => (A B C)
```

### `(read-all-from-string string)`
Parse and return all S-expressions from STRING as a list.

```lisp
(read-all-from-string "1 2 3")
;; => (1 2 3)

(read-all-from-string "(defun f () 1) (f)")
;; => ((DEFUN F NIL 1) (F))
```

### `(read-source-file path)`
Read all forms from a file at PATH.

```lisp
(read-source-file "my-file.lisp")
;; => (list of all forms in file)
```

## Supported Syntax

### Numbers

| Syntax | Example | Result |
|--------|---------|--------|
| Decimal integers | `42`, `-17` | 42, -17 |
| Hexadecimal | `#x2A`, `#X1F` | 42, 31 |

### Symbols

Symbols are automatically uppercased for CL compatibility:

```lisp
(read-from-string "foo")   ;; => FOO
(read-from-string "Foo")   ;; => FOO
(read-from-string "FOO")   ;; => FOO
```

### Strings

Double-quoted strings with escape sequences:

```lisp
(read-from-string "\"hello\"")      ;; => "hello"
(read-from-string "\"line1\\nline2\"")  ;; => "line1\nline2"
```

Supported escapes: `\\`, `\"`, `\n`, `\t`

### Lists

Proper and improper lists:

```lisp
(read-from-string "(a b c)")      ;; => (A B C)
(read-from-string "(a . b)")      ;; => (A . B)
(read-from-string "(a b . c)")    ;; => (A B . C)
```

### Quote Forms

| Syntax | Expansion |
|--------|-----------|
| `'x` | `(quote x)` |
| `` `x `` | `(backquote x)` |
| `,x` | `(unquote x)` |
| `,@x` | `(unquote-splicing x)` |

### Reader Macros

| Syntax | Meaning |
|--------|---------|
| `#x42` | Hexadecimal number (66) |
| `#'fn` | `(function fn)` |
| `#\A` | Character literal (65) |
| `#\Space` | Space character (32) |
| `#\Newline` | Newline character (10) |

### Comments

```lisp
;; Line comments start with semicolon
(+ 1 2)  ; inline comment
```

## Implementation Details

### Architecture

The reader is implemented in common/reader.lisp using pure Habu:

1. **Character predicates**: `whitespace?`, `digit?`, `alpha?`, `symbol-char?`
2. **Tokenizers**: `read-int`, `read-sym`, `read-str`
3. **Parser**: `habu-read`, `read-list`
4. **Public API**: `read-from-string`, `read-all-from-string`

### Key Functions

| Function | Purpose |
|----------|---------|
| `skip-ws` | Skip whitespace and comments |
| `read-int` | Parse integer (decimal) |
| `read-hex` | Parse hexadecimal number |
| `read-sym` | Parse symbol (uppercased) |
| `read-str` | Parse string literal |
| `read-list` | Parse list expression |
| `habu-read` | Main dispatch for reading one form |

### Character Classification

```lisp
(whitespace? ch)  ;; space, tab, newline, return
(digit? ch)       ;; 0-9
(hex-digit? ch)   ;; 0-9, a-f, A-F
(alpha? ch)       ;; a-z, A-Z
(symbol-char? ch) ;; alphanumeric + special chars
```

## Symbol Interning

Symbols are interned for identity comparison:

```lisp
(eq 'foo 'foo)              ;; => T
(eq (read-from-string "foo")
    (read-from-string "foo")) ;; => T
```

The runtime uses a hash table for symbol interning (runtime/gc.c).

## Integration with Compiler

The reader produces standard S-expressions that can be passed directly to the compiler:

```lisp
;; Read source and compile
(let ((forms (read-source-file "program.lisp")))
  (compile-program forms))
```

## Error Handling

The reader does not currently signal detailed errors. Malformed input may result in:
- NIL return for empty/whitespace input
- Partial reads for incomplete expressions
- Runtime errors for invalid syntax

## Examples

### Reading a Complete Program

```lisp
(defparameter *source* "
  (defun square (x)
    (* x x))

  (defun sum-squares (a b)
    (+ (square a) (square b)))

  (sum-squares 3 4)
")

(read-all-from-string *source*)
;; => ((DEFUN SQUARE (X) (* X X))
;;     (DEFUN SUM-SQUARES (A B) (+ (SQUARE A) (SQUARE B)))
;;     (SUM-SQUARES 3 4))
```

### Reading with Quote Forms

```lisp
(read-from-string "'(1 2 3)")
;; => (QUOTE (1 2 3))

(read-from-string "`(a ,b ,@c)")
;; => (BACKQUOTE (A (UNQUOTE B) (UNQUOTE-SPLICING C)))
```

### Reading Nested Structures

```lisp
(read-from-string "((a b) (c (d e)) f)")
;; => ((A B) (C (D E)) F)
```

## Limitations

1. **No package prefixes**: `pkg:symbol` not supported
2. **No read-time eval**: `#.` not implemented
3. **Limited character names**: Only `Space`, `Newline`, `Tab` supported
4. **No arrays**: `#(...)` vector syntax not implemented
5. **No ratios/floats in reader**: Only integers supported directly

## Future Enhancements

- Package-qualified symbols (`pkg:sym`, `pkg::sym`)
- Read-time evaluation (`#.`)
- Vector literals (`#(1 2 3)`)
- Pathname literals (`#p"..."`)
- Structure literals (`#s(...)`)
