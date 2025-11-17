# Habu

A Common Lisp implementation for bare-metal ARM64 systems with real-time constraints.

## Features

- Sub-millisecond latency for control loops
- GC-free execution mode using region allocators
- Incremental generational GC for background tasks
- Direct compilation to ARM64 machine code
- Minimal runtime footprint

## Building

```bash
make              # Build runtime and tests
make test         # Run tests
make benchmark    # Run benchmarks
```

## Requirements

- GCC or Clang with ARM64 cross-compilation support
- SBCL (for bootstrap compiler)
- Make
- QEMU (optional, for testing)

## Usage

```lisp
;; GC-free control loop
(defun control-loop ()
  (with-gc-disabled
    (with-region temp
      (loop
        (let ((sensors (read-sensors temp)))
          (process sensors))
        (reset-region temp)))))

;; Normal GC-managed code
(defun background-task ()
  (let ((data (collect-data)))
    (process-data data)))
```

## Project Status

Early development. See DESIGN.md for architecture details.

## License

To be determined.
