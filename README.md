# Habu

A Common Lisp implementation for bare-metal ARM64 systems with real-time constraints.

## Features

- Sub-millisecond latency for control loops
- GC-free execution mode using region allocators
- Incremental generational GC for background tasks
- Direct compilation to native machine code
- Minimal runtime footprint
- Cross-platform development and testing

## Supported Platforms

**Development Platforms:**
- macOS ARM64 (Apple Silicon) - primary development target
- Linux x86_64 - testing and CI

**Deployment Targets:**
- ARM64 bare-metal (planned)
- ARM64 Linux (planned)

## Building

```bash
make              # Build runtime and tests
make test         # Run tests
make benchmark    # Run benchmarks
make examples     # Build example programs
```

### Platform-Specific Builds

```bash
# Native build (auto-detects platform)
make clean && make test

# Cross-compile for ARM64 (from x86_64)
make cross
```

## Requirements

- GCC or Clang
- Make
- SBCL (for bootstrap compiler, planned)

**For cross-compilation:**
- aarch64-linux-gnu-gcc (ARM64 toolchain)

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
