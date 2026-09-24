# Raw serial streams

[`lib/serial.f`](../lib/serial.f) provides the generic `SERIAL` package for
Linux and macOS AArch64. It opens raw 8N1 terminal streams with numeric baud
rates and exposes partial byte transfers with finite waits. Application command
framing, retries, boot sequences, and flash policy belong to callers.

## Interface

| Word | Inputs | Result |
| --- | --- | --- |
| `BAUD` | Numeric bits per second, `1..0xFFFFFFFF` | `SERIAL:baud` |
| `BYTES` | Length, `1..0x7FFFF000` | `NUM:byte-len` |
| `OPEN8N1` | Borrowed path `ptr u8 n`, baud | `open-result`: `opened handle`, `failed errno`, or `unsupported` |
| `READ` | Handle, writable byte span, `ms` timeout | `io-result` |
| `WRITE` | Handle, borrowed byte span, `ms` timeout | `io-result` |
| `CLOSE` | Handle | `status`: `ok` or `failed errno` |

A byte span is `ptr u8 NUM:byte-len`. The `SERIAL:handle`, `SERIAL:baud`,
and `SERIAL:errno` types are distinct. Errno values retain positive host error
numbers. Raw nominal converters such as `>BAUD` are not validators; each public
operation checks its operands before I/O.

Paths must contain 1–4095 bytes, without embedded NUL. `OPEN8N1` creates a
nonblocking descriptor with `O_NOCTTY` and close-on-exec. On Linux it requires the normal
`N_TTY` line discipline. On both targets it disables parity, extra stop bits, echo, input/output
translations and software/hardware flow control, and enables `CLOCAL`, `CREAD`
and eight data bits. `VMIN=1` and `VTIME=0`; the AIO loop supplies the wait.
The previous `HUPCL` setting is retained. No explicit modem-line changes or
queue flushes are performed.

Both reported baud rates and the raw settings are read back after configuration.
`unsupported` means the line discipline or resulting settings do not meet this
contract. Linux uses the kernel's numeric termios2 rates; macOS uses numeric
termios speeds and accepts the rates supported by the terminal driver.
Unsupported terminal ioctls and other OS failures return `failed`.
On configuration failure the library attempts to restore the original settings
and closes its descriptor, preserving the first error. Restoration is best
effort; an unplugged device may already be unavailable. Opening a serial device
can itself change modem lines according to its driver.

Successful open transfers descriptor ownership to the caller. Coordinate all
users of the same terminal: settings and queues are shared by its open
handles. `CLOSE` consumes the handle even when it returns an error; never retry
it or reuse the numeric descriptor. The configured terminal settings persist
after a normal close. Handle lifetime is a caller obligation, not a linear proof.

## Byte transfers

`READ` and `WRITE` return one available chunk, which may be shorter than the
requested span. They never hide a partial transfer behind a timeout:

| Variant | Payload | Meaning |
| --- | --- | --- |
| `transferred` | Positive byte length | This many bytes read or accepted into the kernel write queue |
| `timeout` | None | No bytes transferred during the wait |
| `closed` | None | Terminal hangup or zero-byte I/O |
| `failed` | Errno | OS error, including a disconnected device reporting `EIO` |

A read does not discard bytes beyond capacity. Callers assemble their own
messages and advance spans after short transfers. A successful write is not
proof that data reached the peer. Capacity/length must be positive; zero-byte
operations are rejected.

Every wait is one `AIO:POLL` and one `AIO:AWAIT` ([aio.md](aio.md)), so
`AIO:START` must precede the first `READ` or `WRITE` and a wait with no
loop running is `E-AIO-STATE`.

Timeouts are `0..2147483647` milliseconds. Zero makes an immediate readiness
attempt, which a port that already has bytes still answers, because readiness is served before the deadline. Readiness races
retain a monotonic deadline; a signal no longer cuts a wait short, since no
thread is parked in `poll(2)` for one to interrupt.
Scheduling delays and driver behavior can extend elapsed time; this is not a
real-time bound. Calls that assemble several chunks should use one overall
deadline if they require a bounded transaction.

First use registers cache cleanup with
[`IMAGE-LIFECYCLE`](../lib/image-lifecycle.f) before resolving any libc symbol.
A failed resolution retains that registration for cleanup or a later retry.
Quiescent image preparation clears all cached function addresses and the
initialization/registration flags, so the next call resolves and registers them
again. The module acquires no dynamic-library reference to release. Callers must
close their descriptors and stop concurrent use before capture; live descriptors
are process resources, not serializable handles.

This handoff's lifecycle integration is pending native validation. The current
compiler fails while loading the required `TASK` library, and shared registration
must support concurrent initialization by different resource owners. A
fresh-process image regression remains required before shipping saved
applications containing this library. The earlier source-loaded host behavior
is covered by the existing transport tests; it does not prove this integration.

## Implementation and checks

Seven small private `TRUSTED:` bindings describe exact libc calls through
Habu's bounded FFI: `open`, two typed `ioctl` operations, `read`, `write`,
`close`, and `__errno_location`. The wait is not among them: it is the loop's. C `int` returns are normalized from 32 bits;
`ssize_t` retains 64 bits. Writable extents are explicit. All configuration,
validation, argument preparation and result normalization is checked Habu.
The trusted bodies contain only fixed foreign calls and have no locals.

The configuration boundary uses Linux's 44-byte kernel `termios2` and
[`TCGETS2`/`TCSETS2` with `BOTHER`](https://man7.org/linux/man-pages/man2/TCSETS.2const.html).
Those speeds are numeric, independent of changes to libc's `speed_t` constants.
This is intentionally a Linux ABI, not a portable libc `struct termios` layout.
Other operating systems are rejected. Supporting another host ABI requires its
own bindings and tests.

Termios scratch storage is task-local: the working record and the saved one,
`$58` in all. Each open allocates and releases
its own NUL-terminated path. A synchronized first call publishes libc symbols
borrowed through [`RTLD_DEFAULT`](https://man7.org/linux/man-pages/man3/dlsym.3.html).
The native executable already depends on libc; this module acquires no library
reference. Separate tasks may use separate ports and buffers concurrently; calls must not nest within one task.

Invalid operands throw `E-OPERAND` (`-9110`); unsupported OS, missing
symbol, and impossible foreign results throw `E-PLATFORM` (`-9111`), `E-SYMBOL`
(`-9112`), and `E-RESULT` (`-9113`). Ordinary OS failures use result variants.

```sh
python3 test/serial.py
```

The 31 native/ABI cases use the supported Habu loader and independent Python
pseudoterminal peers. They verify the kernel layout against C headers, raw
configuration, all 256 byte values, guarded reads, retained stream remainders,
partial writes and a full output queue, timeouts, hangup, failed-open cleanup,
errors after close, rejected operands and nominal substitutions, and concurrent
first use on two terminals at different baud rates. Scratch files stay under
ignored `tmp/test-serial/`. The harness needs Python, a native C compiler and
Linux pseudoterminals; it needs no physical serial device or root.

These checks do not establish a USB adapter's supported speeds, electrical
behavior, modem-line effects, throughput, or application protocol compatibility.
