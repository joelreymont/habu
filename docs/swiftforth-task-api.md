# SwiftForth Task API Capture

Source: local SwiftForth Reference Manual, Multitasking section 7.2 and index
entries for `#USER`, `+USER`, `ACTIVATE`, `CONSTRUCT`, `HALT`, `HIS`, `KILL`,
`PAUSE`, `RELEASE`, and `TASK`. This file records the tasking surface Habu is
tracking; it is not a copy of the manual.

## Captured Surface

| SwiftForth | SwiftForth effect | Habu public word | Habu effect |
| --- | --- | --- | --- |
| `TASK <name>` | `u --` | `TASK:TASK` | `n --` |
| `CONSTRUCT` | `addr --` | `TASK:PREPARE` | `ptr a --` |
| `ACTIVATE` | `addr --` plus following source body | `TASK:ACTIVATE` | `n ptr a --` |
| `PAUSE` | `--` | `TASK:PAUSE` | `--` |
| `HALT` | `addr --` | `TASK:HALT` | `ptr a --` |
| `KILL` | `addr --` | `TASK:KILL` | `ptr a --` |
| `#USER` | `-- n` | `TASK:#USER` | `-- n` |
| `+USER` | `n --` style defining word | `TASK:+USER` | `n n -- n` |
| `HIS` | `addr1 addr2 -- addr3` | `TASK:HIS` | `ptr a ptr a -- ptr a` |
| `GET` | `addr --` | `TASK:GET` | `ptr a --` |
| `RELEASE` | `addr --` | `TASK:RELEASE` | `ptr a --` |

## Habu Deliberate Differences

- Habu uses package-qualified public names. The outside spelling is
  `TASK:KILL`, not a global `KILL`, because the task API belongs to package
  `TASK`.
- Habu `TASK:ACTIVATE` takes an already-compiled XT and a task control block.
  SwiftForth's `ACTIVATE` parses the following source text as the task body.
  The XT form keeps task startup checked: the task body is compiled and typed
  before it can be passed to a worker.
- Habu `TASK:+USER` takes the current offset and allocation size and returns
  the next offset. This keeps user-storage layout explicit and checked while
  preserving the SwiftForth model that a user word resolves through the current
  task's data base.
- Habu spells SwiftForth's `CONSTRUCT` as `TASK:PREPARE`. The bare token
  `construct` is reserved as a type-families language keyword (dictionary lookup
  is case-folded, so `CONSTRUCT` would collide), so the task word that allocates
  and initializes a TCB without starting it uses the role-accurate name
  `TASK:PREPARE`.
- Habu facilities track the owning task token around the underlying pthread
  mutex. `TASK:GET` is a no-op when the current task already owns the facility;
  `TASK:RELEASE` is a no-op when the current task does not own it. That matches
  the SwiftForth ownership contract and avoids exposing raw pthread unlock
  behavior to callers.

## Implemented Coverage

`lib/task.f` implements the full captured surface above. `lib/task-test.f`
proves task creation, start/join/kill, cooperative halt, task-local user
storage, `HIS`, facility owner semantics, worker FFI calls, atomics, an
application-shaped five-task repeated soak, live-task compile guards, and fail-closed
process termination when a task dies or throws.
