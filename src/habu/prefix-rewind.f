\ prefix-rewind.f - return a build host to the end of its own core prefix, the
\ mark src/core/lower-cert-seal.f records, leaving every core-prefix definition
\ live. Payload text, no definitions: a build `include`s or appends it at top
\ level (tools/build-fixpoint.f at the head of every generated engine source),
\ because no checked body can name seed-ndict!. The rewind keeps code and data and
\ removes only records: code that runs after it is ticked before it.

\ Every name below lives under the mark, so each line resolves after the line
\ before it lowered the dictionary; the order cannot change.
\ The checker first: it walks the records being discarded (signature index heads,
\ symbol hash index, signature pool) before their counts move. One call, because
\ the marks a scope invalidates are checker.f's list; a copy here misses cursor
\ families.
CHECKER-BOUND:REWIND
\ seed-ndict!, not ndict!: the host's seal floor is already armed (habu2.f
\ EM-SEAL-SEEDED-RUNTIME, or a cold engine's prefix end) and ndict! refuses any
\ count below it silently (habu1.f BNDSET, ENGINE-ERROR:SEAL-VIOLATION: a build
\ child dies with no diagnostic). seed-ndict! refuses a raise, guards the record
\ span the next write goes to and clears the floor in one operation, so the
\ recompiled prefix can reopen the engine's own packages below the mark.
PREFIX-MARK:DICT seed-ndict!
\ Four cells point into the include rows above the mark. Left alone,
\ ENGINE-PROVIDES? answers yes for removed files, a later `require` is a silent
\ no-op, and a snapshot ships an image claiming a stdlib it lacks
\ (test/snapshot-writer.f).
PREFIX-MARK:REQ REQUIRE-REG:PREFIX-REWIND
\ The host captured its seal floor at the end of its boot, above the dictionary
\ this rewind shortened, so a later FORGET/HIDE dies `seal: cannot FORGET/HIDE
\ sealed engine definitions`. SEAL-CAPTURE moves the floor to the live end and
\ re-arms what seed-ndict! cleared; it reads the settled dictionary, so it is last.
SEAL-CAPTURE
