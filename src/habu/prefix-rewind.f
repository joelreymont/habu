\ prefix-rewind.f - return a build host to the end of its own core prefix.
\
\ PAYLOAD TEXT, NO DEFINITIONS: the four top-level lines at the end of this
\ file are the whole rewind. A build `include`s this file, or appends its text,
\ at its own top level - tools/build-fixpoint.f appends it at the head of every
\ generated engine source - because `seed-ndict!` is a top-level boundary
\ primitive that no checked body can name. Code that must run after the rewind
\ is ticked before it and executed after it: the rewind keeps code and data and
\ removes only records, so the xt survives the name it no longer has.
\
\ src/core/lower-cert-seal.f marks where the core prefix ends: two numbers of
\ its own, and one call asking the checker to record every mark a scope of its
\ own would carry. This returns the host to that moment, leaving every
\ core-prefix definition live rather than orphaning a copy and recompiling the
\ prefix on top of it. Every name below lives under that mark (checker.f,
\ lower-cert-seal.f, include.f and the engine's primitives), so each line still
\ resolves after the line before it lowered the dictionary.
\
\ `seed-ndict!` AND NOT `ndict!`, because a build host reaches this rewind with
\ its own seal floor already armed: a seeded engine arms it at startup
\ (habu2.f EM-SEAL-SEEDED-RUNTIME) and a cold one at its prefix end, and public
\ `ndict!` refuses every count below that floor (habu1.f BNDSET, exit
\ ENGINE-ERROR:SEAL-VIOLATION - silently, so the symptom is a build child that
\ dies with no diagnostic). `seed-ndict!` is the engine's lowering seam: it
\ refuses a raise, guards the record span it redirects the next write to, and
\ clears the floor as one operation, which is what lets the recompiled prefix
\ reopen the engine's own packages below the mark.

\ FOUR PARTS, IN THIS ORDER. The checker first, because the rewind is the
\ checker's own boundary seam and it walks the records being discarded before
\ their counts move - the signature store's index heads, the symbol hash index,
\ the signature pool - and every one of those walks needs the dictionary those
\ records still belong to. It is one call and not a list of numbers on purpose:
\ the marks a scope invalidates are checker.f's list, it changes when that file
\ changes, and a copy of it here went stale the first time, carrying four
\ cursor families out of twenty. A warm image built that way segfaulted on its
\ first type declaration. Then the dictionary.
\
\ Then the include registry, through its own seam for the same reason the
\ signature store has one: the rows above the mark name files whose definitions
\ the dictionary line above just removed, and four cells point INTO those rows.
\ Left alone, ENGINE-PROVIDES? goes on answering yes for them, turning a later
\ `require` into a silent no-op - and a snapshot taken afterwards persists that
\ answer, shipping an image that claims to carry the stdlib it does not
\ (measured, and what test/snapshot-writer.f caught).
\
\ Then the seal floor, because this rewind is what moved it. The floor says
\ "records below this index are the engine's own"; the host captured it at the
\ end of ITS boot, and the lines above just discarded every record from the
\ core-prefix mark upward - so without this the floor stands above the
\ dictionary it describes, and every later FORGET/HIDE in the process is
\ measured against records that no longer exist (measured: the snapshot build's
\ own tail retire died `seal: cannot FORGET/HIDE sealed engine definitions`).
\ SEAL-CAPTURE restates the same cell at the boundary that now exists. It is not
\ a second floor and it does not weaken the guard: it only ever moves the floor
\ to the live end of a dictionary this rewind shortened, alongside the truncation
\ it repairs. It is last because it reads the dictionary the lines above it
\ settle, and because it re-arms the floor that seed-ndict! cleared.
CHECKER-BOUND:REWIND
PREFIX-MARK:DICT seed-ndict!
PREFIX-MARK:REQ REQUIRE-REG:TRUNCATE
SEAL-CAPTURE
