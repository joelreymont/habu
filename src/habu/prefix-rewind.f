\ prefix-rewind.f - return a build host to the end of its own core prefix.
\
\ PAYLOAD-ONLY, exactly like src/habu/hide.f: tools/build-fixpoint.f emits this
\ file at the head of every generated engine source and nothing else loads it.
\ That is what keeps the two raw seams below out of a shipped engine - a
\ booted engine has no such word to call, and the seal guard the first of them
\ bypasses (src/habu/xref.f SEAL-DICT-GUARD) therefore still answers for every
\ name a user program can reach.
\
\ WHY IT IS NOT IN hide.f, the file that owns the other rewind. hide.f is
\ deliberately unpackaged - tools/bootstrap-codegen-test.f includes it to drive
\ the BFR-* words as the executable spec for tools/bootstrap.sh's BOOT-* twin,
\ and says so - and the package lint refuses new definitions in an unpackaged
\ file (measured: `BFR-HIDE-TO-CORE` defines a changed module word outside a
\ package). The split lands the concerns where they belong: hide.f is now
\ exactly the recovery host's mirror surface, and the watermark rewind, which
\ the recovery host cannot have, is here.
\
\ src/core/lower-cert-seal.f marks where the core prefix ends: two numbers of
\ its own, and one call asking the checker to record every mark a scope of its
\ own would carry. This returns the host to that moment, leaving every
\ core-prefix definition live rather than orphaning a copy and recompiling the
\ prefix on top of it.

package PREFIX-REWIND

private

\ THE TWO ENGINE SEAMS THIS REWIND DRIVES, one row each, because a checked body
\ can name neither. NOT spelled `ndict!`: Habu folds case, so a tail spelled
\ like the engine word it wraps IS that word inside this package block and the
\ body would call itself.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: DICT! ( n -- ) ndict! ;
TRUSTED: BOUND! ( -- ) CHECKER-BOUND:REWIND ;

\ BOUND! IS A ROW AND NOT AN AXIOM, which is the other way to reach a pre-hook
\ package. A checked body may only name a QUALIFIED word the checker knows, and
\ src/core/checker.f loads before src/core/check-hook.f, so nothing defined
\ there records a signature (measured: certify rc 70, `undefined word` in
\ to-core). checker.f makes such a word known with a PPRIM: axiom and states its
\ preference for that over a TRUSTED shim - but every MUTATING axiom on that
\ list is token-scoped: TYPE-FIELD-OWNER:OPEN mints an opaque token that each
\ later phase must present unchanged, so an arbitrary checked caller holds
\ nothing. CHECKER-BOUND:REWIND takes no token. It puts every mark a checker
\ scope carries back to a boundary recorded once at boot, so a public axiom
\ would hand that truncation to any checked program in every shipped engine -
\ the corruption REG-PROTECT exists to refuse (`99 PF-COMMIT-N !`), handed back
\ through a name. A row in a payload-only file hands it to nobody: measured on a
\ built engine, a checked body naming the seam is E-UNDEFINED.

public

\ FOUR PARTS, IN THIS ORDER. The checker first, because BOUND! is the checker's
\ own boundary seam and it walks the records being discarded before their counts
\ move - the signature store's index heads, the symbol hash index, the signature
\ pool - and every one of those walks needs the dictionary those records still
\ belong to. It is one call and not a list of numbers on purpose: the marks a
\ scope invalidates are checker.f's list, it changes when that file changes, and
\ a copy of it here went stale the first time, carrying four cursor families out
\ of twenty. A warm image built that way segfaulted on its first type
\ declaration. Then the dictionary.
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
\ to the live end of a dictionary this rewind shortened, and it runs here under
\ the build entry's open latch, alongside the truncation it repairs. It is last
\ because it reads the dictionary the lines above it settle.
: TO-CORE ( -- )
   BOUND!
   PREFIX-MARK:DICT DICT!
   PREFIX-MARK:REQ REQUIRE-REG:TRUNCATE
   SEAL-CAPTURE ;

;package
