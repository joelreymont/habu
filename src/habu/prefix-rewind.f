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
\ can name neither. NOT spelled `seed-ndict!`: Habu folds case, so a tail spelled
\ like the engine word it wraps IS that word inside this package block and the
\ body would call itself.
\
\ `seed-ndict!` AND NOT `ndict!`, because a build host reaches this rewind with
\ its own seal floor already armed: a seeded engine arms it at startup
\ (habu2.f EM-SEAL-SEEDED-RUNTIME) and a cold one at its prefix end, and public
\ `ndict!` refuses every count below that floor (habu1.f BNDSET, exit
\ ENGINE-ERROR:SEAL-VIOLATION - silently, so the symptom is a build child that
\ dies with no diagnostic). `seed-ndict!` is the engine's lowering seam: it
\ refuses a raise, guards the record span it redirects the next write to, and
\ clears the floor as one operation, which is what lets the recompiled prefix
\ reopen the engine's own packages below the mark. tools/native-build.f
\ LOGICAL-RESET drives the same seam for the in-process window build.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: DICT! ( n -- ) seed-ndict! ;
defer BOUND! ( -- )
TRUSTED: BIND-BOUND ( -- )
   s" CHECKER-BOUND:REWIND" XREF-FIND
   dup XREF-FOUND? 0= if drop s" prefix rewind: checker boundary missing" 76 die then
   dup XREF-RETIRED? if drop s" prefix rewind: checker boundary retired" 76 die then
   XREF-START dup 0= if drop s" prefix rewind: checker boundary has no code" 76 die then
   is BOUND! ;
BIND-BOUND

\ The rewind is a pre-hook definition, so an older host may carry its code
\ without a native call model. The protected package owns its dictionary
\ record; the trusted binder reads that record into this private typed slot.
\ Ordinary tick and search continue to refuse internal execution tokens.
\ It does not publish a global axiom or a checked-callable rewind capability.

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
\ to the live end of a dictionary this rewind shortened, alongside the truncation
\ it repairs. It is last because it reads the dictionary the lines above it
\ settle, and because it re-arms the floor that DICT! cleared.
: TO-CORE ( -- )
   BOUND!
   PREFIX-MARK:DICT DICT!
   PREFIX-MARK:REQ REQUIRE-REG:TRUNCATE
   SEAL-CAPTURE ;

;package
