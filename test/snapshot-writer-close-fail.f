\ snapshot-writer-close-fail.f - arm the test-only snapshot close seam.
\
\ Builder-only fixture: the snapshot suite injects this source ahead of the snap
\ driver (BF-EMIT-SNAP-RUN-SOURCE-WITH). It requires snap-lib.f (live by this
\ point in the snap source) and arms SNAP-CLOSE-SEAM:BEFORE with a handler that
\ closes the snapshot output descriptor early. SNAP-WRITE-BYTES then runs its own
\ close-rc on the now-invalid descriptor, which reports failure, so the writer
\ must fail closed (die 74 "snap: output close failed") instead of accepting a
\ half-written image. snap.f undefines INSTALL-TEST after this arm, so no normal
\ or shipping build can reach the seam.

require src/habu/snap-lib.f

package SNAP-WRITER-CLOSE-FAIL

: CLOSE-EARLY ( n -- )
   close ;

: ARM ( -- )
   [: CLOSE-EARLY ;] SNAP-CLOSE-SEAM:INSTALL-TEST ;

ARM

;package
