\ content-key-test.f - focused tests for content-key folds and file keys.
\ Run: bin/hb --load lib/content-key-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f

\ White-box test: reopen the module's package so the fixtures reach the private
\ E-CK-STALE code and call the public builders by their bare package-local names.
package CONTENT-KEY

64 constant CKT-KEY-LEN

variable CKT-ROOT-U
variable CKT-SRC-U

create CKT-ROOT FS-PATH-CAP allot
create CKT-SRC FS-PATH-CAP allot
create CKT-COPY-PATH FS-PATH-CAP allot
create CKT-KEY1 80 allot
create CKT-KEY2 80 allot
create CKT-SEQ-A 80 allot
create CKT-SEQ-B 80 allot
create CKT-INT-A 80 allot
create CKT-INT-B 80 allot

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-SRC$ ( -- ptr u8 n )
   CKT-SRC CKT-SRC-U @ ;

: CKT-SETUP ( -- )
   CLEANUP-RESET
   s" habu-content-key" HB-TMP-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" src.f" CKT-SRC CKT-SRC-U CKT-PATH! ;

: CKT-CLEANUP ( -- )
   CLEANUP-RUN
   CKT-ROOT$ EXISTS? TFALSE ;

\ ---- overlapping folds ------------------------------------------------------
\ The regression this module's fold handles exist for. Two folds run one after
\ the other, then the SAME two folds run overlapping - the second opened while
\ the first is still being folded into, which is what a key derived inside
\ another key's derivation does. The two must agree: a fold's bytes belong to
\ its own handle, not to whoever folded last.
\
\ Against the single shared accumulator this module used to have, they did not:
\ the overlapping run produced ONE key and handed the same wrong value back for
\ both folds. Restoring that behaviour - one slot, never released - turns these
\ equalities red and flips the "the two keys differ" guard to true, which is the
\ silent-mixing signature itself.
: CKT-FOLD-SEQUENTIAL ( -- )
   OPEN
   s" alpha-1" TEXT+
   s" alpha-2" TEXT+
   CKT-SEQ-A FINAL-HEX
   OPEN
   s" beta-1" TEXT+
   s" beta-2" TEXT+
   CKT-SEQ-B FINAL-HEX ;

: CKT-FOLD-OVERLAPPED ( -- )
   OPEN
   s" alpha-1" TEXT+
   OPEN
   s" beta-1" TEXT+
   swap
   s" alpha-2" TEXT+
   swap
   s" beta-2" TEXT+
   CKT-INT-B FINAL-HEX
   CKT-INT-A FINAL-HEX ;

: CKT-FOLD-OVERLAP-MATCHES ( -- )
   CKT-FOLD-SEQUENTIAL
   CKT-FOLD-OVERLAPPED
   CKT-SEQ-A CKT-KEY-LEN CKT-INT-A CKT-KEY-LEN STR= TTRUE
   CKT-SEQ-B CKT-KEY-LEN CKT-INT-B CKT-KEY-LEN STR= TTRUE
   \ and the two keys are genuinely different keys, so the equalities above are
   \ not both passing on one repeated value.
   CKT-SEQ-A CKT-KEY-LEN CKT-SEQ-B CKT-KEY-LEN STR= TFALSE
   CKT-INT-A CKT-KEY-LEN CKT-INT-B CKT-KEY-LEN STR= TFALSE ;

\ A handle is done when its key is taken: reusing it names a slot it no longer
\ owns, and that throws rather than folding into whatever holds the slot now.
\ The stale copy is parked in a typed cell because `catch` takes a ( -- )
\ quotation, so the retry cannot be handed its handle on the stack.
1 LAYOUT-BUFFER CKT-STALE-BUF fold

: CKT-STALE! ( fold -- )
   0 CKT-STALE-BUF ! ;

: CKT-STALE@ ( -- fold )
   0 CKT-STALE-BUF @ ;

: CKT-STALE-USE ( -- )
   CKT-STALE@ s" delta" TEXT+ drop ;

: CKT-FOLD-STALE-THROWS ( -- )
   OPEN dup CKT-STALE!
   s" gamma" TEXT+ CKT-SEQ-A FINAL-HEX
   [: CKT-STALE-USE ;] catch E-CK-STALE T= ;

\ Every fold slot is released by FINAL, so a run that opens and finishes folds
\ forever never exhausts the pool.
: CKT-FOLD-SLOTS-RECYCLE ( -- )
   0 begin dup FOLDS 2 * < while
      OPEN s" recycle" TEXT+ CKT-SEQ-A FINAL-HEX
      1+
   repeat drop
   0 FOLD-FILL 0 T= ;

: CKT-LOGICAL-NAME ( -- )
   CKT-ROOT$ s" copy.f" CKT-COPY-PATH JOIN-PATH {: copyu:n :}
   CKT-SRC$ s" first" WRITE-ALL
   CKT-SRC$ CKT-COPY-PATH copyu COPY-FILE-STREAM
   OPEN CKT-SRC$ s" source.f" FILE-NAMED+ CKT-KEY1 FINAL-HEX
   OPEN CKT-COPY-PATH copyu s" source.f" FILE-NAMED+ CKT-KEY2 FINAL-HEX
   s" one logical name and content ignore the physical location" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$=
   OPEN CKT-COPY-PATH copyu s" renamed.f" FILE-NAMED+ CKT-KEY2 FINAL-HEX
   s" different logical names remain different inputs" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<>
   CKT-COPY-PATH copyu s" second" WRITE-ALL
   OPEN CKT-COPY-PATH copyu s" source.f" FILE-NAMED+ CKT-KEY2 FINAL-HEX
   s" the named file is still read from its physical path" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$<>
   OPEN CKT-SRC$ FILE+ CKT-KEY1 FINAL-HEX
   OPEN CKT-SRC$ CKT-SRC$ FILE-NAMED+ CKT-KEY2 FINAL-HEX
   s" FILE+ retains its path-as-name contract" T-LABEL
   CKT-KEY1 CKT-KEY-LEN CKT-KEY2 CKT-KEY-LEN T$= ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-FOLD-OVERLAP-MATCHES
   CKT-FOLD-STALE-THROWS
   CKT-FOLD-SLOTS-RECYCLE
   CKT-SETUP
   CKT-LOGICAL-NAME
   CKT-CLEANUP
   T-REPORT ;

CKT-MAIN

;package
