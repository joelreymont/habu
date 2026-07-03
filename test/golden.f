\ golden.f - byte-exact golden-file assertions for diagnostic output.
\
\ Captured diagnostic text is normalized (a caller-supplied volatile prefix,
\ e.g. a temp root, is redacted to <root>) then compared byte-exact against a
\ committed golden under test/golden/. --update-golden (scanned from
\ SCRIPT-ARGV by GOLD:INIT) rewrites the golden instead of comparing, and a
\ mismatch prints the first differing byte plus a window of both sides.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and lib/fs-mutate.f.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f

package GOLD

$10000 constant CAP
64 constant WINDOW

create NORM-BUF CAP allot
create READ-BUF CAP allot
create PATH-BUF FS-PATH-CAP allot
create REDACT-BUF FS-PATH-CAP allot
create ROOT-BUF FS-PATH-CAP allot
variable NORM-U
variable READ-U
variable PATH-U
variable REDACT-U
variable ROOT-U
variable UPDATE

\ Golden root defaults to the committed test/golden tree; ROOT! redirects it
\ (tests point it at a temp dir).
: DIR$ ( -- ptr u8 n )
   ROOT-U @ 0 > if ROOT-BUF ROOT-U @ exit then
   s" test/golden" ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: NORM$ ( -- ptr u8 n )
   NORM-BUF NORM-U @ ;

: READ$ ( -- ptr u8 n )
   READ-BUF READ-U @ ;

: REDACT$ ( -- ptr u8 n )
   REDACT-BUF REDACT-U @ ;

: FALSE ( -- bool )
   0 0= 0= ;

: TRUE ( -- bool )
   0 0= ;

: NORM-C+ ( n -- ) {: c:n :}
   NORM-U @ CAP >= if E-STR-CAPACITY throw then
   c NORM-BUF NORM-U @ + c!
   NORM-U @ 1+ NORM-U ! ;

: NORM-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ NORM-C+
      1+
   repeat drop ;

\ Does the redact prefix start at byte i of a[0..u)?
: REDACT-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   REDACT-U @ 0= if FALSE exit then
   i REDACT-U @ + u > if FALSE exit then
   a i + REDACT-U @ REDACT$ STR= ;

: NORMALIZE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 NORM-U !
   0 begin dup u < while
      {: i:n :}
      a u i REDACT-AT? if
         s" <root>" NORM-BYTES+
         i REDACT-U @ +
      else
         a i + c@ NORM-C+
         i 1+
      then
   repeat drop ;

: PATH! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   DIR$ name nameu PATH-BUF JOIN-PATH PATH-U ! ;

: UPDATE-WRITE ( -- )
   DIR$ MAKE-DIRS
   PATH$ NORM$ WRITE-ALL
   s" golden updated: " type PATH$ type cr ;

: MINLEN ( -- n )
   NORM-U @ READ-U @ 2dup > if nip else drop then ;

: DIFF-OFF ( -- n )
   0 begin dup MINLEN < while
      dup NORM-BUF + c@ over READ-BUF + c@ <> if exit then
      1+
   repeat ;

\ Print up to WINDOW bytes of a[off..] so the delta is visible in the log.
: WINDOW. ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   off u >= if exit then
   u off - {: rem:n :}
   rem WINDOW > if WINDOW else rem then {: len:n :}
   a off + len type ;

: MISMATCH. ( -- )
   DIFF-OFF {: off:n :}
   s" golden mismatch: " type PATH$ type cr
   s"   first diff at byte " type off . cr
   s"   golden: " type READ$ off WINDOW. cr
   s"   actual: " type NORM$ off WINDOW. cr ;

public

: UPDATE? ( -- bool )
   UPDATE @ 0 <> ;

: UPDATE! ( n -- )
   UPDATE ! ;

: INIT ( -- )
   0 UPDATE !
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ s" --update-golden" STR= if -1 UPDATE ! then
      1+
   repeat drop ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

\ Set the volatile prefix (temp root) to redact to <root>; empty clears it.
: REDACT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a REDACT-BUF u BYTE-COPY
   u REDACT-U ! ;

: REDACT-CLEAR ( -- )
   0 REDACT-U ! ;

\ Normalize captured, then in update mode rewrite the golden and pass; in
\ compare mode return byte-exact equality (printing the delta on mismatch, and
\ failing when the golden is absent). Callers integrate the result into their
\ own failure path (GE-FAIL/die or lib/test assertions).
: CHECK ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu:n name:ptr nameu:n :}
   src srcu NORMALIZE
   name nameu PATH!
   UPDATE? if UPDATE-WRITE TRUE exit then
   PATH$ EXISTS? 0= if
      s" missing golden (run with --update-golden): " type PATH$ type cr
      FALSE exit
   then
   PATH$ READ-BUF CAP READ-ALL READ-U !
   NORM$ READ$ STR= dup 0= if MISMATCH. then ;

end-package
