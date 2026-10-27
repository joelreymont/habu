\ build-support.f - shared paths, child environment and whole-source scratch.
\ Ordinary app builds use this surface without loading bootstrap orchestration.
\ The BUILD-FIXPOINT package preserves the existing BF-* API and shares one
\ set of helpers and buffers with tools/build-fixpoint.f.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/source.f

package BUILD-FIXPOINT

create BF-ENGINE-BUF FS-PATH-CAP allot
create BF-TMP-BUF FS-PATH-CAP allot

variable BF-ART-PATH-A
variable BF-OUT-PATH-A
variable BF-A-PATH-A
variable BF-B-PATH-A
variable BF-SOURCE-BUF-A
variable BF-SOURCE-CAP-V
TYPED-VARIABLE BF-SOURCE-PATH-A ptr u8
variable BF-SOURCE-PATH-U
variable BF-SOURCE-LEN
variable BF-A-LEN
variable BF-B-LEN
variable BF-TMP-U
variable BF-ENGINE-U

: BF-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BF-PTR-U8@ ( ptr a -- ptr u8 )
   BF-PTR-U8-FIELD @ ;

: BF-PTR-U8! ( ptr u8 ptr a -- )
   BF-PTR-U8-FIELD ! ;

: BF-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: BF-BUF ( ptr a n -- ptr u8 )
   {: slot:ptr cap :}
   slot @ 0= if cap BF-ALLOC-BUF slot BF-PTR-U8! then
   slot BF-PTR-U8@ ;

: BF-SOURCE-CAP ( -- n )
   BF-SOURCE-CAP-V @ ;

\ Process-owned scratch reused by source assembly and certification.
\ Start at one allocator granule and grow to hold whole source files.
: BF-SOURCE-BUF ( -- ptr u8 )
   BF-SOURCE-BUF-A @ 0= if
      MEM-64K MEM-ALLOC-64K-SPAN {: buf:ptr got:n :}
      buf BF-SOURCE-BUF-A BF-PTR-U8!
      got BF-SOURCE-CAP-V !
   then
   BF-SOURCE-BUF-A BF-PTR-U8@ ;

\ A capacity failure names the file and the required and available bytes.
: BF-SOURCE-REFUSED ( ptr u8 n n n -- )
   {: path:ptr pathu:n need:n have:n :}
   s" build-fixpoint: cannot grow the source buffer for " type path pathu type cr
   s" build-fixpoint: needed " type need .
   s" build-fixpoint: have " type have .
   E-FS-CAPACITY throw ;

\ The whole-source reader fills this buffer over several reads. Copy its held
\ prefix before installing the new mapping, then release the old one. A failed
\ allocation leaves the prior span and its bytes owned and intact.
\
\ Callers reacquire BF-SOURCE-BUF after each read; no pointer into this buffer
\ may survive a grow. The bootstrap append helpers follow the same rule.
: BF-SOURCE-ENSURE ( ptr u8 n n -- )
   {: path:ptr pathu:n need:n :}
   need BF-SOURCE-CAP <= if exit then
   need MEM-ALLOC-64K-SPAN {: buf:ptr got:n :}
   got need < if path pathu need got BF-SOURCE-REFUSED then
   BF-SOURCE-BUF-A BF-PTR-U8@ {: old:ptr :}
   BF-SOURCE-CAP {: oldcap:n :}
   old buf oldcap BYTE-COPY
   buf BF-SOURCE-BUF-A BF-PTR-U8!
   got BF-SOURCE-CAP-V !
   oldcap 0 > if old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES then ;

: BF-SOURCE-ROOM ( n -- ptr u8 )
   BF-SOURCE-BUF drop
   BF-SOURCE-PATH-A @ BF-SOURCE-PATH-U @ rot BF-SOURCE-ENSURE
   BF-SOURCE-BUF ;

: BF-PATH-BUF ( ptr a -- ptr u8 )
   FS-PATH-CAP BF-BUF ;

: BF-ART-PATH ( -- ptr u8 )
   BF-ART-PATH-A BF-PATH-BUF ;

: BF-OUT-PATH ( -- ptr u8 )
   BF-OUT-PATH-A BF-PATH-BUF ;

: BF-A-PATH ( -- ptr u8 )
   BF-A-PATH-A BF-PATH-BUF ;

: BF-B-PATH ( -- ptr u8 )
   BF-B-PATH-A BF-PATH-BUF ;

\ The root is copied, as BF-ENGINE! copies the engine: every later BF-A$ joins
\ under it, and a caller's span need not outlive the call - MAKE-TEMP-DIR's is
\ replaced by the task's next MAKE-TEMP-DIR.
: BF-TMP! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 < if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-TMP-BUF u BYTE-COPY
   u BF-TMP-U ! ;

: BF-TMP-OVERRIDE$ ( -- ptr u8 n )
   BF-TMP-BUF BF-TMP-U @ ;

: BF-TMP-RESET ( -- )
   0 BF-TMP-U ! ;

: BF-TMP$ ( -- ptr u8 n )
   BF-TMP-U @ 0 > if BF-TMP-OVERRIDE$ exit then
   s" TMPDIR" GETENV dup 0= if drop drop s" /tmp" then ;

: BF-TMP> ( ptr u8 n ptr u8 -- n )
   {: name:ptr nameu dst:ptr :}
   BF-TMP$ {: root:ptr rootu :}
   rootu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu dst JOIN-PATH ;

: BF-ART$ ( ptr u8 n -- ptr u8 n )
   BF-ART-PATH BF-TMP> BF-ART-PATH swap ;

: BF-OUT$ ( ptr u8 n -- ptr u8 n )
   BF-OUT-PATH BF-TMP> BF-OUT-PATH swap ;

: BF-A$ ( ptr u8 n -- ptr u8 n )
   BF-A-PATH BF-TMP> BF-A-LEN !
   BF-A-PATH BF-A-LEN @ ;

: BF-B$ ( ptr u8 n -- ptr u8 n )
   BF-B-PATH BF-TMP> BF-B-LEN !
   BF-B-PATH BF-B-LEN @ ;

: BF-EXPECT-PATH ( ptr u8 n -- )
   {: path:ptr pathu :}
   pathu 0 <= if E-BUILD-PATH throw then
   path pathu FILE? 0= if E-BUILD-PATH throw then ;

: BF-EXPECT ( ptr u8 n -- )
   BF-ART$ BF-EXPECT-PATH ;

: BF-REMOVE-TMP ( ptr u8 n -- )
   {: a:ptr u :}
   a u BF-A$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: BF-PREPARE-ENV ( -- )
   PROC-ENV-RESET
   s" TMPDIR" >LEN BF-TMP$ >LEN PROC-ENV+ ;

: BF-READ-SOURCE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   path BF-SOURCE-PATH-A !
   pathu BF-SOURCE-PATH-U !
   path pathu size [: BF-SOURCE-ROOM ;] SOURCE:READ-WHOLE-SAMPLED
      BF-SOURCE-LEN ! ;

: BF-ENGINE! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-ENGINE-BUF u BYTE-COPY
   u BF-ENGINE-U ! ;

: BF-ENGINE-SELECTED? ( -- bool )
   BF-ENGINE-U @ 0 > if true exit then
   s" HABU_FIXPOINT_ENGINE" GETENV nip 0 > ;

: BF-ENGINE-RESET ( -- )
   0 BF-ENGINE-U ! ;

: BF-ENGINE$ ( -- ptr u8 n )
   BF-ENGINE-U @ 0 > if BF-ENGINE-BUF BF-ENGINE-U @ exit then
   s" HABU_FIXPOINT_ENGINE" GETENV dup 0 > if exit then drop drop
   s" bin/hb" ;

public

EXPORT BF-A$
EXPORT BF-B$
EXPORT BF-ENGINE!
EXPORT BF-ENGINE-SELECTED?
EXPORT BF-ENGINE$
EXPORT BF-ENGINE-RESET
EXPORT BF-EXPECT
EXPORT BF-OUT$
EXPORT BF-PREPARE-ENV
EXPORT BF-READ-SOURCE
EXPORT BF-REMOVE-TMP
EXPORT BF-SOURCE-BUF
EXPORT BF-SOURCE-CAP
EXPORT BF-SOURCE-LEN
EXPORT BF-TMP$
EXPORT BF-TMP!
EXPORT BF-TMP-RESET

;package
