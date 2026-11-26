\ kernel-format.f - the native kernel object's framing, shared by the arm64
\ and x86-64 writers.
\
\ The object: the bytes HBKERNEL on arm64 or HBKERNX6 on x86-64, the SHA-256 of
\ every byte after it, then the sections in this order. Every number is a u64,
\ low byte first; a name is its length, then its bytes padded with zeros to a
\ multiple of 8.
\   code        its length in bytes, then the bytes padded as a name's
\   labels      n, then each label's position, -1 while unbound
\   references  n, then each one's site, label and kind
\   data spans  n, then each span's first and end position; x86-64 adds the
\               last byte of each instruction control never passes on from
\   fixups      n, then each pending one's site, label and kind
\   primitives  n, then each one's first label, last label, wordlist and name
\   scope sites n, then the label of scope-kind?'s chain for each kind from 2
\   symbols     n, then each one's name and value
\   presets     n, then each header cell's offset and the value the link writes
\   Mach-O imports their length in bytes, then the C names the GOT binds in row
\               order, each ending in a NUL, padded as a name's
\   ELF imports    the same for an ELF image's GOT
\   checker rows their length in bytes, then n, then each row's kind,
\               trusted-only, fixed-ABI, name, package name, atom count and
\               atoms
\ A position counts words on arm64 and bytes on x86-64.
require src/lib/le.f
require src/lib/string.f
require src/core/bytes.f
require src/core/sha256.f
require src/habu/prims.f

package KERNEL-OBJECT
private

DYNAMIC-BUFFER OUT u8                \ the object being written
variable OUT-U
variable COUNT-AT                    \ the open section's count, as an offset in OUT
variable COUNT-N
SHA256-CTX-BYTES BUFFER: SHA-CTX


: U64+ ( n -- )
   OUT-U @ 8 + OUT-RESERVE
   0 OUT OUT-U @ + LE:U64!
   8 OUT-U +! ;

: BYTES+ ( str -- )
   {: s:str :}
   OUT-U @ s STR:LEN + OUT-RESERVE
   s STR:DATA  0 OUT OUT-U @ +  s STR:LEN BYTE-COPY
   s STR:LEN OUT-U +! ;

: BYTE+ ( n -- )
   OUT-U @ 1 + OUT-RESERVE
   0 OUT OUT-U @ + c!  1 OUT-U +! ;

\ Zeros up to the next multiple of 8.
: PAD+ ( -- ) begin OUT-U @ 7 and 0<> while 0 BYTE+ repeat ;

: SPAN+ ( str -- ) BYTES+ PAD+ ;

: NAME+ ( str -- )
   {: s:str :}
   s STR:LEN U64+  s SPAN+ ;


: SECTION ( -- ) OUT-U @ COUNT-AT !  0 COUNT-N !  0 U64+ ;
: ENTRY ( -- ) 1 COUNT-N +! ;
: ;SECTION ( -- ) COUNT-N @  0 OUT COUNT-AT @ + LE:U64! ;


: SYM+ ( str n -- )
   {: s:str v:n :}
   s NAME+  v U64+  ENTRY ;

: IMPORTS+ ( n [ n -- str ] -- )
   {: n:n name :}
   OUT-U @ {: at:n :}
   0 U64+
   n 0 ?do i name execute BYTES+  0 BYTE+ loop
   OUT-U @ at - 8 -  0 OUT at + LE:U64!
   PAD+ ;


: CHECK-ROWS+ ( -- )
   OUT-U @ {: at:n :}
   0 U64+
   [: BYTES+ ;] PRIM-SPEC:ROWS-WRITE
   OUT-U @ at - 8 -  0 OUT at + LE:U64! ;


: SUM ( -- )
   SHA-CTX  0 OUT 40 +  OUT-U @ 40 - >str  0 OUT 8 +  SHA256-IN ;

;package
