\ Boundary acceptance for the versioned NBR package artifact. The retained
\ path printed below can be inspected with a byte dumper after this run.
\ Failure modes: bad magic/version/arch/name/key, malformed section length,
\ section length past EOF, truncation, and trailing bytes must all refuse; a
\ package name past the header's 64-byte field refuses as a profile the format
\ cannot represent.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f
require tools/native-unit-file.f
require tools/native-file-inputs.f

package NUNIT-FILE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create LONG-SOURCE FS-PATH-CAP allot variable LONG-SOURCE-U
create ROW 80 allot
\ An artifact holds the header plus every SECTIONS label.
1024 constant ART-CAP
create GOOD ART-CAP allot
create RAW ART-CAP allot variable RAW-U

: KEY$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

\ The header ends after its section-length table, which starts at byte 160.
: HEADER ( -- n ) 160 NUNIT-FILE:SECTION-COUNT 8 * + ;

: ART$ ( -- ptr u8 n ) PATH PATH-U @ ;
: LONG-SOURCE$ ( -- ptr u8 n ) LONG-SOURCE LONG-SOURCE-U @ ;
: LONG-CONTENT$ ( -- ptr u8 n ) s" : LONG-SOURCE ( -- n ) 42 ;" ;

: SETUP ( -- )
   s" native-unit-file" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT ROOT-U @ s" nbr.unit" PATH JOIN-PATH PATH-U !
   ROOT ROOT-U @ s" abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnop.f"
      LONG-SOURCE JOIN-PATH LONG-SOURCE-U !
   80 0 ?do i ROW i + c! loop ;

: SECTIONS ( -- )
   NUNIT-FILE:RESET
   0 s" code" NUNIT-FILE:SECTION!
   1 ROW 80 NUNIT-FILE:SECTION!
   2 s" calls" NUNIT-FILE:SECTION!
   3 s" addr" NUNIT-FILE:SECTION!
   4 s" protection" NUNIT-FILE:SECTION!
   5 s" checker" NUNIT-FILE:SECTION!
   6 s" data" NUNIT-FILE:SECTION!
   7 s" updates" NUNIT-FILE:SECTION!
   8 s" provides" NUNIT-FILE:SECTION!
   9 s" slots" NUNIT-FILE:SECTION!
   10 s" literals" NUNIT-FILE:SECTION!
   11 s" inputs" NUNIT-FILE:SECTION!
   12 s" retired" NUNIT-FILE:SECTION!
   13 s" refs" NUNIT-FILE:SECTION!
   14 s" deps" NUNIT-FILE:SECTION!
   15 s" compiler" NUNIT-FILE:SECTION!
   16 s" scope" NUNIT-FILE:SECTION!
   17 s" values" NUNIT-FILE:SECTION!
   18 s" origins" NUNIT-FILE:SECTION!
   19 s" output" NUNIT-FILE:SECTION! ;

: WRITE-GOOD ( -- )
   SECTIONS
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:WRITE ;

: READ-GOOD ( -- )
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ ;

: CHECK-ROUNDTRIP ( -- )
   WRITE-GOOD
   READ-GOOD
   0 NUNIT-FILE:SECTION$ s" code" T$=
   1 NUNIT-FILE:SECTION$ ROW 80 T$=
   2 NUNIT-FILE:SECTION$ s" calls" T$=
   3 NUNIT-FILE:SECTION$ s" addr" T$=
   4 NUNIT-FILE:SECTION$ s" protection" T$=
   5 NUNIT-FILE:SECTION$ s" checker" T$=
   6 NUNIT-FILE:SECTION$ s" data" T$=
   7 NUNIT-FILE:SECTION$ s" updates" T$=
   8 NUNIT-FILE:SECTION$ s" provides" T$=
   9 NUNIT-FILE:SECTION$ s" slots" T$=
   10 NUNIT-FILE:SECTION$ s" literals" T$=
   11 NUNIT-FILE:SECTION$ s" inputs" T$=
   12 NUNIT-FILE:SECTION$ s" retired" T$=
   19 NUNIT-FILE:SECTION$ s" output" T$=
   NUNIT-FILE:CLOSE
   ART$ RAW ART-CAP READ-ALL RAW-U !
   RAW GOOD RAW-U @ BYTE-COPY ;

: RESTORE ( -- )
   GOOD RAW RAW-U @ BYTE-COPY
   ART$ RAW RAW-U @ WRITE-ALL ;
: BAD-READ ( -- ) READ-GOOD NUNIT-FILE:CLOSE ;
: BAD-VIEW ( -- )
   RAW ART$ FILE-SIZE NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ-VIEW ;
: FIRST-SECTION ( -- ) 0 NUNIT-FILE:SECTION$ 2drop ;

: REFUSE ( -- )
   [: BAD-READ ;] E-NUNIT-FILE TTHROWSQ
   GOOD RAW-U @ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ-VIEW
   [: BAD-VIEW ;] E-NUNIT-FILE TTHROWSQ
   [: FIRST-SECTION ;] E-NUNIT-FILE TTHROWSQ
   NUNIT-FILE:CLOSE
   RESTORE ;

: CHECK-VIEW ( -- )
   s" borrowed sections retain the input bytes until reset" T-LABEL
   RAW RAW-U @ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ-VIEW
   0 NUNIT-FILE:SECTION$ s" code" T$=
   1 NUNIT-FILE:SECTION$ ROW 80 T$=
   0 NUNIT-FILE:SECTION$ drop RAW HEADER + = TTRUE
   NUNIT-FILE:RESET-KEEP
   [: FIRST-SECTION ;] E-NUNIT-FILE TTHROWSQ
   RAW RAW-U @ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ-BYTES
   0 RAW HEADER + c!
   0 NUNIT-FILE:SECTION$ s" code" T$=
   NUNIT-FILE:CLOSE
   RESTORE ;

: CHECK-CORRUPT ( -- )
   s" corrupt or incompatible NBR files refuse before exposing sections" T-LABEL
   0 RAW c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   255 RAW 8 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   255 RAW 16 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   0 RAW 24 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   0 RAW 32 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   0 RAW 96 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   81 RAW 168 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   1 RAW 160 + c! 4 RAW 163 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   ART$ RAW RAW-U @ 1- WRITE-ALL REFUSE
   ART$ RAW HEADER 1- WRITE-ALL REFUSE
   ART$ RAW RAW-U @ 1+ WRITE-ALL REFUSE ;

: CHECK-EMPTY-TAIL ( -- )
   s" empty trailing sections remain valid at the exact file end" T-LABEL
   NUNIT-FILE:RESET
   0 s" code1234" NUNIT-FILE:SECTION!
   1 ROW 80 NUNIT-FILE:SECTION!
   NUNIT-FILE:SECTION-COUNT 2 ?do i s" " NUNIT-FILE:SECTION! loop
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:WRITE
   READ-GOOD
   0 NUNIT-FILE:SECTION$ s" code1234" T$=
   NUNIT-FILE:SECTION-COUNT 2 ?do i NUNIT-FILE:SECTION$ nip 0 T= loop
   NUNIT-FILE:CLOSE
   ART$ RAW ART-CAP READ-ALL {: size:n :}
   RAW size NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ-VIEW
   NUNIT-FILE:SECTION-COUNT 2 ?do
      i NUNIT-FILE:SECTION$ 0 T= RAW size + = TTRUE
   loop
   NUNIT-FILE:CLOSE ;

: CHECK-LONG-INPUT ( -- )
   s" a real source file and canonical query beyond 64 path bytes survive input replay" T-LABEL
   LONG-SOURCE$ nip 64 > TTRUE
   LONG-SOURCE$ LONG-CONTENT$ WRITE-ALL
   NFILE-INPUTS:RESET
   LONG-SOURCE$ LONG-CONTENT$ NFILE-INPUTS:ADD
   LONG-SOURCE$ LONG-SOURCE$ SOURCE-ROOT:CANON-OS NFILE-INPUTS:ADD-CANON
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK
   NFILE-INPUTS:CLOSE ;

: CHECK-LARGE ( -- )
   s" a unit beyond 64 MiB survives roundtrip" T-LABEL
   $4000001 MEM-ALLOC-BYTES {: body:ptr size:n :}
   17 body c! 29 body size 1- + c!
   SECTIONS
   6 body size NUNIT-FILE:SECTION!
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:WRITE
   READ-GOOD
   6 NUNIT-FILE:SECTION$ {: got:ptr gotu:n :}
   gotu size T=
   got c@ 17 T=
   got size 1- + c@ 29 T=
   NUNIT-FILE:CLOSE
   body size munmap 0 T= ;

\ A name of u bytes, all `L`.
create LONG 65 allot
: LONG$ ( n -- ptr u8 n ) {: u:n :}
   u 0 ?do  [char] L LONG i + c!  loop
   LONG u ;

: WRITE-NAMED ( ptr u8 n -- ) {: a:ptr u:n :}
   ART$ NUNIT-FILE:ARCH-AARCH64 a u KEY$ NUNIT-FILE:WRITE ;

: READ-NAMED ( ptr u8 n -- ) {: a:ptr u:n :}
   ART$ NUNIT-FILE:ARCH-AARCH64 a u KEY$ NUNIT-FILE:READ ;

: CHECK-NAME-FIELD ( -- )
   s" a 64-byte package name fills the header's field and one byte more refuses by name" T-LABEL
   SECTIONS
   64 LONG$ WRITE-NAMED
   64 LONG$ READ-NAMED
   0 NUNIT-FILE:SECTION$ s" code" T$=
   NUNIT-FILE:CLOSE
   [: 65 LONG$ WRITE-NAMED ;] E-NUNIT-PROFILE TTHROWSQ
   [: 65 LONG$ READ-NAMED ;] E-NUNIT-PROFILE TTHROWSQ
   NUNIT-FILE:CLOSE ;

public
: RUN ( -- )
   T-RESET
   SETUP
   s" Native object returns thirteen owned section slices" T-LABEL
   CHECK-ROUNDTRIP
   CHECK-VIEW
   CHECK-CORRUPT
   CHECK-NAME-FIELD
   CHECK-EMPTY-TAIL
   CHECK-LONG-INPUT
   CHECK-LARGE
   T-REPORT
   s" native unit artifact: " type ART$ type cr ;

;package

NUNIT-FILE-TEST:RUN
