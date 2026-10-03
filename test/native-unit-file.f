\ Boundary acceptance for the versioned NBR package artifact. The retained
\ path printed below can be inspected with a byte dumper after this run.
\ Failure modes: bad magic/version/arch/name/key, malformed row length,
\ section length past EOF, truncation, and trailing bytes must all refuse; a
\ package name past the header's 64-byte field refuses as a profile the format
\ cannot represent.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/native-unit-file.f

package NUNIT-FILE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create ROW 48 allot
create GOOD 512 allot
create RAW 512 allot variable RAW-U

: KEY$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

: ART$ ( -- ptr u8 n ) PATH PATH-U @ ;

: SETUP ( -- )
   s" native-unit-file" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT ROOT-U @ s" nbr.unit" PATH JOIN-PATH PATH-U !
   48 0 ?do i ROW i + c! loop ;

: SECTIONS ( -- )
   NUNIT-FILE:RESET
   0 s" code" NUNIT-FILE:SECTION!
   1 ROW 48 NUNIT-FILE:SECTION!
   2 s" calls" NUNIT-FILE:SECTION!
   3 s" addr" NUNIT-FILE:SECTION!
   4 s" protection" NUNIT-FILE:SECTION!
   5 s" checker" NUNIT-FILE:SECTION! ;

: WRITE-GOOD ( -- )
   SECTIONS
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:WRITE ;

: READ-GOOD ( -- )
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:READ ;

: CHECK-ROUNDTRIP ( -- )
   WRITE-GOOD
   READ-GOOD
   0 NUNIT-FILE:SECTION$ s" code" T$=
   1 NUNIT-FILE:SECTION$ ROW 48 T$=
   2 NUNIT-FILE:SECTION$ s" calls" T$=
   3 NUNIT-FILE:SECTION$ s" addr" T$=
   4 NUNIT-FILE:SECTION$ s" protection" T$=
   5 NUNIT-FILE:SECTION$ s" checker" T$=
   NUNIT-FILE:CLOSE
   ART$ RAW 512 READ-ALL RAW-U !
   RAW GOOD RAW-U @ BYTE-COPY ;

: RESTORE ( -- )
   GOOD RAW RAW-U @ BYTE-COPY
   ART$ RAW RAW-U @ WRITE-ALL ;
: BAD-READ ( -- ) READ-GOOD NUNIT-FILE:CLOSE ;

: REFUSE ( -- )
   [: BAD-READ ;] E-NUNIT-FILE TTHROWSQ
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
   49 RAW 168 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   1 RAW 160 + c! 4 RAW 163 + c! ART$ RAW RAW-U @ WRITE-ALL REFUSE
   ART$ RAW RAW-U @ 1- WRITE-ALL REFUSE
   ART$ RAW RAW-U @ 1+ WRITE-ALL REFUSE ;

: CHECK-EMPTY-TAIL ( -- )
   s" empty trailing sections remain valid at the exact file end" T-LABEL
   NUNIT-FILE:RESET
   0 s" code1234" NUNIT-FILE:SECTION!
   1 ROW 48 NUNIT-FILE:SECTION!
   2 s" " NUNIT-FILE:SECTION!
   3 s" " NUNIT-FILE:SECTION!
   4 s" " NUNIT-FILE:SECTION!
   5 s" " NUNIT-FILE:SECTION!
   ART$ NUNIT-FILE:ARCH-AARCH64 s" NBR" KEY$ NUNIT-FILE:WRITE
   READ-GOOD
   0 NUNIT-FILE:SECTION$ s" code1234" T$=
   2 NUNIT-FILE:SECTION$ nip 0 T=
   3 NUNIT-FILE:SECTION$ nip 0 T=
   4 NUNIT-FILE:SECTION$ nip 0 T=
   5 NUNIT-FILE:SECTION$ nip 0 T=
   NUNIT-FILE:CLOSE ;

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
   s" NBR artifact returns six owned section slices" T-LABEL
   CHECK-ROUNDTRIP
   CHECK-CORRUPT
   CHECK-NAME-FIELD
   CHECK-EMPTY-TAIL
   T-REPORT
   s" native unit artifact: " type ART$ type cr ;

;package

NUNIT-FILE-TEST:RUN
