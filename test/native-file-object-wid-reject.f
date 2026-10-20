\ Malformed FILE namespace wordlist references must refuse before publication.
\ bin/hb --load test/native-file-object-wid-reject.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/le.f
require tools/native-unit-object.f

package NATIVE-FILE-WID-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create KEY 64 allot
create REC 48 allot
create LITS 16 allot
create REFS 32 allot
create DEPS 16 allot

: ART$ ( -- ptr u8 n ) PATH PATH-U @ ;
: PUBLISH ( ptr u8 n ptr u8 n ptr n n ptr n n -- )
   2drop 2drop 2drop 2drop ;
: IMPORT-BAD ( -- )
   [: PUBLISH ;] ART$ KEY 64 s" FILE" NUNIT-OBJECT:IMPORT-NAMED 2drop ;
public
: RUN ( -- )
   T-RESET
   s" negative WID references outside the reserved global marker refuse" T-LABEL
   s" native-file-wid" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT ROOT-U @ s" malformed.unit" PATH JOIN-PATH PATH-U !
   NUNIT-CAPTURE:START-PREFIX
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   32 0 ?do 0 REFS i + c! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   s" HBFDEP02" drop DEPS 8 BYTE-COPY
   0 DEPS 8 + LE:U64!
   14 DEPS 16 NUNIT-FILE:SECTION!
   -1 REC LE:U64!
   -1 REC 8 + LE:U64!
   3 REC 16 + LE:U64!
   s" BAD" drop REC 24 + 3 BYTE-COPY
   XREF-NAMESPACE-WL REC 40 + LE:U64!
   1 REC 48 NUNIT-FILE:SECTION!
   10 LITS 16 NUNIT-FILE:SECTION!
   ART$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE
   ['] IMPORT-BAD E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:CLOSE
   T-REPORT
   s" native malformed file artifact: " type ART$ type cr ;
;package

NATIVE-FILE-WID-TEST:RUN
