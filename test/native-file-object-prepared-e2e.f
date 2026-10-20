\ A rejected FILE object or later selected range leaves live state untouched.
\ Repeatable artifacts: the four unit objects printed by this test.
\ bin/hb --load test/native-file-object-prepared-e2e.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/le.f
require tools/native-unit-object.f

package NATIVE-FILE-PREPARED-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create GOOD FS-PATH-CAP allot variable GOOD-U
create LIT-PATH FS-PATH-CAP allot variable LIT-PATH-U
create REQ-PATH FS-PATH-CAP allot variable REQ-PATH-U
create KEY 64 allot
create LITS 16 allot
create LITS-WITH-BODY 41 allot
create REQ-ROW PATH-CAP 9 + allot
create DATA 8 allot
create UPDATES 32 allot
create REFS 32 allot
create DEPS 16 allot
NUNIT-CAPTURE:START-PREFIX
variable OWNED

: ART$ ( -- ptr u8 n ) PATH PATH-U @ ;
: GOOD$ ( -- ptr u8 n ) GOOD GOOD-U @ ;
: LITERAL$ ( -- ptr u8 n ) LIT-PATH LIT-PATH-U @ ;
: REQUIRE$ ( -- ptr u8 n ) REQ-PATH REQ-PATH-U @ ;
: PUBLISH ( ptr u8 n ptr u8 n ptr n n ptr n n -- )
   2drop 2drop 2drop 2drop ;
: IMPORT-BAD ( -- )
   [: PUBLISH ;] ART$ KEY 64 s" FILE" NUNIT-OBJECT:IMPORT-NAMED 2drop ;
: PREPARE-BAD ( -- )
   ART$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop ;
: PREPARE-BAD-REQUIRE ( -- )
   REQUIRE$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop ;

public
: RUN ( -- )
   T-RESET
   32 0 ?do 0 REFS i + c! loop
   s" HBFDEP02" drop DEPS 8 BYTE-COPY
   0 DEPS 8 + LE:U64!
   s" late update rejection preserves prior DATA and HERE" T-LABEL
   s" native-file-prepared" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT ROOT-U @ s" malformed.unit" PATH JOIN-PATH PATH-U !
   7 OWNED !
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   14 DEPS 16 NUNIT-FILE:SECTION!
   0 LITS LE:U64! 0 LITS 8 + LE:U64!
   10 LITS 16 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   OWNED BYTE-VIEW NULL-PTR BYTE-VIEW - DATA-VA VA>N -
      UPDATES LE:U64!
   42 UPDATES 8 + LE:U64!
   -1 UPDATES 16 + LE:U64!
   99 UPDATES 24 + LE:U64!
   7 UPDATES 32 NUNIT-FILE:SECTION!
   ART$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE
   ROOT ROOT-U @ s" valid.unit" GOOD JOIN-PATH GOOD-U !
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   14 DEPS 16 NUNIT-FILE:SECTION!
   0 LITS LE:U64! 0 LITS 8 + LE:U64!
   10 LITS 16 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   GOOD$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE
   ROOT ROOT-U @ s" literal.unit" LIT-PATH JOIN-PATH LIT-PATH-U !
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   14 DEPS 16 NUNIT-FILE:SECTION!
   0 LITS-WITH-BODY LE:U64!
   1 LITS-WITH-BODY 8 + LE:U64!
   0 LITS-WITH-BODY 16 + LE:U64!
   0 LITS-WITH-BODY 24 + LE:U64!
   1 LITS-WITH-BODY 32 + LE:U64!
   120 LITS-WITH-BODY 40 + c!
   10 LITS-WITH-BODY 41 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   LITERAL$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE
   ROOT ROOT-U @ s" require.unit" REQ-PATH JOIN-PATH REQ-PATH-U !
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   14 DEPS 16 NUNIT-FILE:SECTION!
   0 LITS LE:U64! 0 LITS 8 + LE:U64!
   10 LITS 16 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   PATH-CAP 1+ REQ-ROW LE:U64!
   PATH-CAP 1+ 0 ?do 97 REQ-ROW 8 + i + c! loop
   8 REQ-ROW PATH-CAP 9 + NUNIT-FILE:SECTION!
   REQUIRE$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE
   here {: before:ptr :}
   cp@ {: code-before:n :}
   ndict@ {: rec-before:n :}
   AOT-ARM:WIDN {: wid-before:n :}
   NSTR:COUNT {: literal-before:n :}
   ['] IMPORT-BAD E-NUNIT-PROFILE TTHROWSQ
   here BYTE-VIEW before BYTE-VIEW - 0 T=
   OWNED @ 7 T=
   s" later bad range leaves earlier prepared range unpublished" T-LABEL
   GOOD$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop
   LITERAL$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop
   ['] PREPARE-BAD E-NUNIT-PROFILE TTHROWSQ
   here BYTE-VIEW before BYTE-VIEW - 0 T=
   cp@ code-before T=
   ndict@ rec-before T=
   AOT-ARM:WIDN wid-before T=
   NSTR:COUNT literal-before T=
   OWNED @ 7 T=
   NUNIT-OBJECT:DISCARD-PREPARED
   s" oversized require path is rejected before publication" T-LABEL
   GOOD$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop
   LITERAL$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop
   ['] PREPARE-BAD-REQUIRE E-NUNIT-PROFILE TTHROWSQ
   here BYTE-VIEW before BYTE-VIEW - 0 T=
   cp@ code-before T=
   ndict@ rec-before T=
   AOT-ARM:WIDN wid-before T=
   NSTR:COUNT literal-before T=
   OWNED @ 7 T=
   NUNIT-OBJECT:DISCARD-PREPARED
   NUNIT-OBJECT:CLOSE
   T-REPORT
   s" prepared FILE object fixtures: " type GOOD$ type
   s"  " type ART$ type s"  " type LITERAL$ type
   s"  " type REQUIRE$ type cr ;
;package

NATIVE-FILE-PREPARED-TEST:RUN
