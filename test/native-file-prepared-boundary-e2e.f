\ Rejected FILE metadata leaves staged ranges and the live compiler untouched.
\ bin/hb --load test/native-file-prepared-boundary-e2e.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/le.f
require tools/native-unit-object.f

package NATIVE-FILE-PREP-BOUNDARY-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create RETIRE-PATH FS-PATH-CAP allot variable RETIRE-U
create KEY 64 allot
create LITS 16 allot
create LITS-SEG 32 allot
create DATA 8 allot
create OWNER 56 allot
create RETIRE 8 allot
create UPDATE 16 allot
create SLOT-ROW 40 allot
create REFS FS-PATH-CAP allot
create ORIGIN 48 72 + FS-PATH-CAP + 1 + allot
create DEPS 16 allot
create NS-CODE 24 allot
create NS-RECORDS 144 allot
create NS-WIDS 2 allot
create NS-REFS 227 allot

public
: TARGET ( -- n ) 1 ;
private
NUNIT-CAPTURE:START-PREFIX
variable OWNED
PERSISTED-PTR-VARIABLE SLOT

: PATH$ ( -- ptr u8 n ) PATH PATH-U @ ;
: PREPARE ( -- ) PATH$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop ;
: AVAILABLE? ( ptr u8 n ptr u8 n -- bool ) 2drop 2drop true ;
: UNAVAILABLE? ( ptr u8 n ptr u8 n -- bool ) 2drop 2drop false ;
: PREPARE-ORIGIN ( -- )
   PATH$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED
   NUNIT-OBJECT:PREPARED-ORIGIN-OF$ 0 [: AVAILABLE? ;]
   NUNIT-ORIGIN:PREPARE ;
: PREPARE-MISSING-PROVIDER ( -- )
   PATH$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED
   NUNIT-OBJECT:PREPARED-ORIGIN-OF$ 0 [: UNAVAILABLE? ;]
   NUNIT-ORIGIN:PREPARE ;
: PREPARE-PROJECTED-ORIGIN ( -- )
   PATH$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED
   NUNIT-OBJECT:PREPARED-ORIGIN-OF$ 0
      [: NUNIT-OBJECT:PROJECT-CALLABLE? ;] NUNIT-ORIGIN:PREPARE ;
: PREPARE-RETIRE-UNIT ( -- )
   RETIRE-PATH RETIRE-U @ KEY 64 s" FILE"
      NUNIT-OBJECT:PREPARE-NAMED drop ;

: EXTERNAL-TARGET-REF ( -- )
   s" NATIVE-FILE-PREP-BOUNDARY-TEST:TARGET"
      XREF-FIND-TARGET-INDEX NFILE-REF:DECL-KEY$
      {: refkey:ptr refu:n :}
   refu FS-PATH-CAP 96 - > if E-NUNIT-PROFILE throw then
   1 REFS LE:U64! 1 REFS 8 + LE:U64!
   0 REFS 16 + LE:U64! 0 REFS 24 + LE:U64!
   NFILE-REF:RECORD REFS 32 + LE:U64!
   NFILE-REF:EXTERNAL REFS 40 + LE:U64!
   0 REFS 48 + LE:U64! 0 REFS 56 + LE:U64!
   NFILE-REF:RECORD REFS 64 + LE:U64!
   refu REFS 72 + LE:U64!
   0 REFS 80 + LE:U64! 1 REFS 88 + LE:U64!
   refkey REFS 96 + refu BYTE-COPY
   13 REFS 96 refu + NUNIT-FILE:SECTION! ;

: PROJECTED-PROVIDER ( n -- ) {: mode:n :}
   s" ENGINE-ORIGIN-EARLY" drop NS-CODE 19 BYTE-COPY
   0 NS-CODE 19 + c!
   $1F NS-CODE 20 + c! $20 NS-CODE 21 + c!
   $03 NS-CODE 22 + c! $D5 NS-CODE 23 + c!
   0 NS-WIDS c! 0 NS-WIDS 1+ c!
   144 0 ?do 0 NS-RECORDS i + c! loop
   1 NS-RECORDS LE:U64!
   2 NS-RECORDS 8 + LE:U64!
   DNAME-EXT 19 or NS-RECORDS 16 + LE:U64!
   XREF-NAMESPACE-WL NS-RECORDS 40 + LE:U64!
   mode 8 = if -1 else -4 then NS-RECORDS 48 + LE:U64!
   9 NS-RECORDS 64 + LE:U64!
   s" FILE-MARK" drop NS-RECORDS 72 + 9 BYTE-COPY
   1 NS-RECORDS 88 + LE:U64!
   -4 NS-RECORDS 96 + LE:U64!
   12 NS-RECORDS 112 + LE:U64!
   s" FILE-RESTORE" drop NS-RECORDS 120 + 12 BYTE-COPY
   1 NS-RECORDS 136 + LE:U64!
   227 0 ?do 0 NS-REFS i + c! loop
   3 NS-REFS LE:U64!
   3 NS-REFS 16 + LE:U64!
   NFILE-REF:WID NS-REFS 32 + LE:U64!
   NFILE-REF:WID NS-REFS 64 + LE:U64!
   1 NS-REFS 80 + LE:U64!
   NFILE-REF:CODE NS-REFS 96 + LE:U64!
   20 NS-REFS 112 + LE:U64!
   1 NS-REFS 136 + LE:U64!
   [char] a NS-REFS 160 + c!
   1 NS-REFS 161 + LE:U64!
   1 NS-REFS 169 + LE:U64!
   [char] b NS-REFS 193 + c!
   2 NS-REFS 194 + LE:U64!
   1 NS-REFS 202 + LE:U64!
   [char] c NS-REFS 226 + c!
   0 NS-CODE 24 NUNIT-FILE:SECTION!
   1 NS-RECORDS 144 NUNIT-FILE:SECTION!
   4 NS-WIDS 2 NUNIT-FILE:SECTION!
   13 NS-REFS 227 NUNIT-FILE:SECTION! ;

: WRITE-UNIT ( ptr u8 n n -- ) {: name:ptr nameu:n mode:n :}
   ROOT ROOT-U @ name nameu PATH JOIN-PATH PATH-U !
   NUNIT-FILE:RESET
   17 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   32 0 ?do 0 REFS i + c! loop
   13 REFS 32 NUNIT-FILE:SECTION!
   s" HBFDEP02" drop DEPS 8 BYTE-COPY
   0 DEPS 8 + LE:U64!
   14 DEPS 16 NUNIT-FILE:SECTION!
   0 LITS LE:U64! 0 LITS 8 + LE:U64!
   10 LITS 16 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   mode 1 = if
      EXTERNAL-TARGET-REF
      1 RETIRE LE:U64!
      12 RETIRE 8 NUNIT-FILE:SECTION!
   then
   mode 2 = if
      56 0 ?do 0 OWNER i + c! loop
      131120 OWNER 8 + LE:U64!
      8192 OWNER 16 + LE:U64!
      2 OWNER 48 + LE:U64!
      6 OWNER 56 NUNIT-FILE:SECTION!
      1 LITS-SEG LE:U64!
      0 LITS-SEG 16 + LE:U64!
      56 LITS-SEG 24 + LE:U64!
      10 LITS-SEG 32 NUNIT-FILE:SECTION!
   then
   mode 3 = if
      OWNED BYTE-VIEW NULL-PTR BYTE-VIEW - DATA-VA VA>N -
         UPDATE LE:U64!
      42 UPDATE 8 + LE:U64!
      7 UPDATE 16 NUNIT-FILE:SECTION!
      40 0 ?do 0 SLOT-ROW i + c! loop
      SLOT BYTE-VIEW NULL-PTR BYTE-VIEW - DATA-VA VA>N -
         SLOT-ROW LE:U64!
      9 SLOT-ROW 40 NUNIT-FILE:SECTION!
   then
   mode 4 = mode 5 = or if
      SLOT BYTE-VIEW CELL NFILE-REF:REF-DATA {: site:n :}
      ndict@ ndict@ NFILE-REF:ENCODE {: rows:ptr rowsu:n :}
      13 rows rowsu NUNIT-FILE:SECTION!
      40 0 ?do 0 SLOT-ROW i + c! loop
      site SLOT-ROW LE:U64!
      mode 4 = if 2 else 0 then SLOT-ROW 8 + LE:U64!
      9 SLOT-ROW 40 NUNIT-FILE:SECTION!
   then
   mode 6 = if
      EXTERNAL-TARGET-REF
      48 72 + FS-PATH-CAP + 1 + 0 ?do 0 ORIGIN i + c! loop
      $4F524947494E3034 ORIGIN LE:U64!
      1 ORIGIN 8 + LE:U64!
      0 ORIGIN 16 + LE:U64!
      1 ORIGIN 24 + LE:U64!
      0 ORIGIN 48 + LE:U64!
      1 ORIGIN 56 + LE:U64!
      FS-PATH-CAP ORIGIN 88 + LE:U64!
      1 ORIGIN 96 + LE:U64!
      1 ORIGIN 112 + LE:U64!
      FS-PATH-CAP 0 ?do 97 ORIGIN 120 + i + c! loop
      88 ORIGIN 120 + FS-PATH-CAP + c!
      16 ORIGIN 48 72 + FS-PATH-CAP + 1 + NUNIT-FILE:SECTION!
   then
   mode 7 = if
      48 0 ?do 0 ORIGIN i + c! loop
      $4F524947494E3034 ORIGIN LE:U64!
      16 ORIGIN 48 NUNIT-FILE:SECTION!
   then
   mode 8 = mode 9 = or if
      mode PROJECTED-PROVIDER
      48 0 ?do 0 ORIGIN i + c! loop
      $4F524947494E3034 ORIGIN LE:U64!
      16 ORIGIN 48 NUNIT-FILE:SECTION!
   then
   mode 10 = if
      48 0 ?do 0 ORIGIN i + c! loop
      $4F524947494E3033 ORIGIN LE:U64!
      16 ORIGIN 48 NUNIT-FILE:SECTION!
   then
   PATH$ NUNIT-FILE:ARCH-AARCH64 s" FILE" KEY 64 NUNIT-FILE:WRITE ;

: CHECK-STATE ( ptr u8 n n n n -- )
   {: data:ptr code:n rec:n wid:n literals:n :}
   here BYTE-VIEW data BYTE-VIEW - 0 T=
   cp@ code T=
   ndict@ rec T=
   AOT-ARM:WIDN wid T=
   NSTR:COUNT literals T=
   OWNED @ 7 T= ;

public
: RUN ( -- )
   T-RESET
   s" rootless FILE scopes retain distinct occurrence identities" T-LABEL
   NFILE-REF:CLOSE
   \ This path fills the key buffer exactly at the empty root field.
   s" no-root-source-example.f" NULL-PTR 0 0 NFILE-REF:PUSH-FILE
   NFILE-REF:INSTANCE-KEY$ {: first:ptr firstu:n :}
   first REFS firstu BYTE-COPY
   NFILE-REF:POP-FILE
   s" no-root-source-example.f" NULL-PTR 0 0 NFILE-REF:PUSH-FILE
   NFILE-REF:INSTANCE-KEY$ REFS firstu STR= TFALSE
   NFILE-REF:POP-FILE
   NFILE-REF:CLOSE
   NFILE-REF:BIND-SEED
   s" native-file-prep-boundary" NFILE-REF:FILE-INSTANCE!
   NFILE-REF:BEGIN-ALLOC
   s" native-file-prep-boundary" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   7 OWNED !
   here {: data:ptr :}
   cp@ {: code:n :}
   ndict@ {: rec:n :}
   AOT-ARM:WIDN {: wid:n :}
   NSTR:COUNT {: literals:n :}
   ADDRESS-CELLS:LIVE-SPAN nip {: slots:n :}

   s" oversized early origin path is rejected in preparation" T-LABEL
   s" origin-path.unit" 6 WRITE-UNIT
   ['] PREPARE-ORIGIN E-NUNIT-FILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   s" unavailable FILE replay provider is rejected in preparation" T-LABEL
   s" missing-provider.unit" 7 WRITE-UNIT
   ['] PREPARE-MISSING-PROVIDER E-NUNIT-FILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   s" prepared FILE origin with available callbacks is accepted" T-LABEL
   s" available-provider.unit" 7 WRITE-UNIT PREPARE-ORIGIN
   data code rec wid literals CHECK-STATE
   NUNIT-OBJECT:DISCARD-PREPARED

   s" empty FILE origin is rejected before publication" T-LABEL
   s" empty-origin.unit" 0 WRITE-UNIT
   ['] PREPARE-ORIGIN E-NUNIT-FILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   s" old FILE origin format is rejected before publication" T-LABEL
   s" old-origin.unit" 10 WRITE-UNIT
   ['] PREPARE-ORIGIN E-NUNIT-FILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   s" projected FILE-MARK with entry zero is rejected before publication" T-LABEL
   s" zero-entry-provider.unit" 8 WRITE-UNIT
   ['] PREPARE-PROJECTED-ORIGIN E-NUNIT-FILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   s" projected FILE callbacks with code entries are accepted" T-LABEL
   s" code-entry-provider.unit" 9 WRITE-UNIT PREPARE-PROJECTED-ORIGIN
   data code rec wid literals CHECK-STATE
   NUNIT-OBJECT:DISCARD-PREPARED

   s" duplicate retirement is rejected during whole-tree preparation" T-LABEL
   s" retire.unit" 1 WRITE-UNIT
   PATH RETIRE-PATH PATH-U @ BYTE-COPY PATH-U @ RETIRE-U !
   s" empty.unit" 0 WRITE-UNIT PREPARE
   PREPARE-RETIRE-UNIT
   ['] PREPARE-RETIRE-UNIT E-NUNIT-PROFILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   NUNIT-OBJECT:DISCARD-PREPARED

   s" truncated literal owner is rejected before DATA publication" T-LABEL
   s" owner.unit" 2 WRITE-UNIT
   ['] PREPARE E-NUNIT-PROFILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   NUNIT-OBJECT:DISCARD-PREPARED

   s" persisted pointer slot rejects an XT-tagged import" T-LABEL
   s" slot.unit" 3 WRITE-UNIT
   ['] PREPARE E-NUNIT-PROFILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   NUNIT-OBJECT:DISCARD-PREPARED

   s" projected pointer slot rejects a later XT declaration" T-LABEL
   s" project-pointer.unit" 4 WRITE-UNIT PREPARE
   s" project-xt.unit" 5 WRITE-UNIT
   ['] PREPARE E-NUNIT-PROFILE TTHROWSQ
   data code rec wid literals CHECK-STATE
   ADDRESS-CELLS:LIVE-SPAN nip slots T=
   NUNIT-OBJECT:DISCARD-PREPARED

   NUNIT-OBJECT:CLOSE
   T-REPORT
   s" prepared FILE boundary artifacts: " type ROOT ROOT-U @ type cr ;
;package

NATIVE-FILE-PREP-BOUNDARY-TEST:RUN
