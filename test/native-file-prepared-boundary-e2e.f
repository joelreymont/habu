\ Rejected FILE metadata leaves staged ranges and the live compiler untouched.
\ bin/hb --load test/native-file-prepared-boundary-e2e.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/le.f
require tools/native-unit-object.f

package NATIVE-FILE-PREP-BOUNDARY-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot variable PATH-U
create KEY 64 allot
create LITS 16 allot
create LITS-SEG 32 allot
create DATA 8 allot
create OWNER 56 allot
create RETIRE 8 allot
create UPDATE 16 allot
create SLOT-ROW 40 allot

public
: TARGET ( -- n ) 1 ;
private
NUNIT-CAPTURE:START-PREFIX
variable OWNED
PERSISTED-PTR-VARIABLE SLOT

: PATH$ ( -- ptr u8 n ) PATH PATH-U @ ;
: PREPARE ( -- ) PATH$ KEY 64 s" FILE" NUNIT-OBJECT:PREPARE-NAMED drop ;

: WRITE-UNIT ( ptr u8 n n -- ) {: name:ptr nameu:n mode:n :}
   ROOT ROOT-U @ name nameu PATH JOIN-PATH PATH-U !
   NUNIT-FILE:RESET
   13 0 ?do i KEY 0 NUNIT-FILE:SECTION! loop
   0 LITS LE:U64! 0 LITS 8 + LE:U64!
   10 LITS 16 NUNIT-FILE:SECTION!
   6 DATA 8 NUNIT-FILE:SECTION!
   mode 1 = if
      s" NATIVE-FILE-PREP-BOUNDARY-TEST:TARGET"
         XREF-FIND-TARGET-INDEX RETIRE LE:U64!
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
      4 SLOT-ROW 16 + LE:U64!
      9 SLOT-ROW 40 NUNIT-FILE:SECTION!
   then
   mode 4 = mode 5 = or if
      40 0 ?do 0 SLOT-ROW i + c! loop
      here BYTE-VIEW NULL-PTR BYTE-VIEW - DATA-VA VA>N -
         {: raw:n :}
      mode 4 = if raw SNAP-RELOC:XTCELL-DATA-TAG or
      else raw then SLOT-ROW LE:U64!
      mode 4 = if 1 else 0 then SLOT-ROW 8 + LE:U64!
      4 SLOT-ROW 16 + LE:U64!
      9 SLOT-ROW 40 NUNIT-FILE:SECTION!
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
   s" native-file-prep-boundary" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   7 OWNED !
   here {: data:ptr :}
   cp@ {: code:n :}
   ndict@ {: rec:n :}
   AOT-ARM:WIDN {: wid:n :}
   NSTR:COUNT {: literals:n :}
   ADDRESS-CELLS:LIVE-SPAN nip {: slots:n :}

   s" duplicate retirement is rejected during whole-tree preparation" T-LABEL
   s" empty.unit" 0 WRITE-UNIT PREPARE
   s" retire.unit" 1 WRITE-UNIT PREPARE
   ['] PREPARE E-NUNIT-PROFILE TTHROWSQ
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
