\ NBR's first native object profile: contiguous code, inline-name dictionary
\ rows, source-declared relocation sites, two wordlists and checker facts.
\ Every address-bearing field written to disk is a unit offset or a prefix
\ dictionary ordinal. The exact source key fixes that prefix for this format.

require lib/errors.f
require lib/le.f
require src/habu/xref.f
require src/habu/address-carrier.f
require src/compiler/native/a64ir.f
require src/compiler/native/branch.f
require tools/native-unit-capture.f
require tools/native-unit-file.f

package NUNIT-OBJECT

private

0 constant UNIT-REF
1 constant PREFIX-REF
32 constant CALL-BYTES
24 constant ADDR-BYTES
4 constant INSN-BYTES

DYNAMIC-BUFFER CODE-BUF u8
DYNAMIC-BUFFER REC-BUF u8
DYNAMIC-BUFFER CALL-BUF u8
DYNAMIC-BUFFER ADDR-BUF u8
variable CODE-U
variable REC-U
variable CALL-U
variable ADDR-U
create PROTECTION 1 allot
variable IMPORT-CODE
variable IMPORT-REC
variable IMPORT-WID

: BAD ( -- ) E-NUNIT-PROFILE throw ;

: CODE-START ( -- n ) NUNIT-CAPTURE:FIRST-CODE ;
: CODE-END ( -- n ) CODE-START CODE-U @ + ;
: WID-FIRST ( -- n ) NUNIT-CAPTURE:FIRST-WID ;

: CODE-PROFILE ( -- )
   NUNIT-CAPTURE:CODE$ {: a:ptr size:n :}
   size 0 <= size INSN-BYTES mod 0<> or if BAD then
   size CODE-BUF-RESERVE
   a 0 CODE-BUF size BYTE-COPY
   size CODE-U ! ;

: RECORD-PROFILE ( -- )
   NUNIT-CAPTURE:RECORDS$ {: a:ptr size:n :}
   size 0 <= size DREC mod 0<> or if BAD then
   size REC-BUF-RESERVE
   a 0 REC-BUF size BYTE-COPY
   size REC-U ! ;

: WID-SLOT ( n -- n ) {: wid:n :}
   wid WID-FIRST = if 0 exit then
   wid WID-FIRST 1+ = if 1 exit then
   BAD ;

: RECORD-ONE ( n -- ) {: row:n :}
   row NUNIT-CAPTURE:FIRST-REC + XREF-REC {: rec:ptr :}
   row DREC * REC-BUF {: dst:ptr :}
   rec XREF-EXT? if BAD then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if
      rec XREF-PKG-PUBLIC WID-SLOT 0<> if BAD then
      rec XREF-PKG-PRIVATE WID-SLOT 1 <> if BAD then
      0 dst LE:U64!
      1 dst 8 + LE:U64!
      exit
   then
   rec XREF-WORDLIST WID-SLOT dst 40 + LE:U64!
   rec XREF-START {: start:n :}
   start CODE-START < start CODE-END >= or if BAD then
   start CODE-START - dst LE:U64! ;

: RECORDS-PROFILE ( -- )
   REC-U @ DREC / 0 ?do i RECORD-ONE loop ;

: PREFIX-ORD ( n -- n ) {: target:n :}
   NUNIT-CAPTURE:FIRST-REC 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL <>
      rec XREF-WORDLIST XREF-RETIRED-WL <> and if
         rec XREF-START target = if i unloop exit then
      then
   loop
   BAD ;

: TARGET-REF ( n -- n n ) {: target:n :}
   target CODE-START >= target CODE-END < and if
      UNIT-REF target CODE-START - exit
   then
   PREFIX-REF target PREFIX-ORD ;

: SITE-CK ( n n -- ) {: site:n span:n :}
   site 0 < site INSN-BYTES mod 0<> or if BAD then
   span CODE-U @ > site CODE-U @ span - > or if BAD then ;

: CALL-ONE ( n -- ) {: row:n :}
   row NUNIT-CAPTURE:CALL@ {: site:n kind:n target:n :}
   site INSN-BYTES SITE-CK
   target TARGET-REF {: domain:n ref:n :}
   row CALL-BYTES * CALL-BUF {: dst:ptr :}
   site dst LE:U64!
   kind dst 8 + LE:U64!
   domain dst 16 + LE:U64!
   ref dst 24 + LE:U64!
   domain PREFIX-REF = if
      site CODE-START + {: at:n :}
      kind NEMIT:CALL = if at at NBR:BL-WORD
      else kind NEMIT:TAIL = if at at NBR:B-WORD
      else BAD then then
      site CODE-BUF LE:U32!
   then ;

: CALLS-PROFILE ( -- )
   NUNIT-CAPTURE:CALLS CALL-BYTES * {: size:n :}
   size 0 > if size CALL-BUF-RESERVE then
   size CALL-U !
   NUNIT-CAPTURE:CALLS 0 ?do i CALL-ONE loop ;

: ADDR-ONE ( n -- ) {: row:n :}
   row NUNIT-CAPTURE:ADDR@ {: site:n kind:n :}
   kind A64IR:ADDR-CODE <> if BAD then
   site SNAP-RELOC:ADDR-CHAIN-BYTES SITE-CK
   site CODE-BUF {: carrier:ptr :}
   carrier 0 CODE-BUF CODE-U @ + SNAP-RELOC:CHAIN-SIZE
      SNAP-RELOC:ADDR-CHAIN-BYTES <> if BAD then
   carrier SNAP-RELOC:ADDR-CHAIN-BYTES SNAP-RELOC:CHAIN-VALUE
      TARGET-REF {: domain:n ref:n :}
   row ADDR-BYTES * ADDR-BUF {: dst:ptr :}
   site dst LE:U64!
   domain dst 8 + LE:U64!
   ref dst 16 + LE:U64!
   carrier 0 SNAP-RELOC:ADDR-CHAIN-BYTES SNAP-RELOC:SET-CHAIN-VALUE ;

: ADDRS-PROFILE ( -- )
   NUNIT-CAPTURE:ADDRS ADDR-BYTES * {: size:n :}
   size 0 > if size ADDR-BUF-RESERVE then
   size ADDR-U !
   NUNIT-CAPTURE:ADDRS 0 ?do i ADDR-ONE loop ;

: PROTECTED? ( n -- bool ) {: wid:n :}
   wid 0 < wid PROT-WID-MAX >= or if BAD then
   data-base PROT-BITS-OFF + BYTE-VIEW wid 3 rshift + c@
   wid 7 and rshift 1 and 0<> ;

: PROTECTION-PROFILE ( -- )
   0
   WID-FIRST PROTECTED? if 1 or then
   WID-FIRST 1+ PROTECTED? if 2 or then
   PROTECTION c! ;

: COPY-IMPORT ( n -- ) {: slot:n :}
   slot NUNIT-FILE:SECTION$ {: a:ptr size:n :}
   slot 0= if
      size 0 <= size INSN-BYTES mod 0<> or if BAD then
      size CODE-BUF-RESERVE
      a 0 CODE-BUF size BYTE-COPY size CODE-U ! exit
   then
   slot 1 = if
      size 0 <= size DREC mod 0<> or if BAD then
      size REC-BUF-RESERVE
      a 0 REC-BUF size BYTE-COPY size REC-U ! exit
   then
   BAD ;

: IMPORT-REF ( n n -- n ) {: domain:n ref:n :}
   domain UNIT-REF = if
      ref 0 < ref CODE-U @ >= or ref INSN-BYTES mod 0<> or if BAD then
      IMPORT-CODE @ ref + exit
   then
   domain PREFIX-REF = if
      ref 0 < ref IMPORT-REC @ >= or if BAD then
      ref XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST XREF-NAMESPACE-WL = if BAD then
      rec XREF-WORDLIST XREF-RETIRED-WL = if BAD then
      rec XREF-START exit
   then
   BAD ;

: IMPORT-RECORD ( n -- ) {: row:n :}
   row DREC * REC-BUF {: dst:ptr :}
   dst 16 + LE:U64@ {: flags:n :}
   flags DNAME-EXT and 0<> if BAD then
   flags DNAME-LEN-MASK and 16 > if BAD then
   dst 40 + LE:U64@ {: wid:n :}
   wid XREF-NAMESPACE-WL = if
      row 0<> if BAD then
      dst LE:U64@ 0 <> dst 8 + LE:U64@ 1 <> or if BAD then
      IMPORT-WID @ dst LE:U64!
      IMPORT-WID @ 1+ dst 8 + LE:U64!
      exit
   then
   wid 0 < wid 1 > or if BAD then
   IMPORT-WID @ wid + dst 40 + LE:U64!
   dst LE:U64@ {: off:n :}
   off 0 < off CODE-U @ >= or off INSN-BYTES mod 0<> or if BAD then
   IMPORT-CODE @ off + dst LE:U64! ;

: IMPORT-RECORDS ( -- )
   REC-U @ DREC / 0 ?do i IMPORT-RECORD loop ;

: ROW-SECTION ( n n n -- ptr u8 ) {: slot:n row:n width:n :}
   slot NUNIT-FILE:SECTION$ {: a:ptr size:n :}
   size width mod 0<> if BAD then
   row 0 < row size width / >= or if BAD then
   a row width * + ;

: IMPORT-CALL ( n -- ) {: row:n :}
   2 row CALL-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ {: site:n :}
   site INSN-BYTES SITE-CK
   src 8 + LE:U64@ {: kind:n :}
   kind NEMIT:CALL <> kind NEMIT:TAIL <> and if BAD then
   src 16 + LE:U64@ src 24 + LE:U64@ IMPORT-REF {: target:n :}
   site CODE-BUF {: at:ptr :}
   at LE:U32@ {: old:n :}
   kind NEMIT:CALL = if
      old NBR:BL? 0= if BAD then
      IMPORT-CODE @ site + target NBR:BL-WORD at LE:U32!
   else
      old NBR:B? 0= if BAD then
      IMPORT-CODE @ site + target NBR:B-WORD at LE:U32!
   then ;

: IMPORT-CALLS ( -- )
   2 NUNIT-FILE:SECTION$ nip {: size:n :}
   size CALL-BYTES mod 0<> if BAD then
   size CALL-BYTES / 0 ?do i IMPORT-CALL loop ;

: IMPORT-ADDR ( n -- ) {: row:n :}
   3 row ADDR-BYTES ROW-SECTION {: src:ptr :}
   src LE:U64@ {: site:n :}
   site SNAP-RELOC:ADDR-CHAIN-BYTES SITE-CK
   src 8 + LE:U64@ src 16 + LE:U64@ IMPORT-REF {: target:n :}
   site CODE-BUF {: carrier:ptr :}
   carrier 0 CODE-BUF CODE-U @ + SNAP-RELOC:CHAIN-SIZE
      SNAP-RELOC:ADDR-CHAIN-BYTES <> if BAD then
   carrier target SNAP-RELOC:ADDR-CHAIN-BYTES SNAP-RELOC:SET-CHAIN-VALUE ;

: IMPORT-ADDRS ( -- )
   3 NUNIT-FILE:SECTION$ nip {: size:n :}
   size ADDR-BYTES mod 0<> if BAD then
   size ADDR-BYTES / 0 ?do i IMPORT-ADDR loop ;

: IMPORT-PROTECTION ( -- )
   4 NUNIT-FILE:SECTION$ {: a:ptr size:n :}
   size 1 <> if BAD then
   a c@ dup dup 3 and <> if BAD then
   PROTECTION c! ;

TRUSTED: PUBLISH ( ptr u8 n ptr u8 n -- ) native-unit-publish ;
TRUSTED: CALL-MAP ( n -- ) callmap-set ;
TRUSTED: ADDR-MAP ( n -- ) addrmap-set ;

: OUTSIDE-REGION? ( n -- bool ) {: target:n :}
   target dbase@ < if true exit then
   target dbase@ REGION + >= ;

: RESTORE-MAPS ( -- )
   2 NUNIT-FILE:SECTION$ nip CALL-BYTES / 0 ?do
      2 i CALL-BYTES ROW-SECTION {: src:ptr :}
      src 16 + LE:U64@ PREFIX-REF =
      src 8 + LE:U64@ NEMIT:CALL = and
      src 16 + LE:U64@ src 24 + LE:U64@ IMPORT-REF OUTSIDE-REGION? and if
         IMPORT-CODE @ src LE:U64@ + CALL-MAP
      then
   loop
   3 NUNIT-FILE:SECTION$ nip ADDR-BYTES / 0 ?do
      3 i ADDR-BYTES ROW-SECTION LE:U64@
      IMPORT-CODE @ + ADDR-MAP
   loop ;

: RESTORE-PROTECTION ( -- )
   PROTECTION c@ 1 and 0<> if IMPORT-WID @ prot-wid-add then
   PROTECTION c@ 2 and 0<> if IMPORT-WID @ 1+ prot-wid-add then ;

public

: EXPORT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n key:ptr keyu:n checker:ptr checku:n :}
   CODE-PROFILE RECORD-PROFILE RECORDS-PROFILE
   CALLS-PROFILE ADDRS-PROFILE PROTECTION-PROFILE
   NUNIT-FILE:RESET
   0 0 CODE-BUF CODE-U @ NUNIT-FILE:SECTION!
   1 0 REC-BUF REC-U @ NUNIT-FILE:SECTION!
   2 CALL-U @ 0= if 0 CODE-BUF else 0 CALL-BUF then CALL-U @ NUNIT-FILE:SECTION!
   3 ADDR-U @ 0= if 0 CODE-BUF else 0 ADDR-BUF then ADDR-U @ NUNIT-FILE:SECTION!
   4 PROTECTION 1 NUNIT-FILE:SECTION!
   5 checker checku NUNIT-FILE:SECTION!
   path pathu NUNIT-FILE:ARCH-AARCH64 s" NBR" key keyu NUNIT-FILE:WRITE ;

: IMPORT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n key:ptr keyu:n :}
   path pathu NUNIT-FILE:ARCH-AARCH64 s" NBR" key keyu NUNIT-FILE:READ
   0 COPY-IMPORT 1 COPY-IMPORT
   IMPORT-PROTECTION
   cp@ IMPORT-CODE ! ndict@ IMPORT-REC !
   AOT-ARM:WIDN IMPORT-WID !
   IMPORT-WID @ 2 + PROT-WID-MAX > if BAD then
   IMPORT-RECORDS IMPORT-CALLS IMPORT-ADDRS
   wordlist IMPORT-WID @ <> if BAD then
   wordlist IMPORT-WID @ 1+ <> if BAD then
   0 CODE-BUF CODE-U @ 0 REC-BUF REC-U @ DREC / PUBLISH
   RESTORE-MAPS RESTORE-PROTECTION
   5 NUNIT-FILE:SECTION$ ;

: CLOSE ( -- )
   CODE-BUF-RELEASE REC-BUF-RELEASE CALL-BUF-RELEASE ADDR-BUF-RELEASE
   NUNIT-FILE:CLOSE NUNIT-CAPTURE:CLOSE ;

;package
