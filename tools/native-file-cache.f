\ Native definition cache facade and explicit whole-file export/import.
\ The explicit driver captures one selected source in a named HFO range.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/core/checker-owner-extension.f
require src/habu/xref.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/session/lease.f
require src/compiler/native/compiler.f
require src/compiler/workspace.f
require tools/native-file-scope.f
require tools/native-file-inputs.f
require tools/native-unit-object.f
require tools/native-file-reference.f
require tools/native-unit-origin.f
require tools/native-file-object.f
require tools/native-definition-cache.f

package NFILE-CACHE
private

\ The explicit driver is compiled by the retained host before the replacement
\ checker owner ABI loads. These appended fields are selected by that bridge.
CHECKER-OWNER-EXT:RECORD-RESTORE-OFF CELL + constant CHECKER-RELEASE-OFF
CHECKER-RELEASE-OFF CELL + constant CHECKER-RELEASE-BYTES

DYNAMIC-BUFFER CHECKER-COPY u8
variable SELECT-SERIAL
variable SELECT-GEN
variable SELECT-EPOCH
variable SELECT-LEASE
variable SELECT-MODULE
variable SELECT-BUILD
variable CHECKER-PLANNING
variable EX-SESSION-BEFORE
variable EX-MODULE-BEFORE
variable EX-GEN-BEFORE
variable EX-EPOCH-BEFORE
variable EX-LEASE-BEFORE
variable EX-BUILD-BEFORE
variable EX-LAYOUT-BEFORE
create SCOPE-BUF OUTER:SCOPE-BYTES allot
variable EX-COMPILED
PTR-VARIABLE RETRACT-OWNER
TYPED-VARIABLE RETRACT-PRIOR [ n -- ]

CAST: ACTION-XT ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: CHECKER-PREPARE-XT ( n -- [ ptr u8 n n n -- n ] )
CAST: CHECKER-INSTALL-XT ( n -- [ n -- ] )
CAST: PUBLICATION-USE-XT ( n -- [ [ n -- ] -- ] )
CAST: CELLS-XT ( n -- [ [ ptr u8 n -- ] -- ] )
CAST: ORIGIN-USE-XT ( n -- [ [ n n -- ] -- ] )
CAST: RETRACT-XT ( n -- [ n -- ] )
CAST: RETRACT>N ( [ n -- ] -- n )

: BAD ( -- ) E-NUNIT-PROFILE throw ;
: OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

: OPTIONAL-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START ;

: TARGET-XT ( ptr u8 n -- n ) OPTIONAL-XT dup 0= if BAD then ;

: BIND-PUBLICATION ( -- )
   s" CHECKER-REG:PUBLICATION-USE" OPTIONAL-XT
      dup 0= if drop exit then
   ['] NFILE-REF:ON-PUBLISHED swap PUBLICATION-USE-XT execute ;

\ The source owner cuts failed checker rows before the engine reuses the
\ pending dictionary slot. Retire the matching unpublished FILE acceptance
\ at that same cut, including failures caught inside evaluate-closed.
: RETRACT-ROWS ( n -- )
   NFILE-REF:ORIGIN-ABANDON
   RETRACT-PRIOR @ execute ;

: BIND-RETRACT ( -- )
   OWNER {: owner:ptr :}
   owner CHECKER-OWNER-ABI:RETRACT-ROWS-OFF + CELL-VIEW @
      dup 0= if drop BAD then RETRACT-XT RETRACT-PRIOR !
   owner RETRACT-OWNER !
   ['] RETRACT-ROWS RETRACT>N
      owner CHECKER-OWNER-ABI:RETRACT-ROWS-OFF + CELL-VIEW ! ;

: CLEAR-RETRACT ( -- )
   RETRACT-OWNER @ {: owner:ptr :}
   owner NULL-PTR = if exit then
   RETRACT-PRIOR @ RETRACT>N
      owner CHECKER-OWNER-ABI:RETRACT-ROWS-OFF + CELL-VIEW !
   NULL-PTR RETRACT-OWNER ! ;

: CLEAR-PUBLICATION ( -- )
   s" CHECKER-REG:PUBLICATION-CLEAR" OPTIONAL-XT
      dup 0= if drop exit then ACTION-XT execute ;

: MARK-CHECKER ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ ACTION-XT execute ;
: CHECKER$ ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;
: CHECKER-RELEASE ( -- )
   OWNER {: owner:ptr :}
   owner NULL-PTR = if BAD then
   owner CELL - CELL-VIEW @ CHECKER-RELEASE-BYTES < if BAD then
   owner CHECKER-RELEASE-OFF + CELL-VIEW @
      dup 0= if drop BAD then ACTION-XT execute ;

: COPY-CHECKER ( -- ptr u8 n )
   CHECKER$ {: bytes:ptr size:n :}
   size CHECKER-COPY-RESERVE
   bytes 0 CHECKER-COPY size BYTE-COPY
   0 CHECKER-COPY size ;
: CHECKER-COPY$ ( -- ptr u8 n )
   ['] COPY-CHECKER ['] CHECKER-RELEASE finally ;
: CHECKER-BEGIN ( -- )
   CHECKER-PLANNING @ if BAD then
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF + CELL-VIEW @
      ACTION-XT execute
   1 CHECKER-PLANNING ! ;
: CHECKER-END ( -- )
   CHECKER-PLANNING @ 0= if exit then
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF + CELL-VIEW @
      ACTION-XT execute
   0 CHECKER-PLANNING ! ;
: CHECKER-PREPARE ( ptr u8 n n n -- n )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-OFF + CELL-VIEW @
      CHECKER-PREPARE-XT execute ;
: CHECKER-INSTALL ( n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-INSTALL-OFF + CELL-VIEW @
      CHECKER-INSTALL-XT execute ;
: IMPORTER ( -- [ ptr u8 n ptr u8 n ptr n n ptr n n -- ] )
   ['] NPUB:IMPORT-FILE-UNIT ;
: SCOPE-SAVE ( -- ) SCOPE-BUF OUTER:SCOPE-SAVE ;
: SCOPE-INSTALL ( ptr u8 -- ) OUTER:SCOPE-INSTALL ;
: SCOPE-PREPARE ( ptr u8 n n -- ) OUTER:SCOPE-PREPARE ;

: COMPILE-SEEN ( -- ) 1 EX-COMPILED ! ;

\ A named range is importable only when its recorded session and identity
\ history can be recreated in the receiver.
: SELECT-HISTORY ( n n n n n n n n n n n n n n n n n n bool -- bool )
   {: session:n module:n gen:n epoch:n before:n lease:n openlease:n
      openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
      beforegen:n beforeepoch:n beforelease:n
      build:n beforebuild:n openbuildbefore:n openbuildafter:n compiled:bool :}
   gen epoch IR-CTX:CHECK-HISTORY
   lease NLEASE:CHECK-HISTORY
   openlease NLEASE:CHECK-HISTORY
   build IR-BUILD:CHECK-HISTORY
   beforebuild IR-BUILD:CHECK-HISTORY
   openbuildbefore IR-BUILD:CHECK-HISTORY
   openbuildafter IR-BUILD:CHECK-HISTORY
   module IR-ID:SERIAL-CHECK drop
   beforemodule IR-ID:SERIAL-CHECK drop
   openmodulebefore IR-ID:SERIAL-CHECK drop
   openmoduleafter IR-ID:SERIAL-CHECK drop
   build beforebuild < if false exit then
   module beforemodule < if false exit then
   gen beforegen < epoch beforeepoch < or lease beforelease < or
      if false exit then
   session 0= if
      openlease 0<> openepoch 0<> or openmodulebefore 0<> or
      openmoduleafter 0<> or openbuildbefore 0<> or
      openbuildafter 0<> or if false exit then
   else
      session IR-CTX:SESSION-GEN-OF gen > if false exit then
      openlease 0= openlease lease > or if false exit then
      openepoch epoch > if false exit then
      openmoduleafter openmodulebefore <=
      openmoduleafter module > or if false exit then
      openbuildafter openbuildbefore <=
      openbuildafter build > or if false exit then
   then
   before 0<> session before <> and {: source-close:bool :}
   session 0<> session before <> and {: source-start:bool :}
   source-close if
      SELECT-SERIAL @ 0= if false exit then
      epoch beforeepoch - 1 < if false exit then
   then
   source-start if
      compiled 0= if false exit then
      session IR-CTX:SESSION-GEN-OF beforegen <= if false exit then
      openlease beforelease <= if false exit then
      openepoch beforeepoch < if false exit then
      openmodulebefore beforemodule < if false exit then
      openbuildbefore beforebuild < if false exit then
   then
   before 0= session 0= and SELECT-SERIAL @ 0<> and
      if false exit then
   compiled session 0<> and
      SELECT-SERIAL @ 0= source-close or and
      {: target-start:bool :}
   source-start if
      gen beforegen - 1 < if false exit then
      module beforemodule - openmoduleafter openmodulebefore - <
         if false exit then
      build beforebuild - openbuildafter openbuildbefore - <
         if false exit then
   then
   SELECT-GEN @ gen beforegen - +
      target-start if 1 else 0 then +
      source-start if 1 else 0 then - gen max {: nextgen:n :}
   SELECT-EPOCH @ epoch beforeepoch - + epoch max {: nextepoch:n :}
   SELECT-LEASE @ lease beforelease - +
      target-start if 1 else 0 then + lease max {: nextlease:n :}
   SELECT-MODULE @ module beforemodule - +
      target-start if openmoduleafter openmodulebefore - else 0 then +
      source-start if openmoduleafter openmodulebefore - else 0 then -
      module max {: nextmodule:n :}
   SELECT-BUILD @ build beforebuild - +
      target-start if openbuildafter openbuildbefore - else 0 then +
      source-start if openbuildafter openbuildbefore - else 0 then -
      build max {: nextbuild:n :}
   nextgen nextepoch IR-CTX:CHECK-HISTORY
   nextlease NLEASE:CHECK-HISTORY
   nextmodule IR-ID:SERIAL-CHECK drop
   nextbuild IR-BUILD:CHECK-HISTORY
   source-close source-start or target-start or if
      session SELECT-SERIAL !
   then
   nextgen SELECT-GEN ! nextepoch SELECT-EPOCH !
   nextlease SELECT-LEASE ! nextmodule SELECT-MODULE !
   nextbuild SELECT-BUILD !
   true ;

: SELECT-RANGE-HISTORY ( ptr u8 n bool -- bool )
   {: bytes:ptr size:n compiled:bool :}
   bytes size NFILE-OBJECT:SPLIT-RANGE
      {: unit:ptr unitu:n signature:ptr scope:ptr
         session:n module:n gen:n epoch:n before:n lease:n openlease:n
         openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
         beforegen:n beforeepoch:n beforelease:n
         build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unit drop unitu drop scope drop signature drop
   session module gen epoch before lease openlease openepoch beforemodule
      openmodulebefore openmoduleafter beforegen beforeepoch beforelease
      build beforebuild openbuildbefore openbuildafter compiled SELECT-HISTORY ;

: SELECT-CURRENT ( -- )
   NCOMP:SESSION-SERIAL SELECT-SERIAL !
   IR-CTX:HISTORY {: gen:n epoch:n :}
   gen SELECT-GEN ! epoch SELECT-EPOCH !
   NLEASE:HISTORY SELECT-LEASE !
   IR-ID:SERIAL-HISTORY SELECT-MODULE !
   IR-BUILD:HISTORY SELECT-BUILD ! ;

: STORE-RANGE ( n n n n ptr u8 n ptr u8 n ptr u8 n n n n n n n -- bool )
   {: h:n end:n dirty:n compiled:n key:ptr keyu:n inputs:ptr inputu:n
      signature:ptr signatureu:n before:n beforemodule:n beforegen:n
      beforeepoch:n beforelease:n beforebuild:n :}
   signatureu 32 <> if BAD then
   NFILE-REF:ENSURE-CAPTURE
   SCOPE-SAVE
   CHECKER-COPY$ {: checker:ptr checkeru:n :}
   key keyu checker checkeru s" FILE" inputs inputu
      NUNIT-OBJECT:PACK-NAMED-INPUTS true
      {: bytes:ptr size:n admitted:bool :}
   admitted 0= if false exit then
   NCOMP:SESSION-SERIAL {: session:n :}
   IR-CTX:SESSION-SERIAL session = {: compatible:bool :}
   IR-ID:SERIAL-HISTORY {: module:n :}
   IR-CTX:HISTORY {: gen:n epoch:n :}
   NLEASE:HISTORY {: lease:n :}
   h end dirty compiled bytes size signature SCOPE-BUF
      session module gen epoch before lease
      NCOMP:SESSION-LEASE NCOMP:SESSION-EPOCH beforemodule
      NCOMP:SESSION-MODULE-BEFORE NCOMP:SESSION-MODULE-AFTER
      beforegen beforeepoch beforelease
      IR-BUILD:HISTORY beforebuild
      NCOMP:SESSION-BUILD-BEFORE NCOMP:SESSION-BUILD-AFTER
      NFILE-OBJECT:ADD-RANGE
   compatible ;

\ Recreate an opening at its recorded offset from the range's start. The
\ receiver may have minted unrelated identities before it loaded this FILE.
: SAME-ZERO? ( n n -- bool )
   {: a:n b:n :}
   a 0= if b 0= exit then
   b 0<> ;

: REPLAY-SESSION ( n n n n n n n n n n n n n -- )
   {: session:n before:n openlease:n openepoch:n
      openmodulebefore:n openmoduleafter:n
      openbuildbefore:n openbuildafter:n
      beforegen:n beforeepoch:n beforelease:n
      beforemodule:n beforebuild:n :}
   before NCOMP:SESSION-SERIAL SAME-ZERO? 0= if BAD then
   before IR-CTX:SESSION-SERIAL SAME-ZERO? 0= if BAD then
   session before = if exit then
   IR-CTX:HISTORY {: livegen:n liveepoch:n :}
   NLEASE:HISTORY {: livelease:n :}
   IR-ID:SERIAL-HISTORY {: livemodule:n :}
   IR-BUILD:HISTORY {: livebuild:n :}
   before 0<> if IR-CTX:SESSION-CLOSE then
   session 0<> if
      livegen session IR-CTX:SESSION-GEN-OF beforegen - 1- +
      liveepoch openepoch beforeepoch - + IR-CTX:REPLAY-HISTORY
      livelease openlease beforelease - 1- + NLEASE:REPLAY-HISTORY
      livemodule openmodulebefore beforemodule - + IR-ID:REPLAY-SERIAL
      livebuild openbuildbefore beforebuild - + IR-BUILD:REPLAY-HISTORY
      NCOMP:SESSION-ENSURE
      NCOMP:SESSION-SERIAL 0= if BAD then
      livegen session IR-CTX:SESSION-GEN-OF beforegen - +
         NCOMP:SESSION-SERIAL IR-CTX:SESSION-GEN-OF <> if BAD then
      livelease openlease beforelease - +
         NCOMP:SESSION-LEASE <> if BAD then
      liveepoch openepoch beforeepoch - +
         NCOMP:SESSION-EPOCH <> if BAD then
      livemodule openmodulebefore beforemodule - +
         NCOMP:SESSION-MODULE-BEFORE <> if BAD then
      livemodule openmoduleafter beforemodule - +
         NCOMP:SESSION-MODULE-AFTER <> if BAD then
      livebuild openbuildbefore beforebuild - +
         NCOMP:SESSION-BUILD-BEFORE <> if BAD then
      livebuild openbuildafter beforebuild - +
         NCOMP:SESSION-BUILD-AFTER <> if BAD then
   then ;

: RESTORE-RANGE ( ptr u8 n n -- )
   {: bytes:ptr size:n prepared:n :}
   bytes size NFILE-OBJECT:SPLIT-RANGE
      {: unitbytes:ptr unitu:n signature:ptr scope:ptr
         session:n module:n gen:n epoch:n before:n lease:n openlease:n
         openepoch:n beforemodule:n openmodulebefore:n openmoduleafter:n
         beforegen:n beforeepoch:n beforelease:n
         build:n beforebuild:n openbuildbefore:n openbuildafter:n :}
   unitbytes drop unitu drop signature drop
   module IR-ID:SERIAL-CHECK drop
   gen epoch IR-CTX:CHECK-HISTORY
   lease NLEASE:CHECK-HISTORY
   IR-CTX:HISTORY {: livegen:n liveepoch:n :}
   NLEASE:HISTORY {: livelease:n :}
   IR-ID:SERIAL-HISTORY {: livemodule:n :}
   IR-BUILD:HISTORY {: livebuild:n :}
   session before openlease openepoch openmodulebefore openmoduleafter
      openbuildbefore openbuildafter beforegen beforeepoch beforelease
      beforemodule beforebuild REPLAY-SESSION
   livemodule module beforemodule - + IR-ID:REPLAY-SERIAL
   livegen gen beforegen - + liveepoch epoch beforeepoch - +
      IR-CTX:REPLAY-HISTORY
   livelease lease beforelease - + NLEASE:REPLAY-HISTORY
   livebuild build beforebuild - + IR-BUILD:REPLAY-HISTORY
   IMPORTER {: importer :}
   importer prepared true NUNIT-OBJECT:INSTALL-PREPARED 2drop
   prepared NUNIT-OBJECT:PREPARED-CHECKER-ID CHECKER-INSTALL
   scope SCOPE-INSTALL
   prepared NUNIT-OBJECT:PREPARED-ORIGIN-OF$ NUNIT-ORIGIN:INSTALL
   NUNIT-OBJECT:PREPARED-INPUTS$ NFILE-INPUTS:APPEND
   NUNIT-OBJECT:CLOSE-OBJECT ;

public

\ Keep the existing build entry points while the definition driver owns
\ automatic source loads.
: RUN ( [ -- ] -- )
   NDEF-CACHE:RUN ;
: RETAIN-HOST-CHECKER ( -- ) NDEF-CACHE:RETAIN-HOST-CHECKER ;
: REBIND ( -- ) NDEF-CACHE:REBIND ;
: HANDOVER-PREPARE ( -- ) NDEF-CACHE:HANDOVER-PREPARE ;
: HANDOVER-BIND ( -- ) NDEF-CACHE:HANDOVER-BIND ;
: BORROW ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- ) NDEF-CACHE:BORROW ;
: RETURN-BORROW ( -- ) NDEF-CACHE:RETURN-BORROW ;
: ARM-NATIVE-ACCEPT ( [ ptr u8 n -- ] -- bool )
   NDEF-CACHE:ARM-NATIVE-ACCEPT ;
: SUSPEND-DISPATCH ( -- ) NDEF-CACHE:SUSPEND-DISPATCH ;
: DEFINITIONS-CLOSE ( -- ) NDEF-CACHE:DEFINITIONS-CLOSE ;
: WITH ( ptr u8 n [ -- ] -- )
   NDEF-CACHE:WITH ;
: WITH-QUIET ( ptr u8 n [ -- ] -- )
   NDEF-CACHE:WITH-QUIET ;

: APP-LOAD ( ptr u8 n ptr u8 n bool -- )
   NDEF-CACHE:APP-LOAD ;

: LOAD ( ptr u8 n ptr u8 n -- )
   NDEF-CACHE:LOAD ;

: LOAD-WITH-INPUTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   NDEF-CACHE:LOAD-WITH-INPUTS ;

: LOAD-WITH-INPUTS-QUIET ( ptr u8 n ptr u8 n ptr u8 n -- )
   NDEF-CACHE:LOAD-WITH-INPUTS-QUIET ;

\ The explicit command stores one complete selected source as an HFO range.
\ Its native range includes nested loads, so no dependent cache files are
\ needed to move or import the named artifact.
: EXPLICIT-START ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n :}
   NLEASE:IDLE? 0= if BAD then
   IR-CTX:REGISTRY-IDLE? IR-CTX:REGISTRY-SESSION-ONLY? or
      0= if BAD then
   IR-CTX:SESSION-SERIAL NCOMP:SESSION-SERIAL <> if BAD then
   NFILE-REF:ORIGIN-CLEAR
   NFILE-REF:BIND-SEED
   path pathu root rootu 0 NFILE-REF:PUSH-FILE
   NFILE-REF:BEGIN-RANGE
   ['] NFILE-REF:DECL-KEY$ ['] NFILE-REF:DECL-BY-KEY
      CWORK:FILE-IDENTITY-SET
   NUNIT-OBJECT:BIND-HOST-STORAGE
   OWNER CHECKER-OWNER-ABI:UNIT-STORAGE-CELLS-OFF + CELL-VIEW @
      CELLS-XT NUNIT-OBJECT:BIND-CHECKER-CELLS
   NDEF-CACHE:BIND-EXPLICIT-FACTS
   BIND-PUBLICATION
   BIND-RETRACT
   ['] NFILE-REF:ORIGIN-ACCEPT
      s" ENGINE-ORIGIN:FILE-ACCEPT-USE" TARGET-XT
      ORIGIN-USE-XT execute
   s" ENGINE-ORIGIN:ARM" TARGET-XT ACTION-XT execute
   s" ENGINE-ORIGIN:INSTALL" TARGET-XT ACTION-XT execute
   MARK-CHECKER
   0 EX-COMPILED !
   ['] COMPILE-SEEN NFEED:FILE-COMPILE-USE
   ['] OWNER 0 NUNIT-ORIGIN:BIND
   NUNIT-ORIGIN:MARK
   SELECT-CURRENT
   SELECT-SERIAL @ EX-SESSION-BEFORE !
   SELECT-MODULE @ EX-MODULE-BEFORE !
   SELECT-GEN @ EX-GEN-BEFORE !
   SELECT-EPOCH @ EX-EPOCH-BEFORE !
   SELECT-LEASE @ EX-LEASE-BEFORE !
   SELECT-BUILD @ EX-BUILD-BEFORE !
   CWORK:LAYOUT-MARK EX-LAYOUT-BEFORE ! ;

: EXPLICIT-HISTORY? ( -- bool )
   NCOMP:SESSION-SERIAL {: session:n :}
   IR-ID:SERIAL-HISTORY {: module:n :}
   IR-CTX:HISTORY {: gen:n epoch:n :}
   NLEASE:HISTORY {: lease:n :}
   session module gen epoch EX-SESSION-BEFORE @ lease
      NCOMP:SESSION-LEASE NCOMP:SESSION-EPOCH EX-MODULE-BEFORE @
      NCOMP:SESSION-MODULE-BEFORE NCOMP:SESSION-MODULE-AFTER
      EX-GEN-BEFORE @ EX-EPOCH-BEFORE @ EX-LEASE-BEFORE @
      IR-BUILD:HISTORY EX-BUILD-BEFORE @
      NCOMP:SESSION-BUILD-BEFORE NCOMP:SESSION-BUILD-AFTER
      EX-COMPILED @ SELECT-HISTORY ;

: EXPLICIT-EXPORT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 ptr u8 n n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
      signature:ptr artifact:ptr artifactu:n sourceu:n
      inputs:ptr inputu:n :}
   CWORK:LAYOUT-MARK EX-LAYOUT-BEFORE @ <> if BAD then
   path pathu root rootu key keyu sourceu NFILE-OBJECT:START {: h:n :}
   h sourceu 1 EX-COMPILED @ key keyu inputs inputu signature 32
      EX-SESSION-BEFORE @ EX-MODULE-BEFORE @ EX-GEN-BEFORE @
      EX-EPOCH-BEFORE @ EX-LEASE-BEFORE @ EX-BUILD-BEFORE @
      STORE-RANGE {: compatible:bool :}
   compatible 0= if BAD then
   EXPLICIT-HISTORY? 0= if BAD then
   h artifact artifactu inputs inputu NFILE-OBJECT:WRITE ;

: EXPLICIT-IMPORT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 ptr u8 n n -- )
   {: path:ptr pathu:n root:ptr rootu:n key:ptr keyu:n
      signature:ptr artifact:ptr artifactu:n sourceu:n :}
   artifact artifactu path pathu root rootu key keyu sourceu
      NFILE-OBJECT:TRY-OPEN {: h:n hit:bool :}
   hit 0= if BAD then
   h NFILE-OBJECT:NEXT
      {: kind:n end:n mode:n bytes:ptr size:n more:bool :}
   more 0= kind NFILE-OBJECT:RANGE-KIND <> or
      end sourceu <> or if BAD then
   h NFILE-OBJECT:NEXT
      {: extra-kind:n extra-end:n extra-mode:n extra:ptr extrau:n extra?:bool :}
   extra-kind drop extra-end drop extra-mode drop extra drop extrau drop
   extra? if BAD then
   bytes 8 + bytes LE:U64@ + 32 signature 32 STR= 0= if BAD then
   bytes size mode 2 and 0<> SELECT-RANGE-HISTORY 0= if BAD then
   CHECKER-BEGIN
   bytes 8 + bytes LE:U64@ key keyu s" FILE"
      NUNIT-OBJECT:PREPARE-BYTES-NAMED {: prepared:n :}
   prepared NUNIT-OBJECT:PREPARED-CHECKER-OF$
      CHECKER-PREPARE prepared swap
      NUNIT-OBJECT:PREPARED-CHECKER-ID!
   prepared NUNIT-OBJECT:PREPARED-ORIGIN-OF$
      prepared NUNIT-OBJECT:PREPARED-CHECKER-ID
      [: NUNIT-OBJECT:PROJECT-CALLABLE? ;] NUNIT-ORIGIN:PREPARE
   bytes 8 + bytes LE:U64@ + 32 +
      NUNIT-OBJECT:PROJECTED-COUNTS SCOPE-PREPARE
   bytes size prepared RESTORE-RANGE
   NUNIT-OBJECT:DISCARD-PREPARED
   CHECKER-END ;

: EXPLICIT-CLOSE ( -- )
   CLEAR-RETRACT
   s" ENGINE-ORIGIN:FILE-ACCEPT-CLEAR" TARGET-XT ACTION-XT execute
   NFILE-REF:ORIGIN-CLEAR
   CLEAR-PUBLICATION
   CHECKER-END
   NDEF-CACHE:CLEAR-EXPLICIT-FACTS
   NUNIT-ORIGIN:RESET
   NFEED:FILE-COMPILE-DEFAULT
   0 EX-COMPILED !
   NUNIT-DEP:RESET NUNIT-DEP:UNBIND
   NFILE-REF:END-RANGE
   NFILE-OBJECT:CLOSE NUNIT-OBJECT:CLOSE
   NUNIT-OBJECT:CLEAR-HOST-STORAGE
   CWORK:FILE-CLOSE
   CHECKER-COPY-RELEASE
   0 EX-SESSION-BEFORE ! 0 EX-MODULE-BEFORE !
   0 EX-GEN-BEFORE ! 0 EX-EPOCH-BEFORE ! 0 EX-LEASE-BEFORE !
   0 EX-BUILD-BEFORE ! 0 EX-LAYOUT-BEFORE !
   0 SELECT-SERIAL ! 0 SELECT-MODULE !
   0 SELECT-GEN ! 0 SELECT-EPOCH ! 0 SELECT-LEASE !
   0 SELECT-BUILD ! ;

;package
