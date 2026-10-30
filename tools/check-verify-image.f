\ check-verify-image.f - the saved verifier image tools/check-verify-core.f runs its
\ child on: img/check-verify under the out root, built once per engine and closure
\ by tools/check-verify-save.f. A failed build dies or throws (tools/keyed-image.f
\ BUILD-RUN, PUBLISH). No IMAGE-GRANT:CHECK: this is a tool, not a test adapter, so
\ an ungranted gate row builds its own keyed image beside the gate's.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require src/lib/engine-id.f
require src/lib/engine-candidate.f
require tools/build-out.f
require tools/keyed-image.f
require tools/native-file-inputs.f

package CHECK-VERIFY-IMAGE

PATH-CAP 128 + constant PROGRAM-CAP

create ROOT PATH-CAP allot          variable ROOT-U
create ENTRY PATH-CAP allot         variable ENTRY-U
create PROGRAM PROGRAM-CAP allot       variable PROGRAM-U
create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF PATH-CAP allot      variable PATH-U
create ENGINE-BUF PATH-CAP allot    variable ENGINE-U      \ the engine PATH$ is asked for
create KEYED-ENGINE PATH-CAP allot  variable KEYED-ENGINE-U \ the engine the key was made for
create CANON PATH-CAP allot         variable CANON-U       \ this engine's canonical path
DYNAMIC-BUFFER STAMPS u8               variable STAMPS-U      \ the stamp rows of the last resolve
TYPED-VARIABLE RESOLVED bool

: FAMILY$ ( -- ptr u8 n ) s" check-verify" ;
: VERSION$ ( -- ptr u8 n ) s" check-verify-image-v1" ;
: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: PROGRAM$ ( -- ptr u8 n ) PROGRAM PROGRAM-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE-BUF ENGINE-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: PROGRAM+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PROGRAM-U @ u + PROGRAM-CAP > if E-STR-CAPACITY throw then
   a PROGRAM PROGRAM-U @ + u BYTE-COPY
   PROGRAM-U @ u + PROGRAM-U ! ;

\ While this file loads, SOURCE-ROOT:CURRENT$ is the root that resolved it
\ (tools/check-verify-core.f fixes its child the same way). ENTRY is the
\ canonical path of the save driver and ROOT its tree, the tree
\ tools/check-verify-save.f ROOT! builds from, so the key walks the closure the
\ build loads: a root that links to a tree (tools/check-test-lib.f LC-ROOT)
\ keys the same files and shares the image.
SOURCE-ROOT:CURRENT$ s" tools/check-verify-save.f" ENTRY FS:JOIN-PATH
ENTRY swap SOURCE-ROOT:CANONICAL drop ENTRY ENTRY-U COPY!
ENTRY$ SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME ROOT ROOT-U COPY!
0 PROGRAM-U !
s\" tick-order@\n1 set-tier\ns\" " PROGRAM+
ENTRY$ PROGRAM+
s\" \" required\nCHECK-VERIFY-SAVE:TICK!\n0 SCRIPT-ARGV$ CHECK-VERIFY-SAVE:SAVE\n" PROGRAM+

: STAMP ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-CTIME ;
: OUT-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: FILL-KEY ( CONTENT-KEY:fold -- )
   VERSION$ CONTENT-KEY:TEXT+
   ENGINE$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   ENGINE$ SOURCE-ROOT:CANONICAL drop CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   ENTRY$ ROOT$ [: STAMP ;] KEYED-IMAGE:CLOSURE-IN-EACH+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

\ The key: the version text, the engine's bytes, the canonical engine path the
\ image bakes, the program, and the content and canonical path of every file in the
\ entry's closure (tools/keyed-image.f CLOSURE-IN-EACH+). Change time and size
\ stamps (tools/native-file-inputs.f FILE rows) are kept and PATH$ checks them on
\ every call; an edit racing a build belongs to the next re-key.
: RESOLVE-IN ( -- )
   ENGINE$ STAMP
   [: FILL-KEY ;] CONTENT-KEY:WITH-FOLD
   ENGINE$ ENGINE-KEY BUILD-OUT:KEY-OF!
   FAMILY$ ENGINE-KEY PATH-BUF PATH-U KEYED-IMAGE:PATH!
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   rowsu 1 max STAMPS-RESERVE
   rows 0 STAMPS rowsu BYTE-COPY  rowsu STAMPS-U !
   ENGINE$ KEYED-ENGINE KEYED-ENGINE-U COPY!
   true RESOLVED ! ;

\ The inputs table is process-global: bracketed as tools/hb-build-lib.f
\ HBB-MAKER-KEY-IN-ROOT brackets it.
: RESOLVE ( -- )
   false RESOLVED !
   NFILE-INPUTS:MARK {: end:n :}
   end NFILE-INPUTS:BYTES$ nip - {: first:n :}
   end NFILE-INPUTS:FROM
   ['] RESOLVE-IN catch {: rc:n :}
   end NFILE-INPUTS:TRUNCATE
   first NFILE-INPUTS:FROM
   rc 0<> if rc throw then ;

\ The last resolve still holds: same engine, every stamp as recorded.
: FRESH? ( -- bool )
   RESOLVED @ 0= if false exit then
   ENGINE$ KEYED-ENGINE KEYED-ENGINE-U @ STR= 0= if false exit then
   [: 0 STAMPS STAMPS-U @ NFILE-INPUTS:CHECK ;] catch {: rc:n :}
   rc 0= if true exit then
   rc E-NUNIT-PROFILE = rc E-NUNIT-FILE = or if false exit then
   rc throw ;

public

\ This process's own engine, canonical path for canonical path, providing
\ src/compiler/native/compiler.f, which every native-build product carries and the
\ Gforth recovery prefix does not. Any other engine (HABU_UNDER_TEST naming another,
\ a shebang script) runs tools/check-verify-child.f, verdicts unchanged.
: CAPABLE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   ENGINE-ID:PATH$ SOURCE-ROOT:CANONICAL drop CANON CANON-U COPY!
   a u SOURCE-ROOT:CANONICAL drop CANON CANON-U @ STR= 0= if false exit then
   s" src/compiler/native/compiler.f" ENGINE-PROVIDES? ;

\ The image for the engine at this path, built within the timeout (ms) unless
\ it was published under its key. Take it before staging a child's argv or env: a build
\ stages its own in the same process-wide tables.
: PATH$ ( ptr u8 n n -- ptr u8 n )
   {: a:ptr u:n timeout:n :}
   a u ENGINE-BUF ENGINE-U COPY!
   FRESH? 0= if RESOLVE then
   KEY-HEX FAMILY$ ENGINE-KEY ENGINE$ PROGRAM$ ['] OUT-ARG timeout
   KEYED-IMAGE:ENSURE-WITH drop
   PATH-BUF PATH-U @ ;

\ The gate's build row: the image for the candidate engine (test/gate-images.f).
: SETTLE ( n -- )
   {: timeout:n :}
   ENGINE-CANDIDATE:PATH$ timeout PATH$ 2drop ;

;package
