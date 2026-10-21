\ check-verify-image.f - the saved verifier image tools/check-verify-core.f
\ runs its child on: hb-check-verify-<key> in the build cache, the engine with
\ the verifier and tools/check-verify-child-lib.f loaded at tier 1 and saved by
\ tools/check-verify-save.f, built once per engine and closure.
\
\ The key is the version text, the engine's bytes and the source root the image
\ bakes for it (SOURCE-ROOT:ENGINE-FOR$, tools/check-verify-save.f), the program
\ (which names the entry under ROOT, the tree this file was loaded from) and the
\ content and canonical path of every file of the entry's closure
\ (tools/keyed-image.f CLOSURE-IN-EACH+). Each file's mtime and size stamp
\ (tools/native-file-inputs.f FILE rows) is kept, and PATH$ checks the stamps on
\ every call: a change re-keys and builds again; an edit racing a build belongs to
\ the next re-key, as for hb-build's makers (tools/hb-build-lib.f). A build that
\ fails dies or throws (tools/keyed-image.f BUILD-RUN, PUBLISH); there is no
\ fallback.
\
\ CAPABLE? says whether a child engine can run the image: it is this process's
\ own engine, canonical path for canonical path, and that engine provides
\ src/compiler/native/compiler.f, a row of every native-build product and of
\ nothing the Gforth recovery prefix loads. Any other engine - an HABU_UNDER_TEST
\ naming another engine, a test's shebang script - runs the source child,
\ tools/check-verify-child.f, verdicts unchanged.
\
\ No IMAGE-GRANT:CHECK: this is a tool, not a test adapter (grants stay in
\ test/, tools/keyed-image.f:5). A gate row that reaches it ungranted builds its
\ own keyed image, an atomic rename beside the gate's, instead of dying.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require src/lib/engine-id.f
require src/lib/engine-candidate.f
require tools/keyed-image.f
require tools/native-file-inputs.f

package CHECK-VERIFY-IMAGE

FS-PATH-CAP 128 + constant PROGRAM-CAP

create ROOT FS-PATH-CAP allot          variable ROOT-U
create ENTRY FS-PATH-CAP allot         variable ENTRY-U
create PROGRAM PROGRAM-CAP allot       variable PROGRAM-U
create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot      variable PATH-U
create ENGINE-BUF FS-PATH-CAP allot    variable ENGINE-U      \ the engine PATH$ is asked for
create KEYED-ENGINE FS-PATH-CAP allot  variable KEYED-ENGINE-U \ the engine the key was made for
create CANON FS-PATH-CAP allot         variable CANON-U       \ this engine's canonical path
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
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: PROGRAM+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PROGRAM-U @ u + PROGRAM-CAP > if E-STR-CAPACITY throw then
   a PROGRAM PROGRAM-U @ + u BYTE-COPY
   PROGRAM-U @ u + PROGRAM-U ! ;

\ While this file loads, SOURCE-ROOT:CURRENT$ is the root that resolved it
\ (tools/check-verify-core.f fixes its child the same way).
SOURCE-ROOT:CURRENT$ ROOT ROOT-U COPY!
ROOT$ s" tools/check-verify-save.f" ENTRY JOIN-PATH ENTRY-U !
0 PROGRAM-U !
s\" tick-order@\n1 set-tier\ns\" " PROGRAM+
ENTRY$ PROGRAM+
s\" \" required\nCHECK-VERIFY-SAVE:TICK!\n0 SCRIPT-ARGV$ CHECK-VERIFY-SAVE:SAVE\n" PROGRAM+

: STAMP ( ptr u8 n -- ) s" " NFILE-INPUTS:ADD-MTIME ;
: OUT-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: FILL-KEY ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   VERSION$ CONTENT-KEY:TEXT+
   ENGINE$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   ENGINE$ SOURCE-ROOT:ENGINE-FOR$ CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   ENTRY$ ROOT$ [: STAMP ;] KEYED-IMAGE:CLOSURE-IN-EACH+ ;

\ Key, path and stamps, from the rows this resolve adds after MARK. A refused
\ walk releases its fold: the pool is process-wide and a few leaks would refuse
\ every later key derivation.
: RESOLVE-IN ( -- )
   ENGINE$ STAMP
   CONTENT-KEY:OPEN {: fold:CONTENT-KEY:fold :}
   fold [: FILL-KEY ;] catch {: rc:n :} drop
   rc 0<> if fold CONTENT-KEY:DISCARD rc throw then
   fold KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
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

\ Whether the engine at this path can run the image: this process's own
\ engine, and one that provides the native compiler.
: CAPABLE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   ENGINE-ID:PATH$ SOURCE-ROOT:CANONICAL drop CANON CANON-U COPY!
   a u SOURCE-ROOT:CANONICAL drop CANON CANON-U @ STR= 0= if false exit then
   s" src/compiler/native/compiler.f" ENGINE-PROVIDES? ;

\ The image for the engine at this path, built within the timeout (ms) unless
\ it is in the cache. Take it before staging a child's argv or env: a build
\ stages its own in the same process-wide tables.
: PATH$ ( ptr u8 n n -- ptr u8 n )
   {: a:ptr u:n timeout:n :}
   a u ENGINE-BUF ENGINE-U COPY!
   FRESH? 0= if RESOLVE then
   FAMILY$ PATH-BUF PATH-U @ ENGINE$ PROGRAM$ ['] OUT-ARG timeout
   KEYED-IMAGE:ENSURE-WITH drop
   PATH-BUF PATH-U @ ;

\ The gate's build row: the image for the candidate engine (test/gate-images.f).
: SETTLE ( n -- )
   {: timeout:n :}
   ENGINE-CANDIDATE:PATH$ timeout PATH$ 2drop ;

;package
