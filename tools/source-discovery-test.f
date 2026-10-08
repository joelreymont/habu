\ source-discovery-test.f - checked fixtures for the whole-file discovery pass.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/source.f tools/source-discovery.f tools/source-discovery-test.f
\
\ Proves the ordered event artifact for include/require/provided mixes (include
\ replay-every-occurrence vs require dedup), canonical registry (equivalent spelling
\ collapse), tool-preloaded require paths not hiding a later user require,
\ colon-body loader capture with byte-exact token spans, a parsing keyword's
\ operand read as data, a comment between a literal path and the word that takes
\ it skipped as the loader skips it, the shared checked path emitter, fail-closed
\ rejection when the artifact cannot be produced (loader word
\ shadowed/undefined/retired, dynamic loader path, unsupported opener,
\ serialization overflow), a body's loader or retirement read past as the run
\ of a word no check runs, and the dynamic-tail manifest (the loader's own
\ definition site tolerated).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/source.f
require tools/source-discovery.f

package SD-TEST
using SOURCE-ROOT

FS-PATH-CAP constant SDT-PC
$1000 constant SDT-SRC-CAP

create SDT-ROOT SDT-PC allot
create SDT-ENTRY SDT-PC allot
create SDT-OUT $2000 allot
create SDT-SRC SDT-SRC-CAP allot
\ The emitted text names two canonical fixture paths: more than SB holds.
SDT-PC 2 * $40 + constant SDT-WANT-CAP
create SDT-WANT SDT-WANT-CAP allot
variable SDT-WANT-U
variable SDT-ROOT-U
variable SDT-ENTRY-U
variable SDT-SRC-U

: SDT-ROOT$ ( -- ptr u8 n )   SDT-ROOT SDT-ROOT-U @ ;
: SDT-ENTRY$ ( -- ptr u8 n )  SDT-ENTRY SDT-ENTRY-U @ ;

: SDT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: SDT-PREP ( -- )
   CLEANUP-RESET
   s" habu-source-discovery-test" HB-TMP-MKDIR SDT-ROOT SDT-ROOT-U SDT-COPY!
   SDT-ROOT$ CLEANUP-TREE+ ;

: SDT-WRITE-ENTRY ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n content:ptr contentu:n :}
   SDT-ROOT$ name nameu SDT-ENTRY JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ content contentu WRITE-ALL ;

: SDT-PATH ( ptr u8 n -- ptr u8 n )
   SDT-ROOT$ 2swap JOIN CANONICAL drop ;

: SDT-DISCOVER ( -- )  SDT-ENTRY$ DISCOVER:RUN ;

: SDT-WANT+ ( ptr u8 n -- )  SDT-WANT SDT-WANT-CAP SDT-WANT-U BUF-APPEND ;

: SDT-MIXED$ ( -- ptr u8 n )
   S\" require sd-a.f\ninclude sd-b.f\ninclude sd-b.f\ns\" sd-c.f\" required\ns\" sd-c.f\" required\ns\" sd-d.f\" provided\n: HELPER ( n -- n ) dup + ;\nrequire sd-e.f\n" ;

: SDT-TEST-MIXED ( -- )
   s" mixed.f" SDT-MIXED$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 7 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-KIND@ EV-INCLUDED T=
   2 EVENT-KIND@ EV-INCLUDED T=
   1 EVENT-PATH@ s" sd-b.f" SDT-PATH T$=
   3 EVENT-KIND@ EV-REQUIRED T=
   3 EVENT-STATE@ EV-STATE-FRESH T=
   4 EVENT-STATE@ EV-STATE-KNOWN T=
   5 EVENT-KIND@ EV-PROVIDED T=
   6 EVENT-KIND@ EV-REQUIRED T=
   6 EVENT-PATH@ s" sd-e.f" SDT-PATH T$= ;

: SDT-SPELLING$ ( -- ptr u8 n )
   S\" s\" ./sd-f.f\" required\ns\" sd-f.f\" required\n" ;

: SDT-TEST-SPELLING ( -- )
   s" spelling.f" SDT-SPELLING$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-KNOWN T=
   0 EVENT-PATH@ s" sd-f.f" SDT-PATH T$=
   1 EVENT-PATH@ s" sd-f.f" SDT-PATH T$= ;

: SDT-FRESH$ ( -- ptr u8 n )
   S\" s\" sd-tool.f\" required\ns\" sd-user.f\" required\n" ;

: SDT-TEST-FRESH ( -- )
   REQUIRE-N @ {: save-n:n :}
   s" sd-tool.f" SDT-PATH provided
   s" fresh.f" SDT-FRESH$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   0 EVENT-PATH@ s" sd-tool.f" SDT-PATH T$=
   1 EVENT-STATE@ EV-STATE-FRESH T=
   save-n REQUIRE-N ! ;

\ An entry with more direct dependencies than 256: discovery records an event
\ for each require, in order, with its own path. Line k requires sd-wXY.f,
\ where XY spells k in two letters, so line 256 requires sd-wjw.f.
: SDT-WIDE-LINE$ ( -- ptr u8 n )  S\" require sd-waa.f\n" ;
17 constant SDT-WIDE-LINE                \ the bytes of SDT-WIDE-LINE$
257 constant SDT-WIDE-N
create SDT-WIDE SDT-WIDE-N SDT-WIDE-LINE * allot

: SDT-WIDE-LINE! ( n -- )
   {: k:n :}
   k SDT-WIDE-LINE * SDT-WIDE + {: at:ptr :}
   SDT-WIDE-LINE$ {: l:ptr lu:n :}
   l at lu BYTE-COPY
   k 26 / $61 + at 12 + c!
   k 26 mod $61 + at 13 + c! ;

: SDT-TEST-WIDE ( -- )
   REQUIRE-N @ {: save-n:n :}
   SDT-WIDE-N 0 ?do i SDT-WIDE-LINE! loop
   s" wide.f" SDT-WIDE SDT-WIDE-N SDT-WIDE-LINE * SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT SDT-WIDE-N T=
   256 EVENT-KIND@ EV-REQUIRED T=
   256 EVENT-STATE@ EV-STATE-FRESH T=
   256 EVENT-PATH@ s" sd-wjw.f" SDT-PATH T$=
   0 EVENT-PATH@ s" sd-waa.f" SDT-PATH T$=
   save-n REQUIRE-N ! ;

: SDT-TEST-EMIT ( -- )
   s" emit.f" S\" require sd-x.f\ns\" sd-y.f\" provided\n" SDT-WRITE-ENTRY
   SDT-DISCOVER
   SDT-OUT $2000 DISCOVER:EMIT {: elen:n :}
   SDT-WANT-U BUF-RESET
   S\" required 0 s\" " SDT-WANT+
   s" sd-x.f" SDT-PATH SDT-WANT+
   S\" \"\nprovided 0 s\" " SDT-WANT+
   s" sd-y.f" SDT-PATH SDT-WANT+
   S\" \"\n" SDT-WANT+
   SDT-OUT elen SDT-WANT SDT-WANT-U @ T$= ;

: SDT-RUN-ENTRY ( -- )   SDT-DISCOVER ;

\ `kernel:` is the engine's synonym for `:`, so it shadows a loader word too.
: SDT-TEST-SHADOW ( -- )
   s" shadow.f" S\" : required ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-SHADOW TTHROWSQ
   s" kernel-shadow.f" S\" kernel: required ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-SHADOW TTHROWSQ ;

: SDT-TEST-UNDEFINE ( -- )
   s" undef.f" S\" undefine required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-SHADOW TTHROWSQ ;

: SDT-TEST-DYNAMIC ( -- )
   s" dyn.f" S\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ ;

: SDT-TEST-OPENER ( -- )
   s" opener.f" S\" C\\\" sd-g.f\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-OPENER TTHROWSQ ;

\ --- whole-file scan: colon-body loaders are events, spans byte-exact --------

: SDT-COLON$ ( -- ptr u8 n )
   S\" : MAYBE ( -- ) s\" sd-h.f\" required ;\nMAYBE\n" ;

: SDT-TEST-COLON-BODY ( -- )
   s" colon.f" SDT-COLON$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   0 EVENT-PATH@ s" sd-h.f" SDT-PATH T$= ;

: SDT-EVENT-TOK$ ( n -- ptr u8 n ) {: ix:n :}
   ix EVENT-TOK@ {: off:n len:n :}
   SDT-SRC off + len ;

: SDT-TEST-COLON-SPAN ( -- )
   s" colon-span.f" SDT-COLON$ SDT-WRITE-ENTRY
   SDT-ENTRY$ SDT-SRC SDT-SRC-CAP READ-ALL SDT-SRC-U !
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 SDT-EVENT-TOK$ s" required" T$= ;

\ --- only the definition openers open a definition ---------------------------

\ A USER DEFINER'S NAME ENDS IN `:` AND IS STILL AN ORDINARY CALL. `64
\ SPAN-BUFFER: SB` (lib/span.f) names a buffer through a `create ... does>`
\ word, and the walk reads the whole line as calls: it consumes no name of its
\ own, so the require after it is the event it always was. Only `:`, `TRUSTED:`
\ and `undefine` take the token that follows them.
: SDT-TEST-USER-DEFINER ( -- )
   s" user-definer.f"
   S\" require sd-j.f\n64 SPAN-BUFFER: SB\n: EXAMPLE ( n -- ) {: required:n :} SB SPAN:LEN drop required drop ;\ns\" sd-k.f\" required\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-PATH@ s" sd-j.f" SDT-PATH T$=
   1 EVENT-PATH@ s" sd-k.f" SDT-PATH T$= ;

\ The string the file never closes is the one refusal the walk makes about text
\ rather than about a loader: the scan runs off the end and E-DISC-UNTERM is
\ what a caller sees. hb-build reads an application through this walk, so a
\ source with a bad literal is refused here before anything compiles it.
: SDT-TEST-UNTERM-STRING ( -- )
   s" unterm.f" S\" : L ( -- ) s\" sd-l.f\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-UNTERM TTHROWSQ ;

\ --- a parsing keyword's operand is data -------------------------------------

\ `'`, `[']`, `char` and `[char]` take the next whitespace-delimited token raw,
\ whatever it spells, as the loader does: `char s"` is 115 and opens no string,
\ `char \` is 92 and hides nothing after it, `' :` starts no definition and
\ `' require` names the loader word without loading anything. In a body a local
\ of the keyword's name is the local, so the `;` after the local `char` ends the
\ definition and its local `require`. Each source loads, and the walk's one
\ event is the require the loader runs.
: SDT-RAW-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" raw.f" a u SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-raw.f" SDT-PATH T$= ;

: SDT-TEST-PARSED-OPERAND ( -- )
   S\" char s\" constant SDT-SQ\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-Q ( -- n ) [char] s\" ;\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" char \\ drop require sd-raw.f\n" SDT-RAW-ONE
   S\" ' :\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" ' require constant SDT-RQ\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-RT ( -- ) ['] require drop ;\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-L ( n n -- n ) {: char require :} char ;\nrequire sd-raw.f\n" SDT-RAW-ONE ;

\ --- a comment keeps a literal path ------------------------------------------

\ The loader skips a comment as it skips blanks, so a literal path, a `( … )` or
\ a `\` comment and the loader word are the literal load they are without the
\ comment: the event the loader runs, at the loader word. A `c"` string across a
\ comment is the opener it is without one (E-DISC-OPENER). A token that is no
\ comment is code, whatever comments surround it, and the path is no longer
\ the literal (E-DISC-DYNAMIC). A refused walk leaves no event to read: reading
\ one past the log faults, so the event is read only when the walk left it.
: SDT-COMMENTED-ONE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" commented.f" a u SDT-WRITE-ENTRY
   SDT-ENTRY$ SDT-SRC SDT-SRC-CAP READ-ALL SDT-SRC-U !
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 1 T=
   EVENT-COUNT 1 <> if exit then
   0 EVENT-PATH@ s" sd-commented.f" SDT-PATH T$=
   0 SDT-EVENT-TOK$ s" required" T$= ;

: SDT-TEST-COMMENTED-LITERAL ( -- )
   S\" s\" sd-commented.f\" ( kept ) required\n" SDT-COMMENTED-ONE
   S\" s\" sd-commented.f\" \\ kept\nrequired\n" SDT-COMMENTED-ONE
   s" commented-opener.f" S\" c\" sd-commented.f\" ( kept ) required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-OPENER TTHROWSQ
   s" commented-code.f" S\" s\" sd-commented.f\" ( kept ) 2dup \\ kept\nrequired 2drop\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ ;

\ UNDEFINE-IF-DEFINED takes the same literal across a comment: a name no loader
\ word has retires nothing discovery reads, and a loader word's still refuses.
: SDT-TEST-COMMENTED-RETIRE ( -- )
   s" commented-retire.f" S\" s\" SDT-NOT-A-LOADER\" ( kept ) UNDEFINE-IF-DEFINED\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T=
   s" commented-retire-loader.f" S\" s\" required\" \\ kept\nUNDEFINE-IF-DEFINED\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-RETIRE TTHROWSQ ;

\ --- a loader form in a body is the run of a word no check runs ---------------

\ The walk reads the entry whole, refusing nothing, and records one event: the
\ load of PATH. A refused walk leaves no event to read.
: SDT-ONE-LOAD ( ptr u8 n -- ) {: pa:ptr pu:n :}
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 1 T=
   EVENT-COUNT 1 <> if exit then
   0 EVENT-PATH@ pa pu SDT-PATH T$= ;

\ SDT-ONE-LOAD of an entry holding SOURCE.
: SDT-ONE-EVENT ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n pa:ptr pu:n :}
   s" one-event.f" a u SDT-WRITE-ENTRY
   pa pu SDT-ONE-LOAD ;

\ A loader word in a body after anything but a path literal is nothing to
\ follow or refuse. The walk reads on past it: a later literal load in the body
\ is recorded, and `;` ends the body, so a top-level loader taking no literal
\ after it is refused at itself.
: SDT-TEST-BODY-DYNAMIC ( -- )
   s" body-dyn.f" S\" : L ( ptr u8 n -- ) included ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T=
   S\" : L ( ptr u8 n -- ) included ;\nrequire sd-top.f\n" s" sd-top.f" SDT-ONE-EVENT
   S\" : L ( ptr u8 n -- ) included s\" sd-body.f\" required ;\n" s" sd-body.f" SDT-ONE-EVENT
   s" body-dyn-top.f" S\" : L ( ptr u8 n -- ) included ;\nrequired\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ
   DISCOVER:LAST-TOKEN {: off:n len:n :}
   off 31 T=
   len 8 T= ;

: SDT-TEST-BODY-OPENER ( -- )
   s" body-opener.f" S\" : L ( -- ) C\\\" sd-i.f\" required ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T= ;

\ A definition of a loader word's name is no call a body makes: after a body,
\ as anywhere, it is refused at its name.
: SDT-TEST-BODY-SHADOW ( -- )
   s" body-shadow.f" S\" : HELP ( -- ) ;\n: included ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-SHADOW TTHROWSQ ;

\ UNDEFINE-IF-DEFINED in a body retires nothing while the file loads, whatever
\ name it takes: a loader word's or one computed when the word runs.
: SDT-TEST-RETIRE ( -- )
   s" retire.f" S\" : R ( -- ) s\" require\" UNDEFINE-IF-DEFINED ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T= ;

: SDT-TEST-RETIRE-DYNAMIC ( -- )
   s" retire-dyn.f" S\" : R ( ptr u8 n -- ) UNDEFINE-IF-DEFINED ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T= ;

\ --- oversized string literals: data tolerated, loader path rejected ---------

: SDT-X16$ ( -- ptr u8 n )
   s" xxxxxxxxxxxxxxxx" ;

: SDT-DOT16$ ( -- ptr u8 n )
   s" ././././././././" ;

\ The engine's resolver alone caps a loader's path: it refuses an absolute one
\ of 2050 bytes or more, a relative one that is 2050 bytes or more joined as
\ root, `/` and path to a root searched before one holds the file, and one over
\ PATH-CAP once normalized (src/core/include.f SOURCE-ROOT CHECK,
\ SEARCH-ROOTS, JOIN! and NORMALIZE). Paths of 1280
\ and of PATH-CAP x bytes resolve past PATH-CAP against any root. PATH-CAP
\ bytes of `./` before a file name resolve within it; SDT-OVER bytes of them
\ and an 18-byte name are 2050 bytes.
$500 constant SDT-BIG
$7F0 constant SDT-OVER

\ writes name = head + u bytes of the 16-byte piece, u a multiple of 16, + tail
: SDT-WRITE-BIG ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n head:ptr headu:n u:n piece:ptr pieceu:n tail:ptr tailu:n :}
   SDT-ROOT$ name nameu SDT-ENTRY JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ head headu WRITE-ALL
   u 16 / 0 ?do SDT-ENTRY$ piece pieceu APPEND-FILE loop
   SDT-ENTRY$ tail tailu APPEND-FILE ;

: SDT-TEST-BIG-STRING-DATA ( -- )
   s" big-ok.f" S\" s\" " SDT-BIG SDT-X16$ S\" \" 2drop\n" SDT-WRITE-BIG
   SDT-DISCOVER
   EVENT-COUNT 0 T= ;

: SDT-TEST-BIG-STRING-LOADER ( -- )
   s" big-bad.f" S\" s\" " SDT-BIG SDT-X16$ S\" \" required\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] E-DISC-CAPACITY TTHROWSQ
   s" big-resolved.f" S\" s\" " PATH-CAP SDT-X16$ S\" \" required\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] E-DISC-CAPACITY TTHROWSQ
   s" big-over.f" S\" s\" " SDT-OVER SDT-DOT16$ S\" sd-pad-over-caps.f\" required\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] E-DISC-CAPACITY TTHROWSQ ;

\ In a body the same loads are calls the word makes when it runs, which load
\ nothing: a path the resolver refuses is nothing to follow or refuse.
: SDT-TEST-BIG-STRING-BODY ( -- )
   s" big-body.f" S\" : L ( -- ) s\" " SDT-BIG SDT-X16$ S\" \" required ;\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T=
   s" big-body-resolved.f" S\" : L ( -- ) s\" " PATH-CAP SDT-X16$ S\" \" required ;\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T=
   s" big-body-over.f" S\" : L ( -- ) s\" " SDT-OVER SDT-DOT16$ S\" sd-pad-over-caps.f\" required ;\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   EVENT-COUNT 0 T= ;

\ A path over PATH-CAP as written that the resolver takes is a load the walk
\ follows, at top level and in a body.
: SDT-TEST-BIG-STRING-PAD ( -- )
   s" big-pad.f" S\" s\" " PATH-CAP SDT-DOT16$ S\" sd-pad.f\" required\n" SDT-WRITE-BIG
   s" sd-pad.f" SDT-ONE-LOAD
   s" big-body-pad.f" S\" : L ( -- ) s\" " PATH-CAP SDT-DOT16$ S\" sd-pad.f\" required ;\n" SDT-WRITE-BIG
   s" sd-pad.f" SDT-ONE-LOAD ;

\ Find loaders beyond the former 1 MiB ceiling, then grow the same scratch
\ again. The normal smaller-file fixtures after this must not see stale text.
: SDT-TEST-LARGE-SOURCE ( -- )
   s" large.f" s" " SDT-WRITE-ENTRY
   SDT-DISCOVER EVENT-COUNT 0 T=
   SDT-SRC-CAP 0 ?do 32 SDT-SRC i + c! loop
   $100 0 ?do SDT-ENTRY$ SDT-SRC SDT-SRC-CAP APPEND-FILE loop
   SDT-ENTRY$ S\" require sd-large-a.f\n" APPEND-FILE
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-large-a.f" SDT-PATH T$=
   0 EVENT-TOK@ 7 T= $100000 T=
   SDT-ENTRY$ FILE-SIZE {: end:n :}
   SDT-ENTRY$ S\" include sd-large-b.f\n" APPEND-FILE
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   1 EVENT-KIND@ EV-INCLUDED T=
   1 EVENT-PATH@ s" sd-large-b.f" SDT-PATH T$=
   1 EVENT-TOK@ 7 T= end T= ;

\ --- tree files: a body's retirement, and the loader's own definitions --------

\ driver-io.f retires loader names in a body (DRV-RETIRE-RELOADS), the run of a
\ word no check runs: the walk reads past them and records the file's one
\ require.
: SDT-TEST-TREE-BODY-RETIRE ( -- )
   s" src/habu/driver-io.f" DISCOVER:RUN
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" src/habu/sign-id.f" CANONICAL drop T$= ;

\ The loader's own definition site. The manifest tolerates the reserved names it
\ defines, and it loads no source itself, so the walk that keys the engine's
\ prefix (test/whitebox-engine.f) crosses it without losing a file.
: SDT-TEST-MANIFEST-INCLUDE ( -- )
   s" src/core/include.f" DISCOVER:RUN
   EVENT-COUNT 0 T= ;

: SDT-RUN-EMIT-SMALL ( -- )
   SDT-DISCOVER
   SDT-OUT 4 DISCOVER:EMIT drop ;

: SDT-TEST-EMIT-CAP ( -- )
   s" emitcap.f" S\" require sd-z.f\n" SDT-WRITE-ENTRY
   [: SDT-RUN-EMIT-SMALL ;] E-FS-CAPACITY TTHROWSQ ;

: SDT-TEST-LOCALS ( -- )
   s" locals.f"
   S\" : EXAMPLE ( n n n n n -- n n n n n ) {: include:n included:n require:n required:n provided:n :} include included require required provided ;\nrequire sd-after.f\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-after.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-SCOPES ( -- )
   s" local-scopes.f"
   S\" : EXAMPLE ( n -- ) dup if {: required:n :} required drop else drop s\" sd-else.f\" required then 1 0 ?do 1 {: required:n :} required drop loop s\" sd-loop.f\" required 1 case 1 of 2 {: required:n :} required drop endof endcase s\" sd-case.f\" required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 3 T=
   0 EVENT-PATH@ s" sd-else.f" SDT-PATH T$=
   1 EVENT-PATH@ s" sd-loop.f" SDT-PATH T$=
   2 EVENT-PATH@ s" sd-case.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-QUOTATION ( -- )
   s" local-quotation.f"
   S\" : EXAMPLE ( n -- n ) {: required:n :} [: s\" sd-quote.f\" required ;] execute required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-quote.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-CASE ( -- )
   s" local-case.f"
   S\" : EXAMPLE ( n -- n ) {: required:n :} s\" sd-uppercase.f\" REQUIRED required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-uppercase.f" SDT-PATH T$= ;


\ A local of a loader word's name is the local from its group's closer to the
\ end of its block: before and after that, the name is the loader word.
: SDT-TEST-LOCAL-LIFETIME ( -- )
   S\" : EXAMPLE ( n -- n ) s\" sd-before.f\" required {: required:n :} s\" sd-in.f\" required ;\n"
   s" sd-before.f" SDT-ONE-EVENT
   S\" : EXAMPLE ( n -- ) if 1 {: required:n :} s\" sd-in.f\" required drop then s\" sd-after.f\" required ;\n"
   s" sd-after.f" SDT-ONE-EVENT ;


: SDT-TEST-LOCAL-CONTROL ( -- )
   s" local-control.f"
   S\" : EXAMPLE ( n -- n ) {: then:n :} 1 if 1 {: required:n :} required drop then drop ELSE s\" sd-control.f\" required THEN then ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" sd-control.f" SDT-PATH T$= ;


: SDT-MAIN ( -- )
   T-RESET
   SDT-PREP
   SDT-TEST-LARGE-SOURCE
   SDT-TEST-MIXED
   SDT-TEST-SPELLING
   SDT-TEST-FRESH
   SDT-TEST-WIDE
   SDT-TEST-EMIT
   SDT-TEST-SHADOW
   SDT-TEST-UNDEFINE
   SDT-TEST-DYNAMIC
   SDT-TEST-OPENER
   SDT-TEST-COLON-BODY
   SDT-TEST-COLON-SPAN
   SDT-TEST-USER-DEFINER
   SDT-TEST-UNTERM-STRING
   SDT-TEST-PARSED-OPERAND
   SDT-TEST-COMMENTED-LITERAL
   SDT-TEST-COMMENTED-RETIRE
   SDT-TEST-BODY-DYNAMIC
   SDT-TEST-BODY-OPENER
   SDT-TEST-BODY-SHADOW
   SDT-TEST-RETIRE
   SDT-TEST-RETIRE-DYNAMIC
   SDT-TEST-BIG-STRING-DATA
   SDT-TEST-BIG-STRING-LOADER
   SDT-TEST-BIG-STRING-BODY
   SDT-TEST-BIG-STRING-PAD
   SDT-TEST-TREE-BODY-RETIRE
   SDT-TEST-MANIFEST-INCLUDE
   SDT-TEST-EMIT-CAP
   SDT-TEST-LOCALS
   SDT-TEST-LOCAL-SCOPES
   SDT-TEST-LOCAL-QUOTATION
   SDT-TEST-LOCAL-CASE
   SDT-TEST-LOCAL-LIFETIME
   SDT-TEST-LOCAL-CONTROL
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   CLEANUP-RUN
   T-REPORT
   s" source-discovery-test: ok" type cr ;

SDT-MAIN

;using
;package
