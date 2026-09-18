\ source-discovery-test.f - checked fixtures for the whole-file discovery pass.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/source.f tools/source-discovery.f tools/source-discovery-test.f
\
\ Proves the ordered event artifact for include/require/provided mixes (include
\ replay-every-occurrence vs require dedup), canonical registry (equivalent spelling
\ collapse), tool-preloaded require paths not hiding a later user require,
\ colon-body loader capture with byte-exact token spans, the shared checked
\ path emitter, fail-closed rejection when the artifact cannot be produced
\ (loader word shadowed/undefined/retired, dynamic loader path, unsupported
\ opener, serialization overflow), and the dynamic-tail manifest boundary
\ (manifested repo files tolerated, the same shapes elsewhere rejected).

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
   s" habu-source-discovery-test" TMPDIR-MKDIR SDT-ROOT SDT-ROOT-U SDT-COPY!
   SDT-ROOT$ CLEANUP-TREE+ ;

: SDT-WRITE-ENTRY ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n content:ptr contentu:n :}
   SDT-ROOT$ name nameu SDT-ENTRY JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ content contentu WRITE-ALL ;

: SDT-PATH ( ptr u8 n -- ptr u8 n )
   SDT-ROOT$ 2swap JOIN CANONICAL drop ;

: SDT-DISCOVER ( -- )  SDT-ENTRY$ DISCOVER:RUN ;

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

: SDT-TEST-EMIT ( -- )
   s" emit.f" S\" require sd-x.f\ns\" sd-y.f\" provided\n" SDT-WRITE-ENTRY
   SDT-DISCOVER
   SDT-OUT $2000 DISCOVER:EMIT {: elen:n :}
   SB-RESET
   S\" required 0 s\" " SB-APPEND
   s" sd-x.f" SDT-PATH SB-APPEND
   S\" \"\nprovided 0 s\" " SB-APPEND
   s" sd-y.f" SDT-PATH SB-APPEND
   S\" \"\n" SB-APPEND
   SDT-OUT elen SB$ T$= ;

: SDT-RUN-ENTRY ( -- )   SDT-DISCOVER ;

: SDT-TEST-SHADOW ( -- )
   s" shadow.f" S\" : required ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
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

\ --- fail-closed: dynamic/opener/retire forms inside colon bodies ------------

: SDT-TEST-BODY-DYNAMIC ( -- )
   s" body-dyn.f" S\" : L ( ptr u8 n -- ) included ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ ;

: SDT-TEST-BODY-OPENER ( -- )
   s" body-opener.f" S\" : L ( -- ) C\\\" sd-i.f\" required ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-OPENER TTHROWSQ ;

: SDT-TEST-BODY-SHADOW ( -- )
   s" body-shadow.f" S\" : HELP ( -- ) ;\n: included ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-SHADOW TTHROWSQ ;

: SDT-TEST-RETIRE ( -- )
   s" retire.f" S\" : R ( -- ) s\" require\" UNDEFINE-IF-DEFINED ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-RETIRE TTHROWSQ ;

: SDT-TEST-RETIRE-DYNAMIC ( -- )
   s" retire-dyn.f" S\" : R ( ptr u8 n -- ) UNDEFINE-IF-DEFINED ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-RETIRE TTHROWSQ ;

: SDT-TEST-RETIRE-OTHER ( -- )
   s" retire-ok.f" S\" : R ( -- ) s\" SDT-NOT-A-LOADER\" UNDEFINE-IF-DEFINED ;\n" SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 0 T= ;

\ --- oversized string literals: data tolerated, loader path rejected ---------

: SDT-X16$ ( -- ptr u8 n )
   s" xxxxxxxxxxxxxxxx" ;

\ writes name = `s" <1280 x bytes>` + tail (tail supplies the closing quote)
: SDT-WRITE-BIG ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n tail:ptr tailu:n :}
   SDT-ROOT$ name nameu SDT-ENTRY JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ S\" s\" " WRITE-ALL
   $50 0 ?do SDT-ENTRY$ SDT-X16$ APPEND-FILE loop
   SDT-ENTRY$ tail tailu APPEND-FILE ;

: SDT-TEST-BIG-STRING-DATA ( -- )
   s" big-ok.f" S\" \" 2drop\n" SDT-WRITE-BIG
   SDT-DISCOVER
   EVENT-COUNT 0 T= ;

: SDT-TEST-BIG-STRING-LOADER ( -- )
   s" big-bad.f" S\" \" required\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] E-DISC-CAPACITY TTHROWSQ ;

\ --- dynamic-tail manifest: seeded repo files tolerated, path-keyed ----------

: SDT-TEST-MANIFEST-DRIVER ( -- )
   s" src/habu/driver-io.f" DISCOVER:RUN
   EVENT-COUNT 0 T= ;

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


: SDT-TEST-LOCAL-LIFETIME ( -- )
   s" before-local.f"
   S\" : EXAMPLE ( n -- n ) required {: required:n :} required ;\n"
   SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ
   s" after-local.f"
   S\" : EXAMPLE ( n -- ) if 1 {: required:n :} required drop then required ;\n"
   SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ ;


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
   SDT-TEST-MIXED
   SDT-TEST-SPELLING
   SDT-TEST-FRESH
   SDT-TEST-EMIT
   SDT-TEST-SHADOW
   SDT-TEST-UNDEFINE
   SDT-TEST-DYNAMIC
   SDT-TEST-OPENER
   SDT-TEST-COLON-BODY
   SDT-TEST-COLON-SPAN
   SDT-TEST-USER-DEFINER
   SDT-TEST-UNTERM-STRING
   SDT-TEST-BODY-DYNAMIC
   SDT-TEST-BODY-OPENER
   SDT-TEST-BODY-SHADOW
   SDT-TEST-RETIRE
   SDT-TEST-RETIRE-DYNAMIC
   SDT-TEST-RETIRE-OTHER
   SDT-TEST-BIG-STRING-DATA
   SDT-TEST-BIG-STRING-LOADER
   SDT-TEST-MANIFEST-DRIVER
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
