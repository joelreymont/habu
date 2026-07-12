\ source-discovery-test.f - checked fixtures for the whole-file discovery pass.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/source.f tools/source-discovery.f tools/source-discovery-test.f
\
\ Proves the ordered event artifact for include/require/provided mixes (include
\ replay-every-occurrence vs require dedup), exact-string registry (no spelling
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
   1 EVENT-PATH@ s" sd-b.f" T$=
   3 EVENT-KIND@ EV-REQUIRED T=
   3 EVENT-STATE@ EV-STATE-FRESH T=
   4 EVENT-STATE@ EV-STATE-KNOWN T=
   5 EVENT-KIND@ EV-PROVIDED T=
   6 EVENT-KIND@ EV-REQUIRED T=
   6 EVENT-PATH@ s" sd-e.f" T$= ;

: SDT-SPELLING$ ( -- ptr u8 n )
   S\" s\" ./sd-f.f\" required\ns\" sd-f.f\" required\n" ;

: SDT-TEST-SPELLING ( -- )
   s" spelling.f" SDT-SPELLING$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   1 EVENT-STATE@ EV-STATE-FRESH T=
   0 EVENT-PATH@ s" ./sd-f.f" T$=
   1 EVENT-PATH@ s" sd-f.f" T$= ;

: SDT-FRESH$ ( -- ptr u8 n )
   S\" s\" sd-tool.f\" required\ns\" sd-user.f\" required\n" ;

: SDT-TEST-FRESH ( -- )
   REQUIRE-N @ {: save-n:n :}
   s" sd-tool.f" provided
   s" fresh.f" SDT-FRESH$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   EVENT-COUNT 2 T=
   0 EVENT-KIND@ EV-REQUIRED T=
   0 EVENT-STATE@ EV-STATE-FRESH T=
   0 EVENT-PATH@ s" sd-tool.f" T$=
   1 EVENT-STATE@ EV-STATE-FRESH T=
   save-n REQUIRE-N ! ;

: SDT-TEST-EMIT ( -- )
   s" emit.f" S\" require sd-x.f\ns\" sd-y.f\" provided\n" SDT-WRITE-ENTRY
   SDT-DISCOVER
   SDT-OUT $2000 DISCOVER:EMIT {: elen:n :}
   SDT-OUT elen S\" required 0 s\" sd-x.f\"\nprovided 0 s\" sd-y.f\"\n" T$= ;

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
   0 EVENT-PATH@ s" sd-h.f" T$= ;

: SDT-EVENT-TOK$ ( n -- ptr u8 n ) {: ix:n :}
   ix EVENT-TOK@ {: off:n len:n :}
   SDT-SRC off + len ;

: SDT-TEST-COLON-SPAN ( -- )
   s" colon-span.f" SDT-COLON$ SDT-WRITE-ENTRY
   SDT-ENTRY$ SDT-SRC SDT-SRC-CAP READ-ALL SDT-SRC-U !
   SDT-DISCOVER
   EVENT-COUNT 1 T=
   0 SDT-EVENT-TOK$ s" required" T$= ;

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

: SDT-TEST-MANIFEST-WORKER ( -- )
   s" test/run-worker.f" DISCOVER:RUN
   EVENT-COUNT 0 T= ;

: SDT-TEST-MANIFEST-DRIVER ( -- )
   s" src/habu/driver-io.f" DISCOVER:RUN
   EVENT-COUNT 0 T= ;

: SDT-RUN-EMIT-SMALL ( -- )
   SDT-DISCOVER
   SDT-OUT 4 DISCOVER:EMIT drop ;

: SDT-TEST-EMIT-CAP ( -- )
   s" emitcap.f" S\" require sd-z.f\n" SDT-WRITE-ENTRY
   [: SDT-RUN-EMIT-SMALL ;] E-FS-CAPACITY TTHROWSQ ;

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
   SDT-TEST-BODY-DYNAMIC
   SDT-TEST-BODY-OPENER
   SDT-TEST-BODY-SHADOW
   SDT-TEST-RETIRE
   SDT-TEST-RETIRE-DYNAMIC
   SDT-TEST-RETIRE-OTHER
   SDT-TEST-BIG-STRING-DATA
   SDT-TEST-BIG-STRING-LOADER
   SDT-TEST-MANIFEST-WORKER
   SDT-TEST-MANIFEST-DRIVER
   SDT-TEST-EMIT-CAP
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   CLEANUP-RUN
   T-REPORT
   s" source-discovery-test: ok" type cr ;

SDT-MAIN

;package
