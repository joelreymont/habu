\ source-discovery-test.f - checked fixtures for the restricted discovery pass.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/source.f tools/source-discovery.f tools/source-discovery-test.f
\
\ Proves the ordered event artifact for include/require/provided mixes (include
\ replay-every-occurrence vs require dedup), exact-string registry (no spelling
\ collapse), tool-preloaded require paths not hiding a later user require, the
\ shared checked path emitter, and fail-closed rejection when the artifact
\ cannot be produced (loader word shadowed/undefined, dynamic loader path,
\ unsupported opener, serialization overflow).

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

create SDT-ROOT SDT-PC allot
create SDT-ENTRY SDT-PC allot
create SDT-OUT $2000 allot
variable SDT-ROOT-U
variable SDT-ENTRY-U

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

: SDT-RUN-SHADOW ( -- )   SDT-DISCOVER ;

: SDT-TEST-SHADOW ( -- )
   s" shadow.f" S\" : required ( ptr u8 n -- ) 2drop ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-SHADOW ;] E-DISC-SHADOW TTHROWSQ ;

: SDT-TEST-UNDEFINE ( -- )
   s" undef.f" S\" undefine required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-SHADOW ;] E-DISC-SHADOW TTHROWSQ ;

: SDT-TEST-DYNAMIC ( -- )
   s" dyn.f" S\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-SHADOW ;] E-DISC-DYNAMIC TTHROWSQ ;

: SDT-TEST-OPENER ( -- )
   s" opener.f" S\" C\\\" sd-g.f\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-SHADOW ;] E-DISC-OPENER TTHROWSQ ;

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
   SDT-TEST-EMIT-CAP
   EVENT-OFF DISCOVERY-OFF EVENTS-RESET
   CLEANUP-RUN
   T-REPORT
   s" source-discovery-test: ok" type cr ;

SDT-MAIN

end-package
