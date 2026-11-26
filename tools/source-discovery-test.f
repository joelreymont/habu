\ source-discovery-test.f - fixtures for the whole-file discovery pass
\ (tools/source-discovery.f): its ordered load-event artifact and its rejections.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/memory.f src/lib/fs.f
\ src/lib/fs-mutate.f src/lib/source.f tools/source-discovery.f tools/source-discovery-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/source.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require tools/source-discovery.f
require tools/gate-json-assert-core.f
require src/habu/verify-source.f
require src/lib/process.f

package SD-TEST

PATH-CAP constant SDT-PC
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

: SDT-ROOT$ ( -- str )   SDT-ROOT SDT-ROOT-U @ >str ;
public
: SDT-ENTRY$ ( -- str )  SDT-ENTRY SDT-ENTRY-U @ >str ;
private

: SDT-COPY! ( str ^u8 ^n -- ) {: s:str dst:^ lenp:^ :}
   s STR:DATA dst s STR:LEN BYTE-COPY
   s STR:LEN lenp ! ;

: SDT-PREP ( -- )
   FS:CLEANUP-RESET
   s" habu-source-discovery-test" FS:MKTEMP-DIR SDT-ROOT SDT-ROOT-U SDT-COPY!
   SDT-ROOT$ FS:CLEANUP-TREE+ ;

: SDT-WRITE-ENTRY ( str str -- )
   {: name:str content:str :}
   SDT-ROOT$ name SDT-ENTRY FS:JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ content FS:WRITE-ALL ;

: SDT-PATH ( str -- str )
   SDT-ROOT$ swap SOURCE-ROOT:JOIN SOURCE-ROOT:CANONICAL drop ;

: SDT-READ ( -- )  SDT-ENTRY$ SOURCE-ROOT:ENTRY-RESOLVE drop SOURCE-ROOT:RESOLVED-ROOT$ DISCOVER:READ-IN ;

: SDT-DISCOVER ( -- )  SDT-READ DISCOVER:RUN-READ ;

: SDT-WANT+ ( str -- )  SDT-WANT SDT-WANT-CAP SDT-WANT-U BUF-APPEND ;

: SDT-MIXED$ ( -- str )
   S\" require sd-a.f\ninclude sd-b.f\ninclude sd-b.f\ns\" sd-c.f\" required\ns\" sd-c.f\" required\ns\" sd-d.f\" provided\n: HELPER ( n -- n ) dup + ;\nrequire sd-e.f\n" ;

: SDT-TEST-MIXED ( -- )
   s" mixed.f" SDT-MIXED$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 7 T=
   0 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   0 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   1 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-INCLUDED T=
   2 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-INCLUDED T=
   1 SOURCE-EVENT:PATH@ s" sd-b.f" SDT-PATH T$=
   3 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   3 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   4 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-KNOWN T=
   5 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-PROVIDED T=
   6 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   6 SOURCE-EVENT:PATH@ s" sd-e.f" SDT-PATH T$= ;

: SDT-SPELLING$ ( -- str )
   S\" s\" ./sd-f.f\" required\ns\" sd-f.f\" required\n" ;

: SDT-TEST-SPELLING ( -- )
   s" spelling.f" SDT-SPELLING$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 2 T=
   0 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   1 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-KNOWN T=
   0 SOURCE-EVENT:PATH@ s" sd-f.f" SDT-PATH T$=
   1 SOURCE-EVENT:PATH@ s" sd-f.f" SDT-PATH T$= ;

: SDT-FRESH$ ( -- str )
   S\" s\" sd-tool.f\" required\ns\" sd-user.f\" required\n" ;

: SDT-TEST-FRESH ( -- )
   REQUIRE-REG:COUNT {: save-n:n :}
   s" sd-tool.f" SDT-PATH provided
   s" fresh.f" SDT-FRESH$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 2 T=
   0 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   0 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   0 SOURCE-EVENT:PATH@ s" sd-tool.f" SDT-PATH T$=
   1 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   save-n REQUIRE-REG:TRUNCATE ;

\ More than 256 direct dependencies: one event per require, in order. Line k
\ requires sd-wXY.f, XY spelling k in two letters (line 256 requires sd-wjw.f).
: SDT-WIDE-LINE$ ( -- str )  S\" require sd-waa.f\n" ;
17 constant SDT-WIDE-LINE                \ the bytes of SDT-WIDE-LINE$
257 constant SDT-WIDE-N
create SDT-WIDE SDT-WIDE-N SDT-WIDE-LINE * allot

: SDT-WIDE-LINE! ( n -- )
   {: k:n :}
   k SDT-WIDE-LINE * SDT-WIDE + {: at:^ :}
   SDT-WIDE-LINE$ {: l:str :}
   l STR:DATA at l STR:LEN BYTE-COPY
   k 26 / $61 + at 12 + c!
   k 26 mod $61 + at 13 + c! ;

: SDT-TEST-WIDE ( -- )
   REQUIRE-REG:COUNT {: save-n:n :}
   SDT-WIDE-N 0 ?do i SDT-WIDE-LINE! loop
   s" wide.f" SDT-WIDE SDT-WIDE-N SDT-WIDE-LINE * >str
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT SDT-WIDE-N T=
   256 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   256 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   256 SOURCE-EVENT:PATH@ s" sd-wjw.f" SDT-PATH T$=
   0 SOURCE-EVENT:PATH@ s" sd-waa.f" SDT-PATH T$=
   save-n REQUIRE-REG:TRUNCATE ;

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
   SDT-OUT elen >str SDT-WANT SDT-WANT-U @ >str T$= ;

\ A discovery a text runs reports the file's require as a file's.
: SDT-TEST-TEXT ( -- )
   s" text.f" S\" require sd-x.f\n" SDT-WRITE-ENTRY
   SDT-READ  s" DISCOVER:RUN-READ" evaluate-closed
   SDT-OUT $2000 DISCOVER:EMIT {: elen:n :}
   SDT-WANT-U BUF-RESET
   S\" required 0 s\" " SDT-WANT+
   s" sd-x.f" SDT-PATH SDT-WANT+
   S\" \"\n" SDT-WANT+
   SDT-OUT elen >str SDT-WANT SDT-WANT-U @ >str T$= ;

: SDT-RUN-ENTRY ( -- )   SDT-DISCOVER ;

\ Q's walk is rejected as KIND: it stops with 70 and
\ VERIFY:DISCOVERY-STOPPED holds the kind.
: SDT-STOPS ( [ -- ] n -- ) {: q kind:n :}
   q 70 TTHROWSQ
   VERIFY:DISCOVERY-STOPPED @ kind T= ;

\ The entry walked with its bytes armed as the rejection source, as the checker
\ arms the file it walks: a record is placed in them.
: SDT-ARMED-RUN ( -- )
   SDT-READ
   DISCOVER:BYTES$ 1 1 0 REJECTION:SOURCE!
   DISCOVER:RUN-READ ;

\ The entry is rejected as KIND, its record placed at the LEN bytes from byte OFF.
: SDT-STOPS-AT ( n n n -- ) {: kind:n off:n len:n :}
   REJECTION:JSON @ {: json:n :}
   REJECTION:CAPTURE  true REJECTION:JSON !
   [: SDT-ARMED-RUN ;] kind SDT-STOPS
   json REJECTION:JSON !  REJECTION:SOURCE-OFF
   REJECTION:BUFFER$ GJA-PARSE {: root:SPAN:span<u8> :}
   root s" byte_start" off GJA-ASSERT-INT-FIELD
   root s" byte_end" off len + GJA-ASSERT-INT-FIELD
   REJECTION:BUFFER-OFF ;

: SDT-TEST-DYNAMIC ( -- )
   s" dyn.f" S\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC SDT-STOPS ;

: SDT-TEST-OPENER ( -- )
   s" opener.f" S\" C\\\" sd-g.f\" required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-OPENER SDT-STOPS ;

\ --- whole-file scan: colon-body loaders are events --------------------------

: SDT-COLON$ ( -- str )
   S\" : MAYBE ( -- ) s\" sd-h.f\" required ;\nMAYBE\n" ;

: SDT-TEST-COLON-BODY ( -- )
   s" colon.f" SDT-COLON$ SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   0 SOURCE-EVENT:STATE@ SOURCE-EVENT:STATE-FRESH T=
   0 SOURCE-EVENT:PATH@ s" sd-h.f" SDT-PATH T$= ;

\ --- only the definition openers open a definition ---------------------------

\ A user definer's name ending in `:` is an ordinary call: `64 SPAN-BUFFER: SB`
\ (src/lib/span.f) consumes no name, so the require after it is still an event.
\ Only `:`, `kernel:`, `TRUSTED:` and, outside a definition, the engine's
\ definers, the package keywords and `undefine` take the next token as a name;
\ a library storage definer is a call here too.
: SDT-TEST-USER-DEFINER ( -- )
   s" user-definer.f"
   S\" require sd-j.f\n64 SPAN-BUFFER: SB\n: EXAMPLE ( n -- ) {: required:n :} SB SPAN:LEN drop required drop ;\ns\" sd-k.f\" required\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 2 T=
   0 SOURCE-EVENT:PATH@ s" sd-j.f" SDT-PATH T$=
   1 SOURCE-EVENT:PATH@ s" sd-k.f" SDT-PATH T$= ;

\ An unclosed string is the walk's one rejection about text rather than a loader
\ (E-DISC-UNTERM). hb-build reads an application through this walk, so the
\ source is rejected before anything compiles it.
: SDT-TEST-UNTERM-STRING ( -- )
   s" unterm.f" S\" : L ( -- ) s\" sd-l.f\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-UNTERM SDT-STOPS ;

\ --- a parsing keyword's operand is data -------------------------------------

\ At top level `char` and `'` take the next token raw, in a body `[char]` and
\ `[']`, as the loader does; each source's one event is the require it runs.
: SDT-RAW-ONE ( str -- ) {: s:str :}
   s" raw.f" s SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-raw.f" SDT-PATH T$= ;

: SDT-TEST-PARSED-OPERAND ( -- )
   S\" char s\" constant SDT-SQ\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-Q ( -- n ) [char] s\" ;\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" char \\ drop require sd-raw.f\n" SDT-RAW-ONE
   S\" ' :\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" ' require constant SDT-RQ\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-RT ( -- ) ['] require drop ;\nrequire sd-raw.f\n" SDT-RAW-ONE
   S\" : SDT-L ( n n -- n ) {: char require :} char ;\nrequire sd-raw.f\n" SDT-RAW-ONE ;

\ --- a comment keeps a literal path ------------------------------------------

\ The loader skips a comment as it skips blanks: a literal path, a `( … )` or `\`
\ comment and the loader word are the literal load.
\ A `c"` across a comment is still an opener (E-DISC-OPENER); a non-comment token
\ between makes the path dynamic (E-DISC-DYNAMIC). A rejected walk leaves no
\ event: reading one past the log faults, so read one only when the walk left it.
: SDT-COMMENTED-ONE ( str -- )
   {: s:str :}
   s" commented.f" s SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T=
   SOURCE-EVENT:COUNT 1 <> if exit then
   0 SOURCE-EVENT:PATH@ s" sd-commented.f" SDT-PATH T$= ;

: SDT-TEST-COMMENTED-LITERAL ( -- )
   S\" s\" sd-commented.f\" ( kept ) required\n" SDT-COMMENTED-ONE
   S\" s\" sd-commented.f\" \\ kept\nrequired\n" SDT-COMMENTED-ONE
   s" commented-opener.f" S\" c\" sd-commented.f\" ( kept ) required\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-OPENER SDT-STOPS
   s" commented-code.f" S\" s\" sd-commented.f\" ( kept ) 2dup \\ kept\nrequired 2drop\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] E-DISC-DYNAMIC SDT-STOPS ;

\ --- a loader form in a body is the run of a word no check runs ---------------

\ The walk rejects nothing and records one event, the load of PATH.
: SDT-ONE-LOAD ( str -- ) {: ps:str :}
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T=
   SOURCE-EVENT:COUNT 1 <> if exit then
   0 SOURCE-EVENT:PATH@ ps SDT-PATH T$= ;

\ SDT-ONE-LOAD of an entry holding SOURCE.
: SDT-ONE-EVENT ( str str -- ) {: s:str ps:str :}
   s" one-event.f" s SDT-WRITE-ENTRY
   ps SDT-ONE-LOAD ;

\ A loader word in a body after anything but a path literal is read past: a
\ later literal load in the body is recorded, and a top-level loader taking no
\ literal after `;` is rejected at itself.
: SDT-TEST-BODY-DYNAMIC ( -- )
   s" body-dyn.f" S\" : L ( str -- ) str> included ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 0 T=
   S\" : L ( str -- ) str> included ;\nrequire sd-top.f\n" s" sd-top.f" SDT-ONE-EVENT
   S\" : L ( str -- ) str> included s\" sd-body.f\" required ;\n" s" sd-body.f" SDT-ONE-EVENT
   s" body-dyn-top.f" S\" : L ( str -- ) str> included ;\nrequired\n" SDT-WRITE-ENTRY
   E-DISC-DYNAMIC 31 8 SDT-STOPS-AT ;

: SDT-TEST-BODY-OPENER ( -- )
   s" body-opener.f" S\" : L ( -- ) C\\\" sd-i.f\" required ;\n" SDT-WRITE-ENTRY
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 0 T= ;

\ --- a name is data, a loader word's too --------------------------------------

\ A definition, export, package, using or `undefine` naming a loader word
\ rejects nothing: each source loads, and the loader form after it is the one
\ event.
: SDT-TEST-LOADER-NAME ( -- )
   S\" package SDT-SH\n: include ( -- ) ;\nkernel: require ( -- ) ;\n;package\npackage SDT-SH2\nTRUSTED: include ( -- ) ;\n;package\ns\" sd-raw.f\" required\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" undefine required\nrequire sd-raw.f\n" s" sd-raw.f" SDT-ONE-EVENT
   S\" package REQUIRED\npublic\n: CVT-RQ ( -- n ) 1 ;\n;package\nusing REQUIRED\n;using\npackage CVT-EXP\npublic\nEXPORT required\n;package\ns\" sd-raw.f\" required\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" : U ( str -- ) undefine required ;\ns\" sd-raw.f\" required\n"
   s" sd-raw.f" SDT-ONE-EVENT ;

\ SOURCE, as an entry, is rejected with CODE at its loader word at OFF, LEN bytes
\ long.
: SDT-REPLACED-AT ( str n n n -- ) {: s:str code:n off:n len:n :}
   s" replaced.f" s SDT-WRITE-ENTRY
   code off len SDT-STOPS-AT ;

\ After the file defines or retires a loader word's name, a later loader form
\ the walk reads with it is rejected at the word; a body's other uses are calls
\ it reads past.
: SDT-TEST-LOADER-USE ( -- )
   S\" undefine require\n: require ( -- ) parse-name required ;\nrequire sd-raw.f\n"
   E-DISC-SHADOW 56 7 SDT-REPLACED-AT
   S\" package SDT-SH\nkernel: include ( -- ) ;\n;package\ninclude sd-raw.f\n"
   E-DISC-SHADOW 49 7 SDT-REPLACED-AT
   S\" package SDT-SH\n: required ( str -- ) drop ;\n;package\n: L ( -- ) s\" sd-raw.f\" required ;\n"
   E-DISC-SHADOW 77 8 SDT-REPLACED-AT
   S\" package SDT-SH\n: include ( -- ) ;\n;package\n: L ( -- ) include sd-raw.f ;\n"
   E-DISC-SHADOW 54 7 SDT-REPLACED-AT
   S\" package CVT-RES\nprivate\nvariable REQUIRED\n;package\ns\" sd-raw.f\" required\n"
   E-DISC-SHADOW 64 8 SDT-REPLACED-AT
   S\" undefine required\ns\" sd-raw.f\" required\n" E-DISC-RETIRE 31 8 SDT-REPLACED-AT
   S\" HB-TARGET-WASM? [if]\n: required ( str -- ) drop ;\n[then]\ns\" sd-raw.f\" required\n"
   E-DISC-SHADOW 70 8 SDT-REPLACED-AT
   S\" package SDT-SH\nprivate\nvariable REQUIRED\n: L ( -- n ) REQUIRED @ ;\n;package\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT ;

\ An engine definer or a package keyword takes the next token as a name, a
\ loader word's spelling too, so the require after it is the one event.
: SDT-TEST-DEFINER-OPERAND ( -- )
   S\" package REQUIRED\npublic\n: CVT-RQ ( -- n ) 1 ;\n;package\nusing REQUIRED\n;using\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" package CVT-RES\nprivate\nvariable REQUIRED\ncreate INCLUDED\n1 constant INCLUDE\ndefer PROVIDED ( -- )\n;package\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" package CVT-DEF\npublic\nDEFLINEAR CVT-DEF:tok\nEXPORT required\nprivate\nCAST: include ( n -- ^u8 )\nLINEAR: provided ( ^n -- CVT-DEF:tok )\n;package\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT ;

\ Family declarations parse their bodies as data, including member names that
\ spell loaders: a MUTUAL's list names and member headers too. Only the require
\ after the declarations is a dependency.
: SDT-TEST-FAMILY-BODIES ( -- )
   s" family-bodies.f"
   S\" STRUCTURE box FIELD include n FIELD required n ;STRUCTURE\nENUM choice VARIANT require FIELD included n ;VARIANT ;ENUM\nSUMTYPE outcome VARIANT provided n ;VARIANT ;SUMTYPE\nENUM colors include require ;ENUM\nMUTUAL include required STRUCTURE include FIELD require ^required ;STRUCTURE STRUCTURE required FIELD provided ^include ;STRUCTURE ;MUTUAL\ninclude sd-a.f\nrequire sd-raw.f\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 2 T=
   0 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-INCLUDED T=
   0 SOURCE-EVENT:PATH@ s" sd-a.f" SDT-PATH T$=
   1 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   1 SOURCE-EVENT:PATH@ s" sd-raw.f" SDT-PATH T$=
   \ A closer in a body comment ends nothing, so the `(` after it opens no comment.
   S\" STRUCTURE cbox FIELD a n \\ ends at ;STRUCTURE (\n;STRUCTURE\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   \ A comment before a member name or a MUTUAL's first name is skipped.
   S\" STRUCTURE fbox FIELD \\ the ( first\nx n ;STRUCTURE\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" MUTUAL \\ the ( pair\nma STRUCTURE ma FIELD x n ;STRUCTURE ;MUTUAL\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT ;

\ A package word may replace a family opener. The file really loads dep.f;
\ discovery must include it or reject at the ambiguous opener.
$100 constant SDT-LOAD-CAP
create SDT-LOAD-OUT SDT-LOAD-CAP allot
create SDT-LOAD-ERR SDT-LOAD-CAP allot

: SDT-TEST-FAMILY-SHADOW ( -- )
   s" dep.f" s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" SDT-WRITE-ENTRY
   s" family-shadow.f"
   S\" package DEMO : structure ( -- ) ; structure require dep.f ;package\n"
   SDT-WRITE-ENTRY
   s" SD-TEST:SDT-ENTRY$ included"
   SDT-LOAD-OUT SDT-LOAD-CAP >LEN SDT-LOAD-ERR SDT-LOAD-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN {: outu:len erru:len oc :}
   s" SD-TEST:SDT-ENTRY$ included"
   SDT-LOAD-OUT outu LEN>N >str SDT-LOAD-ERR erru LEN>N >str
   oc 0 T-ENDING-EXITED=
   SDT-LOAD-OUT outu LEN>N >str s" LOADED" T$=
   E-DISC-SHADOW 34 9 SDT-STOPS-AT
   s" family-qualified-shadow.f"
   S\" package DEMO : DEMO:structure ( -- ) ; structure require dep.f ;package\n"
   SDT-WRITE-ENTRY
   s" SD-TEST:SDT-ENTRY$ included"
   SDT-LOAD-OUT SDT-LOAD-CAP >LEN SDT-LOAD-ERR SDT-LOAD-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN {: qoutu:len qerru:len qoc :}
   s" SD-TEST:SDT-ENTRY$ included"
   SDT-LOAD-OUT qoutu LEN>N >str SDT-LOAD-ERR qerru LEN>N >str
   qoc 0 T-ENDING-EXITED=
   SDT-LOAD-OUT qoutu LEN>N >str s" LOADED" T$=
   [: SDT-DISCOVER ;] E-DISC-SHADOW SDT-STOPS
   s" mutual-shadow.f"
   S\" package DEMO : mutual ( -- ) ; mutual require dep.f ;package\n"
   SDT-WRITE-ENTRY
   E-DISC-SHADOW 31 6 SDT-STOPS-AT ;

: SDT-TEST-FAMILY-RETIRE ( -- )
   s" family-retire.f"
   S\" package DEMO undefine enum\nenum require dep.f ;package\n"
   SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-RETIRE SDT-STOPS ;

\ A library storage definer (src/core/layout-buffer.f, pointer-storage.f) is a
\ word, not a keyword, so its operand is code: storage named with a loader
\ word's spelling is rejected at it, E-DISC-DYNAMIC at PROVIDED, though it loads
\ and checks. The walk has no wordlists: after a redefinition of the definer,
\ `require x.f` as its operand loads x.f. A rejection is safe, a missed load is
\ not.
: SDT-TEST-STORAGE-DEFINER ( -- )
   s" storage.f" S\" package CVT-RES\nTYPED-VARIABLE PROVIDED n\n;package\n" SDT-WRITE-ENTRY
   E-DISC-DYNAMIC 31 8 SDT-STOPS-AT ;

\ The definers are interpret keywords: in a body one is a call that takes its
\ name when the word runs. The `;` after `create` ends MK, so the top-level
\ loader after it, which takes no literal, is rejected at itself.
: SDT-TEST-DEFINER-IN-BODY ( -- )
   s" definer-body.f" S\" : MK ( -- ) create ;\nrequired\n" SDT-WRITE-ENTRY
   E-DISC-DYNAMIC 21 8 SDT-STOPS-AT ;

\ --- oversized string literals: data tolerated, loader path rejected ---------

: SDT-X16$ ( -- str )
   s" xxxxxxxxxxxxxxxx" ;

: SDT-DOT16$ ( -- str )
   s" ././././././././" ;

\ SDT-FIT bytes pass PATH-CAP only once joined to a root, SDT-BIG as written.
PATH-CAP 16 / 1+ 16 * constant SDT-BIG
PATH-CAP 16 / 16 * constant SDT-FIT

\ writes name = head + u bytes of the 16-byte piece, u a multiple of 16, + tail
: SDT-WRITE-BIG ( str str n str str -- )
   {: name:str head:str u:n piece:str tail:str :}
   SDT-ROOT$ name SDT-ENTRY FS:JOIN-PATH SDT-ENTRY-U !
   SDT-ENTRY$ head FS:WRITE-ALL
   u 16 / 0 ?do SDT-ENTRY$ piece FS:APPEND-FILE loop
   SDT-ENTRY$ tail FS:APPEND-FILE ;

: SDT-TEST-BIG-STRING-DATA ( -- )
   s" big-ok.f" S\" s\" " SDT-BIG SDT-X16$ S\" \" 2drop\n" SDT-WRITE-BIG
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 0 T= ;

\ The entry's top-level `required` of a path that resolves past the cap: the
\ loader's E-PATH-RANGE record, status 70.
: SDT-RANGE-STOP ( -- )
   REJECTION:CAPTURE
   [: SDT-RUN-ENTRY ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s" E-PATH-RANGE" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" `required` got a path" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;

: SDT-TEST-BIG-STRING-LOADER ( -- )
   s" big-bad.f" S\" s\" " SDT-BIG SDT-X16$ S\" \" required\n" SDT-WRITE-BIG
   SDT-RANGE-STOP
   s" big-resolved.f" S\" s\" " SDT-FIT SDT-X16$ S\" \" required\n" SDT-WRITE-BIG
   SDT-RANGE-STOP ;

\ In a body the same loads are calls the word makes when it runs, which load
\ nothing: a path the resolver rejects is nothing to follow or reject.
: SDT-TEST-BIG-STRING-BODY ( -- )
   s" big-body.f" S\" : L ( -- ) s\" " SDT-BIG SDT-X16$ S\" \" required ;\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 0 T=
   s" big-body-resolved.f" S\" : L ( -- ) s\" " SDT-FIT SDT-X16$ S\" \" required ;\n" SDT-WRITE-BIG
   [: SDT-RUN-ENTRY ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 0 T= ;

\ A path over PATH-CAP as written that the resolver takes is a load the walk
\ follows, at top level and in a body.
: SDT-TEST-BIG-STRING-PAD ( -- )
   s" big-pad.f" S\" s\" " SDT-BIG SDT-DOT16$ S\" sd-pad.f\" required\n" SDT-WRITE-BIG
   s" sd-pad.f" SDT-ONE-LOAD
   s" big-body-pad.f" S\" : L ( -- ) s\" " SDT-BIG SDT-DOT16$ S\" sd-pad.f\" required ;\n" SDT-WRITE-BIG
   s" sd-pad.f" SDT-ONE-LOAD ;

\ Loaders past 1 MiB are found, the same scratch then grows again, and the
\ smaller fixtures after this see no stale text.
: SDT-TEST-LARGE-SOURCE ( -- )
   s" large.f" s" " SDT-WRITE-ENTRY
   SDT-DISCOVER SOURCE-EVENT:COUNT 0 T=
   SDT-SRC-CAP 0 ?do 32 SDT-SRC i + c! loop
   $100 0 ?do SDT-ENTRY$ SDT-SRC SDT-SRC-CAP >str FS:APPEND-FILE loop
   SDT-ENTRY$ S\" require sd-large-a.f\n" FS:APPEND-FILE
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-large-a.f" SDT-PATH T$=
   SDT-ENTRY$ S\" include sd-large-b.f\n" FS:APPEND-FILE
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 2 T=
   0 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-REQUIRED T=
   1 SOURCE-EVENT:KIND@ SOURCE-EVENT:KIND-INCLUDED T=
   1 SOURCE-EVENT:PATH@ s" sd-large-b.f" SDT-PATH T$= ;

\ --- a tree file: the loader's own definitions ---------------------------------

: SDT-TEST-LOADER-SITE ( -- )
   s" src/core/include.f" SOURCE-ROOT:ENTRY-RESOLVE drop SOURCE-ROOT:RESOLVED-ROOT$
   DISCOVER:READ-IN DISCOVER:RUN-READ
   SOURCE-EVENT:COUNT 0 T= ;

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
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-after.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-SCOPES ( -- )
   s" local-scopes.f"
   S\" : EXAMPLE ( n -- ) dup if {: required:n :} required drop else drop s\" sd-else.f\" required then 1 0 ?do 1 {: required:n :} required drop loop s\" sd-loop.f\" required 1 case 1 of 2 {: required:n :} required drop endof endcase s\" sd-case.f\" required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 3 T=
   0 SOURCE-EVENT:PATH@ s" sd-else.f" SDT-PATH T$=
   1 SOURCE-EVENT:PATH@ s" sd-loop.f" SDT-PATH T$=
   2 SOURCE-EVENT:PATH@ s" sd-case.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-QUOTATION ( -- )
   s" local-quotation.f"
   S\" : EXAMPLE ( n -- n ) {: required:n :} [: s\" sd-quote.f\" required ;] execute required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-quote.f" SDT-PATH T$= ;


: SDT-TEST-LOCAL-CASE ( -- )
   s" local-case.f"
   S\" : EXAMPLE ( n -- n ) {: required:n :} s\" sd-uppercase.f\" REQUIRED required ;\n"
   SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-uppercase.f" SDT-PATH T$= ;


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
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-control.f" SDT-PATH T$= ;


\ A parsing word's raw operands are data even when they spell loaders or
\ source delimiters. Rows are collected before any consumer walks its files.
: SDT-HARVEST-ENTRY ( -- )
   [: SDT-DISCOVER ;] DISCOVER:HARVEST ;

\ A pass's end decides which rows a spelling may use; a pass over files already
\ harvested decides only that.
: SDT-SETTLE ( -- )  DISCOVER:ROWS-PASS-BEGIN DISCOVER:ROWS-PASS-END ;

: SDT-SELECT-NAMED ( str -- ) {: name:str :}
   SDT-ROOT$ name SDT-ENTRY FS:JOIN-PATH SDT-ENTRY-U ! ;

: SDT-HARVEST-NAMED ( str -- )
   SDT-SELECT-NAMED SDT-HARVEST-ENTRY ;

: SDT-ROW-LIB ( -- )
   s" row-lib.f"
   S\" package SDT-R public\n: W ( -- ) ;\nparses-through: W 1 ( ;W SDT-R:;W )\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY ;

: SDT-TEST-ROWS ( -- )
   DISCOVER:ROWS-RESET
   SDT-ROW-LIB
   DISCOVER:ROWS-VERSION SDT-ROW-LIB DISCOVER:ROWS-VERSION T=
   S\" SDT-R:W required : ( s\" ;W\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   S\" SDT-R:W include missing.f SDT-R:;W\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   \ Harvesting the same file twice must not invent a second declaration.
   SDT-ROW-LIB
   S\" SDT-R:W required SDT-R:;W\nrequire sd-raw.f\n"
   s" sd-raw.f" SDT-ONE-EVENT
   \ A normal walk records no rows, and reset removes the earlier harvest.
   DISCOVER:ROWS-RESET
   s" row-lib.f" S\" package SDT-R : W ( -- ) ; parses: W 1 ;package\n"
   SDT-WRITE-ENTRY SDT-DISCOVER
   s" row-use.f" s" SDT-R:W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS ;

: SDT-TEST-ROW-KEYS ( -- )
   DISCOVER:ROWS-RESET
   SDT-ROW-LIB
   s" row-use.f" s" OTHER:W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   s" row-use.f" s" package SDT-R W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   DISCOVER:ROWS-RESET
   s" row-bare.f" s" : PN ( -- ) ; parses: PN 1 parses: PN 2" SDT-WRITE-ENTRY
   SDT-HARVEST-ENTRY
   S\" PN required\nrequire sd-raw.f\n" s" sd-raw.f" SDT-ONE-EVENT
   DISCOVER:ROWS-RESET
   s" row-qualified.f"
   s" package SDT-Q : W ( -- ) ; ;package package OTHER parses: SDT-Q:W 1 ;package"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   S\" SDT-Q:W required\nrequire sd-raw.f\n" s" sd-raw.f" SDT-ONE-EVENT
   \ Inside SDT-P, `SDT-Q:W` names the W of part SDT-P:SDT-Q and runs its row.
   DISCOVER:ROWS-RESET
   s" row-part.f"
   S\" package SDT-P package SDT-Q public : W ( -- ) parse-name drop ; parses: W 1 ;package\nSDT-Q:W required\nrequire sd-raw.f\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   s" sd-raw.f" SDT-ONE-LOAD
   \ The part's W also counts against top-level SDT-Q's row, so the row selects
   \ no word and the load after `SDT-Q:W` is found.
   DISCOVER:ROWS-RESET
   s" row-part-twin.f"
   S\" package SDT-Q public : W ( -- ) parse-name drop ; parses: W 1 ;package\npackage SDT-P package SDT-Q public : W ( -- ) ; ;package\nSDT-Q:W require sd-raw.f\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY SDT-SETTLE
   s" sd-raw.f" SDT-ONE-LOAD
   \ A W in SDT-R's global section is top-level, not SDT-R's, so `SDT-R:W` still
   \ selects the row.
   DISCOVER:ROWS-RESET
   s" row-global.f" s" package SDT-R global : W ( -- ) ; ;package" SDT-WRITE-ENTRY
   SDT-HARVEST-ENTRY
   SDT-ROW-LIB SDT-SETTLE
   S\" SDT-R:W required SDT-R:;W\nrequire sd-raw.f\n" s" sd-raw.f" SDT-ONE-EVENT ;

\ A redeclaration or retirement makes a spelling insufficient to select the
\ row's word. This holds whichever file was harvested first.
: SDT-TEST-ROW-CONFLICT ( -- )
   DISCOVER:ROWS-RESET
   s" row-other.f" s" package SDT-R : W ( -- ) ; ;package" SDT-WRITE-ENTRY
   SDT-HARVEST-ENTRY
   SDT-ROW-LIB SDT-SETTLE
   s" row-use.f" s" SDT-R:W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   DISCOVER:ROWS-RESET
   SDT-ROW-LIB
   s" row-retire.f" s" undefine SDT-R:W" SDT-WRITE-ENTRY SDT-HARVEST-ENTRY SDT-SETTLE
   s" row-use.f" s" SDT-R:W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   DISCOVER:ROWS-RESET
   s" row-bare.f" s" : PN ( -- ) ; parses: PN 1" SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   s" row-other.f" s" package OTHER : PN ( -- ) ; ;package" SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   SDT-SETTLE
   s" row-use.f" s" PN required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   DISCOVER:ROWS-RESET ;

: SDT-TEST-MISSING-PATH ( -- )
   s" missing-path.f" s" include" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS
   s" missing-path.f" s" require" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS ;

\ The walk ends at a top-level root move: lib.f, only under the moved root, is
\ neither an event nor a rejection.
: SDT-TEST-ROOT-MOVE ( -- )
   SDT-ROOT$ s" sub" SDT-ENTRY FS:JOIN-PATH {: subu:n :}
   SDT-ENTRY subu >str FS:MAKE-DIR
   s" sub/lib.f" s" : SDT-LIB ( -- ) ;" SDT-WRITE-ENTRY
   s" root-move.f" S\" s\" sub\" SOURCE-ROOT:ROOT!\nrequire lib.f\n" SDT-WRITE-ENTRY
   SDT-DISCOVER
   SOURCE-EVENT:COUNT 0 T= ;

: SDT-HARVEST-MISSING ( -- )
   s" never-existing.f" SDT-PATH SDT-ROOT$ DISCOVER:READ-IN
   DISCOVER:RUN-READ ;

: SDT-HARVEST-OTHER ( -- ) E-TBL-BOUNDS throw ;

: SDT-TEST-HARVEST-FAULT ( -- )
   DISCOVER:ROWS-RESET
   s" harvest-fault.f" S\" require sd-first.f\nrequired\n" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] DISCOVER:HARVEST
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" sd-first.f" SDT-PATH T$=
   [: SDT-HARVEST-MISSING ;] DISCOVER:HARVEST
   SOURCE-EVENT:COUNT 0 T=
   [: [: SDT-HARVEST-OTHER ;] DISCOVER:HARVEST ;] E-TBL-BOUNDS TTHROWSQ ;

\ A compiled parsing call does not consume the definition's source text, and a
\ body's `require` loads nothing: the string load after them is the one event.
: SDT-TEST-ROW-BODY ( -- )
   DISCOVER:ROWS-RESET
   SDT-ROW-LIB
   S\" : B ( -- ) SDT-R:W require sd-none.f s\" sd-body.f\" required SDT-R:;W ;\n"
   s" sd-body.f" SDT-ONE-EVENT ;

\ A prior partial scan may mistake raw operands for declarations. Replacing
\ that file's evidence on retry removes those false declarations, while a
\ genuine later declaration remains a conflict on every retry.
: SDT-TEST-ROW-ORDER ( -- )
   DISCOVER:ROWS-RESET
   s" row-use.f"
   S\" require row-lib.f\npackage SDT-R\nSDT-R:W : W ; required SDT-R:;W\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   SDT-ROW-LIB
   s" row-use.f" S\" require row-lib.f\npackage SDT-R\nSDT-R:W : W ; required SDT-R:;W\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T=
   DISCOVER:ROWS-RESET
   SDT-ROW-LIB
   s" row-real.f" S\" require row-lib.f\nSDT-R:W required SDT-R:;W\npackage SDT-R : W ( -- ) ; ;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY SDT-HARVEST-ENTRY SDT-SETTLE
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS ;

: SDT-CROSS-PASS ( bool -- ) {: lib-first:bool :}
   DISCOVER:ROWS-PASS-BEGIN
   lib-first if s" cross-lib.f" SDT-HARVEST-NAMED then
   s" cross-a.f" SDT-HARVEST-NAMED
   s" cross-b.f" SDT-HARVEST-NAMED
   lib-first 0= if s" cross-lib.f" SDT-HARVEST-NAMED then
   DISCOVER:ROWS-PASS-END ;

: SDT-CROSS-SETTLE ( bool -- ) {: lib-first:bool :}
   8 0 ?do
      DISCOVER:ROWS-VERSION
      lib-first SDT-CROSS-PASS
      DISCOVER:ROWS-VERSION = DISCOVER:ROWS-PASS-STABLE? and
      if unloop exit then
   loop
   false TTRUE ;

\ The first scan of either consumer sees the other's raw `: W` or `: V`.
\ A complete pass with both library rows must remove both false declarations.
: SDT-TEST-ROW-CROSS ( -- )
   s" cross-lib.f"
   S\" package SDT-CROSS public\n: W ( -- ) ;\nparses-through: W 1 ( ;W )\n: V ( -- ) ;\nparses-through: V 1 ( ;V )\n;package\n"
   SDT-WRITE-ENTRY
   s" cross-a.f" S\" require cross-lib.f\npackage SDT-CROSS\nSDT-CROSS:W : V ; required ;W\n;package\nrequire cross-dep.f\n" SDT-WRITE-ENTRY
   s" cross-b.f" S\" require cross-lib.f\npackage SDT-CROSS\nSDT-CROSS:V : W ; required ;V\n;package\nrequire cross-dep.f\n" SDT-WRITE-ENTRY
   DISCOVER:ROWS-RESET
   false SDT-CROSS-SETTLE
   s" cross-a.f" SDT-SELECT-NAMED [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 2 T=
   s" cross-b.f" SDT-SELECT-NAMED [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 2 T=
   DISCOVER:ROWS-RESET
   true SDT-CROSS-SETTLE
   s" cross-a.f" SDT-SELECT-NAMED [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 2 T= ;

: SDT-CASCADE-PASS ( -- )
   DISCOVER:ROWS-PASS-BEGIN
   s" cascade-lib.f" SDT-HARVEST-NAMED
   s" cascade-dup.f" SDT-HARVEST-NAMED
   s" cascade-bridge.f" SDT-HARVEST-NAMED
   s" cascade-use.f" SDT-HARVEST-NAMED
   DISCOVER:ROWS-PASS-END ;

\ Disabling W exposes a second real V declaration. A later complete pass
\ must disable V too, so its `required` operand is no longer hidden.
: SDT-TEST-ROW-CASCADE ( -- )
   s" cascade-lib.f"
   S\" package SDT-CASCADE public\n: W ( -- ) ;\nparses: W 1\n: V ( -- ) ;\nparses: V 1\n;package\n"
   SDT-WRITE-ENTRY
   s" cascade-dup.f" s" package SDT-CASCADE : W ( -- ) ; ;package" SDT-WRITE-ENTRY
   s" cascade-bridge.f" s" package SDT-CASCADE SDT-CASCADE:W : V ; ;package" SDT-WRITE-ENTRY
   s" cascade-use.f" S\" SDT-CASCADE:V required\nrequire cascade-dep.f\n" SDT-WRITE-ENTRY
   DISCOVER:ROWS-RESET
   8 0 ?do
      DISCOVER:ROWS-VERSION
      SDT-CASCADE-PASS
      DISCOVER:ROWS-VERSION = DISCOVER:ROWS-PASS-STABLE? and
      if leave then
   loop
   DISCOVER:ROWS-PASS-STABLE? TTRUE
   s" cascade-use.f" SDT-SELECT-NAMED
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS ;

\ Replacing an unchanged early row must not let a later row for the same
\ spelling take precedence, including while the early file is being read.
: SDT-TEST-ROW-REPEAT ( -- )
   DISCOVER:ROWS-RESET
   s" repeat-a.f"
   S\" package SDT-REP public\n: W ( -- ) ;\nparses: W 1\nSDT-REP:W : variable X\n;package\n"
   SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   s" repeat-b.f" s" parses: SDT-REP:W 2" SDT-WRITE-ENTRY SDT-HARVEST-ENTRY
   s" repeat-use.f" S\" SDT-REP:W required\nrequire repeat-dep.f\n" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T=
   DISCOVER:ROWS-VERSION
   10 0 ?do
      s" repeat-a.f" SDT-HARVEST-NAMED
      s" repeat-b.f" SDT-HARVEST-NAMED
   loop
   DISCOVER:ROWS-VERSION T=
   s" repeat-use.f" SDT-SELECT-NAMED
   [: SDT-DISCOVER ;] 0 TTHROWSQ
   SOURCE-EVENT:COUNT 1 T= ;

: SDT-CYCLE-PASS ( -- )
   DISCOVER:ROWS-PASS-BEGIN
   s" cycle-lib.f" SDT-HARVEST-NAMED
   s" cycle-a.f" SDT-HARVEST-NAMED
   s" cycle-b.f" SDT-HARVEST-NAMED
   DISCOVER:ROWS-PASS-END ;

\ Both apparent rows are operands of the other's parsing word. They vanish
\ under provisional use, so each vanished identity must stay disabled.
: SDT-TEST-ROW-CYCLE ( -- )
   s" cycle-lib.f" s" package SDT-CYC public : W ( -- ) ; : V ( -- ) ; ;package"
   SDT-WRITE-ENTRY
   s" cycle-a.f" s" SDT-CYC:W parses-through: SDT-CYC:V 0 ( ;V ) ;W"
   SDT-WRITE-ENTRY
   s" cycle-b.f" s" SDT-CYC:V parses-through: SDT-CYC:W 0 ( ;W ) ;V"
   SDT-WRITE-ENTRY
   DISCOVER:ROWS-RESET
   8 0 ?do
      DISCOVER:ROWS-VERSION
      SDT-CYCLE-PASS
      DISCOVER:ROWS-VERSION = DISCOVER:ROWS-PASS-STABLE? and
      if leave then
   loop
   DISCOVER:ROWS-PASS-STABLE? TTRUE
   DISCOVER:ROWS-VERSION
   8 0 ?do SDT-CYCLE-PASS loop
   DISCOVER:ROWS-VERSION T=
   s" cycle-use.f" s" SDT-CYC:W required" SDT-WRITE-ENTRY
   [: SDT-DISCOVER ;] E-DISC-DYNAMIC SDT-STOPS ;

public
: SDT-MAIN ( -- )
   T-RESET
   SDT-PREP
   SDT-TEST-LARGE-SOURCE
   SDT-TEST-MIXED
   SDT-TEST-SPELLING
   SDT-TEST-FRESH
   SDT-TEST-WIDE
   SDT-TEST-EMIT
   SDT-TEST-TEXT
   SDT-TEST-DYNAMIC
   SDT-TEST-OPENER
   SDT-TEST-COLON-BODY
   SDT-TEST-USER-DEFINER
   SDT-TEST-UNTERM-STRING
   SDT-TEST-PARSED-OPERAND
   SDT-TEST-COMMENTED-LITERAL
   SDT-TEST-BODY-DYNAMIC
   SDT-TEST-BODY-OPENER
   SDT-TEST-LOADER-NAME
   SDT-TEST-LOADER-USE
   SDT-TEST-DEFINER-OPERAND
   SDT-TEST-FAMILY-BODIES
   SDT-TEST-FAMILY-SHADOW
   SDT-TEST-FAMILY-RETIRE
   SDT-TEST-STORAGE-DEFINER
   SDT-TEST-DEFINER-IN-BODY
   SDT-TEST-BIG-STRING-DATA
   SDT-TEST-BIG-STRING-LOADER
   SDT-TEST-BIG-STRING-BODY
   SDT-TEST-BIG-STRING-PAD
   SDT-TEST-LOADER-SITE
   SDT-TEST-EMIT-CAP
   SDT-TEST-LOCALS
   SDT-TEST-LOCAL-SCOPES
   SDT-TEST-LOCAL-QUOTATION
   SDT-TEST-LOCAL-CASE
   SDT-TEST-LOCAL-LIFETIME
   SDT-TEST-LOCAL-CONTROL
   SDT-TEST-ROWS
   SDT-TEST-ROW-KEYS
   SDT-TEST-ROW-CONFLICT
   SDT-TEST-MISSING-PATH
   SDT-TEST-ROOT-MOVE
   SDT-TEST-HARVEST-FAULT
   SDT-TEST-ROW-BODY
   SDT-TEST-ROW-ORDER
   SDT-TEST-ROW-CROSS
   SDT-TEST-ROW-CASCADE
   SDT-TEST-ROW-REPEAT
   SDT-TEST-ROW-CYCLE
   SOURCE-EVENT:OFF SOURCE-EVENT:DISCOVERY-OFF SOURCE-EVENT:RESET
   FS:CLEANUP-RUN
   T-REPORT
   s" source-discovery-test: ok" type cr ;

;package

SD-TEST:SDT-MAIN
