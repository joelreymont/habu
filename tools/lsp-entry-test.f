\ Run: bin/hb --load tools/lsp-entry-test.f
\ An absolute server entry must load its own implementation when the client
\ starts it from a project directory containing older Habu tool sources.
\ The printed artifact keeps both server conversations and their fixtures.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/engine-candidate.f

package LSP-ENTRY-TEST

create PROJECT FS-PATH-CAP allot    variable PROJECT-U
create SERVER FS-PATH-CAP allot     variable SERVER-U
create CHILD FS-PATH-CAP allot      variable CHILD-U
create SUBJECT FS-PATH-CAP allot    variable SUBJECT-U
create PATH FS-PATH-CAP allot       variable PATH-U
create DRIVER FS-PATH-CAP allot     variable DRIVER-U
create WIRE 16384 allot            variable WIRE-U
create BODY 4096 allot

: RC>N ( result<n,n> -- n )
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: PROJECT-PATH ( ptr u8 n -- ptr u8 n )
   PROJECT PROJECT-U @ 2swap PATH JOIN-PATH PATH-U !
   PATH PATH-U @ ;

: SERVER-ENTRY ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:ENGINE$ 2swap SERVER JOIN-PATH SERVER-U !
   SERVER SERVER-U @ ;

: POISON ( ptr u8 n -- )
   PROJECT-PATH s\" 86 throw\n" WRITE-ALL ;

: SETUP ( -- )
   s" hb-lsp-entry" FS:MKTEMP-DIR {: d:ptr du:n :}
   d PROJECT du BYTE-COPY  du PROJECT-U !
   s" tools" PROJECT-PATH MAKE-DIRS
   s" src/core" PROJECT-PATH MAKE-DIRS
   s" src/habu" PROJECT-PATH MAKE-DIRS
   s" src/core/util.f" PROJECT-PATH s" " WRITE-ALL
   s" tools/lsp-core.f" POISON
   s" tools/check-verify-child-lib.f" POISON
   s" src/habu/verify-source.f" POISON
   s" dep.f" PROJECT-PATH s\" : DEP ( -- n ) 1 ;\n" WRITE-ALL
   s" subject.f" PROJECT-PATH {: p:ptr pu:n :}
   p SUBJECT pu BYTE-COPY  pu SUBJECT-U !
   SUBJECT SUBJECT-U @ s\" require dep.f\n: A ( -- n ) DEP ;\n: B ( -- n ) A ;\n" WRITE-ALL
   s" tools/lsp.f" SERVER-ENTRY 2drop
   s" tools/check-verify-child.f" SERVER-ENTRY drop CHILD SERVER-U @ BYTE-COPY
   SERVER-U @ CHILD-U !
   s" tools/lsp.f" SERVER-ENTRY 2drop ;

: STAGE ( CMD:command ptr u8 n -- CMD:command )
   {: entry:ptr entryu:n :}
   PROJECT PROJECT-U @ CMD:CWD!
   \ Exercise a cold image build here, then reuse it for the nested entry.
   s" HABU_OUT" s" out" PROJECT-PATH CMD:ENV+
   s" --load" CMD:ARG+
   entry entryu CMD:ARG+ ;

: QUOTED ( ptr u8 n -- )
   s" s" SB-APPEND 34 SB-APPEND-C 32 SB-APPEND-C
   SB-APPEND 34 SB-APPEND-C ;

\ Keep the inherited root at the entry tree while loading the entry by its
\ absolute path. With this nesting, CURRENT$ names that root, not the file.
: NESTED ( ptr u8 n -- ptr u8 n ) {: entry:ptr u:n :}
   SB-RESET s" : NESTED-LOAD ( -- ) " SB-APPEND
   entry u SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME QUOTED
   s"  [: " SB-APPEND
   entry u QUOTED
   s\"  required ;] SOURCE-ROOT:WITH ;\nNESTED-LOAD\n" SB-APPEND
   s" driver.f" PROJECT-PATH {: path:ptr pathu:n :}
   path DRIVER pathu BYTE-COPY pathu DRIVER-U !
   DRIVER DRIVER-U @ SB$ WRITE-ALL
   DRIVER DRIVER-U @ ;

\ A document request forces the server to acquire and run its verifier image.
\ Loading the entry and reaching EOF alone never crosses that boundary.
: FRAME ( -- )
   SB$ {: a:ptr u:n :}
   a BODY u BYTE-COPY
   SB-RESET s" Content-Length: " SB-APPEND u FMT:SB-U
   s\" \r\n\r\n" SB-APPEND
   SB$ {: h:ptr hu:n :}
   h WIRE WIRE-U @ + hu BYTE-COPY hu WIRE-U +!
   BODY WIRE WIRE-U @ + u BYTE-COPY u WIRE-U +! ;

: URI+ ( -- )
   s" file://" SB-APPEND SUBJECT SUBJECT-U @ SB-APPEND ;

: CONVERSATION ( -- )
   0 WIRE-U !
   SB-RESET
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{}}}" SB-APPEND FRAME
   SB-RESET
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"" SB-APPEND
   URI+
   s\" \",\"languageId\":\"habu\",\"version\":1,\"text\":\"require dep.f\\n: A ( -- n ) DEP ;\\n: B ( -- n ) A ;\\n\"}}}" SB-APPEND FRAME
   SB-RESET
   s\" {\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"textDocument/documentHighlight\",\"params\":{\"textDocument\":{\"uri\":\"" SB-APPEND
   URI+
   s\" \"},\"position\":{\"line\":2,\"character\":13}}}" SB-APPEND FRAME
   SB-RESET
   s\" {\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/documentLink\",\"params\":{\"textDocument\":{\"uri\":\"" SB-APPEND
   URI+ s\" \"}}}" SB-APPEND FRAME ;

: STREAM-PATH ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n suffix:ptr suffixu:n :}
   SB-RESET name nameu SB-APPEND suffix suffixu SB-APPEND
   SB$ PROJECT-PATH ;

: SERVER-RUN ( ptr u8 n ptr u8 n CMD:command -- CMD:command )
   >r {: entry:ptr entryu:n name:ptr nameu:n :} r>
   entry entryu STAGE
   CONVERSATION
   WIRE WIRE-U @ CMD:IN!
   ENGINE-CANDIDATE:PATH$ 30000 >MS CMD:RUN-RC RC>N {: rc:n :}
   CMD:OUT$ {: out:ptr outu:n :}
   CMD:ERR$ {: err:ptr erru:n :}
   name nameu s" .in" STREAM-PATH WIRE WIRE-U @ WRITE-ALL
   name nameu s" .out" STREAM-PATH out outu WRITE-ALL
   name nameu s" .err" STREAM-PATH err erru WRITE-ALL
   rc 1 <> if err erru type cr then rc 1 T=
   err erru s\" lsp: input ended before exit\n" T$=
   out outu s\" \"version\":1,\"diagnostics\":[]" CONTAINS? TTRUE
   out outu s\" \"id\":2,\"result\":[{\"range\":{\"start\":{\"line\":1,\"character\":2},\"end\":{\"line\":1,\"character\":3}},\"kind\":1}" CONTAINS? TTRUE
   out outu s\" \"id\":3,\"result\":[{\"range\":" CONTAINS? TTRUE
   out outu s" dep.f" PROJECT-PATH CONTAINS? TTRUE ;

: SERVER-CASE ( ptr u8 n ptr u8 n -- )
   [: SERVER-RUN ;] CMD:WITH-COMMAND ;

: CHILD-RUN ( ptr u8 n CMD:command -- CMD:command )
   >r {: entry:ptr entryu:n :} r>
   entry entryu STAGE
   s" --" CMD:ARG+
   SUBJECT SUBJECT-U @ CMD:ARG+
   s\" \nrequire dep.f\n: A ( -- n ) DEP ;\n" CMD:IN!
   ENGINE-CANDIDATE:PATH$ 30000 >MS CMD:RUN-RC RC>N {: rc:n :}
   rc 0 <> if CMD:ERR$ type cr then rc 0 T=
   CMD:OUT$ s" check-verify: verified" CONTAINS? TTRUE ;

: CHILD-CASE ( ptr u8 n -- )
   [: CHILD-RUN ;] CMD:WITH-COMMAND ;

public

: TEST ( -- )
   SETUP
   SERVER SERVER-U @ s" direct" SERVER-CASE
   CHILD CHILD-U @ CHILD-CASE
   SERVER SERVER-U @ NESTED s" nested" SERVER-CASE
   CHILD CHILD-U @ NESTED CHILD-CASE
   s" artifact: " type PROJECT PROJECT-U @ type cr
   T-REPORT ;

;package

LSP-ENTRY-TEST:TEST
