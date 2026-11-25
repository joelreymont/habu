\ An absolute server entry must load its own implementation when the client
\ starts it from a project directory containing older Habu tool sources.
\ The printed artifact keeps both server conversations and their fixtures.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/engine-candidate.f

package LSP-ENTRY-TEST

create PROJECT PATH-CAP allot    variable PROJECT-U
create SERVER PATH-CAP allot     variable SERVER-U
create CHILD PATH-CAP allot      variable CHILD-U
create SUBJECT PATH-CAP allot    variable SUBJECT-U
create PATH PATH-CAP allot       variable PATH-U
create DRIVER PATH-CAP allot     variable DRIVER-U
create WIRE 16384 allot            variable WIRE-U
create BODY 4096 allot

: RC>N ( result<n,n> -- n )
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: PROJECT-PATH ( str -- str )
   PROJECT PROJECT-U @ >str swap PATH FS:JOIN-PATH PATH-U !
   PATH PATH-U @ >str ;

: SERVER-ENTRY ( str -- str )
   SOURCE-ROOT:ENGINE$ swap SERVER FS:JOIN-PATH SERVER-U !
   SERVER SERVER-U @ >str ;

: POISON ( str -- )
   PROJECT-PATH s\" 86 throw\n" FS:WRITE-ALL ;

: SETUP ( -- )
   s" hb-lsp-entry" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA PROJECT d STR:LEN BYTE-COPY  d STR:LEN PROJECT-U !
   s" tools" PROJECT-PATH FS:MAKE-DIRS
   s" src/core" PROJECT-PATH FS:MAKE-DIRS
   s" src/habu" PROJECT-PATH FS:MAKE-DIRS
   s" src/core/exit-status.f" PROJECT-PATH s" " FS:WRITE-ALL
   s" tools/lsp-core.f" POISON
   s" tools/check-verify-child-lib.f" POISON
   s" src/habu/verify-source.f" POISON
   s" dep.f" PROJECT-PATH s\" : DEP ( -- n ) 1 ;\n" FS:WRITE-ALL
   s" subject.f" PROJECT-PATH {: p:str :}
   p STR:DATA SUBJECT p STR:LEN BYTE-COPY  p STR:LEN SUBJECT-U !
   SUBJECT SUBJECT-U @ >str s\" require dep.f\n: A ( -- n ) DEP ;\n: B ( -- n ) A ;\n" FS:WRITE-ALL
   s" tools/lsp.f" SERVER-ENTRY drop
   s" tools/check-verify-child.f" SERVER-ENTRY STR:DATA CHILD SERVER-U @ BYTE-COPY
   SERVER-U @ CHILD-U !
   s" tools/lsp.f" SERVER-ENTRY drop ;

: STAGE ( CMD:command str -- CMD:command )
   {: entry:str :}
   PROJECT PROJECT-U @ >str CMD:CWD!
   \ Exercise a cold image build here, then reuse it for the nested entry.
   s" HABU_OUT" s" out" PROJECT-PATH CMD:ENV+
   s" --load" CMD:ARG+
   entry CMD:ARG+ ;

: QUOTED ( str -- )
   s" s" SB-APPEND 34 SB-APPEND-C 32 SB-APPEND-C
   SB-APPEND 34 SB-APPEND-C ;

\ Keep the inherited root at the entry tree while loading the entry by its
\ absolute path. With this nesting, CURRENT$ names that root, not the file.
: NESTED ( str -- str ) {: entry:str :}
   SB-RESET s" : NESTED-LOAD ( -- ) " SB-APPEND
   entry SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME QUOTED
   s"  [: " SB-APPEND
   entry QUOTED
   s\"  required ;] SOURCE-ROOT:WITH ;\nNESTED-LOAD\n"
   SB-APPEND
   s" driver.f" PROJECT-PATH {: path:str :}
   path STR:DATA DRIVER path STR:LEN BYTE-COPY path STR:LEN DRIVER-U !
   DRIVER DRIVER-U @ >str SB$ FS:WRITE-ALL
   DRIVER DRIVER-U @ >str ;

\ A document request forces the server to run its verifier child.
\ Loading the entry and reaching EOF alone never crosses that boundary.
: FRAME ( -- )
   SB$ {: s:str :}
   s STR:DATA BODY s STR:LEN BYTE-COPY
   SB-RESET s" Content-Length: " SB-APPEND s STR:LEN FMT:SB-U
   s\" \r\n\r\n" SB-APPEND
   SB$ {: h:str :}
   h STR:DATA WIRE WIRE-U @ + h STR:LEN BYTE-COPY h STR:LEN WIRE-U +!
   BODY WIRE WIRE-U @ + s STR:LEN BYTE-COPY s STR:LEN WIRE-U +! ;

: URI+ ( -- )
   s" file://" SB-APPEND SUBJECT SUBJECT-U @ >str SB-APPEND ;

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

: STREAM-PATH ( str str -- str )
   {: name:str suffix:str :}
   SB-RESET name SB-APPEND suffix SB-APPEND
   SB$ PROJECT-PATH ;

\ A child has no deadline: its cold image build takes as long as the host's
\ load makes it, and a hung row is named by its WAIT line.
: SERVER-RUN ( str str CMD:command -- CMD:command )
   >r {: entry:str name:str :} r>
   entry STAGE
   CONVERSATION
   WIRE WIRE-U @ >str CMD:IN!
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-RC RC>N {: rc:n :}
   CMD:OUT$ {: out:str :}
   CMD:ERR$ {: err:str :}
   name s" .in" STREAM-PATH WIRE WIRE-U @ >str FS:WRITE-ALL
   name s" .out" STREAM-PATH out FS:WRITE-ALL
   name s" .err" STREAM-PATH err FS:WRITE-ALL
   rc 1 <> if err type cr then rc 1 T=
   err s\" lsp: input ended before exit\n" T$=
   out s\" \"version\":1,\"diagnostics\":[]" STR:CONTAINS? TTRUE
   out s\" \"id\":2,\"result\":[{\"range\":{\"start\":{\"line\":1,\"character\":2},\"end\":{\"line\":1,\"character\":3}},\"kind\":1}" STR:CONTAINS? TTRUE
   out s\" \"id\":3,\"result\":[{\"range\":" STR:CONTAINS? TTRUE
   out s" dep.f" PROJECT-PATH STR:CONTAINS? TTRUE ;

: SERVER-CASE ( str str -- )
   [: SERVER-RUN ;] CMD:WITH-COMMAND ;

: CHILD-RUN ( str CMD:command -- CMD:command )
   >r {: entry:str :} r>
   entry STAGE
   s" --" CMD:ARG+
   SUBJECT SUBJECT-U @ >str CMD:ARG+
   s\" \nrequire dep.f\n: A ( -- n ) DEP ;\n" CMD:IN!
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-RC RC>N {: rc:n :}
   rc 0 <> if CMD:ERR$ type cr then rc 0 T=
   CMD:OUT$ s" check-verify: verified" STR:CONTAINS? TTRUE ;

: CHILD-CASE ( str -- )
   [: CHILD-RUN ;] CMD:WITH-COMMAND ;

public

: TEST ( -- )
   SETUP
   SERVER SERVER-U @ >str s" direct" SERVER-CASE
   CHILD CHILD-U @ >str CHILD-CASE
   SERVER SERVER-U @ >str NESTED s" nested" SERVER-CASE
   CHILD CHILD-U @ >str NESTED CHILD-CASE
   s" artifact: " type PROJECT PROJECT-U @ >str type cr
   T-REPORT ;

;package

LSP-ENTRY-TEST:TEST
