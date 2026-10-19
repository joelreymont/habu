\ Run: bin/hb --load tools/lsp-entry-test.f
\ An absolute server entry must load its own implementation when the client
\ starts it from a project directory containing older Habu tool sources.

require src/lib/errors.f
require src/lib/string.f
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
   CLEANUP-RESET
   s" hb-lsp-entry" HB-TMP-MKDIR {: d:ptr du:n :}
   d du CLEANUP-TREE+
   d PROJECT du BYTE-COPY  du PROJECT-U !
   s" tools" PROJECT-PATH MAKE-DIRS
   s" src/core" PROJECT-PATH MAKE-DIRS
   s" src/habu" PROJECT-PATH MAKE-DIRS
   s" src/core/util.f" PROJECT-PATH s" " WRITE-ALL
   s" tools/lsp-core.f" POISON
   s" src/habu/verify-source.f" POISON
   s" dep.f" PROJECT-PATH s\" : DEP ( -- n ) 1 ;\n" WRITE-ALL
   s" subject.f" PROJECT-PATH {: p:ptr pu:n :}
   p SUBJECT pu BYTE-COPY  pu SUBJECT-U !
   SUBJECT SUBJECT-U @ s\" require dep.f\n: A ( -- n ) DEP ;\n" WRITE-ALL
   s" tools/lsp.f" SERVER-ENTRY 2drop
   s" tools/check-verify-child.f" SERVER-ENTRY drop CHILD SERVER-U @ BYTE-COPY
   SERVER-U @ CHILD-U !
   s" tools/lsp.f" SERVER-ENTRY 2drop ;

: STAGE ( ptr u8 n -- )
   PROC-CMD:RESET
   PROJECT PROJECT-U @ PROC-CMD:CWD!
   s" --load" PROC-CMD:ARG+
   PROC-CMD:ARG+ ;

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

: SERVER-CASE ( ptr u8 n -- )
   STAGE
   ENGINE-CANDIDATE:PATH$ 30000 >MS PROC-CMD:RUN-RC RC>N
   dup 1 <> if PROC-CMD:ERR$ type cr then 1 T=
   PROC-CMD:ERR$ s" lsp: input ended before exit" CONTAINS? TTRUE ;

: CHILD-CASE ( ptr u8 n -- )
   STAGE
   s" --" PROC-CMD:ARG+
   SUBJECT SUBJECT-U @ PROC-CMD:ARG+
   s\" \nrequire dep.f\n: A ( -- n ) DEP ;\n" PROC-CMD:IN!
   ENGINE-CANDIDATE:PATH$ 30000 >MS PROC-CMD:RUN-RC RC>N
   dup 0 <> if PROC-CMD:ERR$ type cr then 0 T=
   PROC-CMD:OUT$ s" check-verify: verified" CONTAINS? TTRUE ;

public

: TEST ( -- )
   SETUP
   SERVER SERVER-U @ SERVER-CASE
   CHILD CHILD-U @ CHILD-CASE
   SERVER SERVER-U @ NESTED SERVER-CASE
   CHILD CHILD-U @ NESTED CHILD-CASE
   CLEANUP-RUN
   T-REPORT ;

;package

LSP-ENTRY-TEST:TEST
