\ t-sh-jdiag.fs — `JSON-DIAGS ON` makes the checker emit a structured JSON object
\ per reject/uncheckable verdict to stderr instead of the human prose line — the
\ high-leverage form for LLM repair. Default off = prose (the
\ existing diagnostic, covered by the other suites). Run: gforth test/t-sh-jdiag.fs -e bye
require sh-driver.fs

\ build an engine = util+checker+render + a JSON-or-prose CHECK! hook + a body that
\ contradicts its declared sig (rejects), run it, return its stderr (the diagnostic).
: ERR-OF ( a u -- a u )
   s" /tmp/nf-jd-bin" FORTH-EXE
   s" /tmp/nf-jd-bin >/dev/null 2>/tmp/nf-jd-err" system
   s" /tmp/nf-jd-err" slurp-file ;
: HAS? ( a u sub-a sub-u -- f )  search nip nip ;
: SRC ( json? -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   IF s" -1 JSON-DIAGS ! " +B THEN
   s" : H CHECK! ; ' H set-check : SQBAD ( i64 -- i64 ) dup ; " +B  CBUF CL @ ;
: SRC-U ( -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   s" -1 JSON-DIAGS ! : H CHECK ; ' H set-check : U [: leave ;] drop ; " +B
   CBUF CL @ ;

\ JSON on: the reject surfaces as a JSON object naming the code + word, plus
\ machine-readable repair context.
T{ -1 SRC ERR-OF  s\" {\"code\":\"E-MISMATCH\"" HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"verdict\":\"rejected\"" HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"word\":\"sqbad\""        HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"token\":\"dup\""         HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"token_index\":1"         HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"file\":\"<input>\""      HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"line\":1"                HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"column\":"               HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"byte_start\":"           HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"byte_end\":"             HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"definition_source\":"    HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"declared_effect\":\"i64 -- i64 " HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"inferred_effect\":\"i64 -- i64 i64 " HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"return_stack\":{\"expected\":\"\"" HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"suggestion\":\"the body leaves more"  HAS? -> true }T
\ JSON on also classifies uncheckable definitions distinctly.
T{ SRC-U ERR-OF  s\" \"code\":\"E-UNCHECKABLE\"" HAS? -> true }T
T{ SRC-U ERR-OF  s\" \"verdict\":\"uncheckable\"" HAS? -> true }T
T{ SRC-U ERR-OF  s\" \"token\":\"leave\"" HAS? -> true }T
T{ SRC-U ERR-OF  s\" \"token_index\":2" HAS? -> true }T
\ JSON off (default): prose line, no JSON object.
T{  0 SRC ERR-OF  s" habu: in"                   HAS? -> true }T
T{  0 SRC ERR-OF  s\" {\"code\""                 HAS? -> false }T

cr ." t-sh-jdiag: " #ERRORS @ . ." failure(s)" cr
#ERRORS @ 0<> negate (bye)
