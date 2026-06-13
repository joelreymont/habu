\ t-sh-jdiag.fs — `JSON-DIAGS ON` makes the checker emit a structured JSON object
\ per reject (code/word/token/expected/actual) to stderr instead of the human
\ prose line — the high-leverage form for LLM repair. Default off = prose (the
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

\ JSON on: the reject surfaces as a JSON object naming the code + word.
T{ -1 SRC ERR-OF  s\" {\"code\":\"E-MISMATCH\"" HAS? -> true }T
T{ -1 SRC ERR-OF  s\" \"word\":\"sqbad\""        HAS? -> true }T
\ JSON off (default): prose line, no JSON object.
T{  0 SRC ERR-OF  s" habu: in"                   HAS? -> true }T
T{  0 SRC ERR-OF  s\" {\"code\""                 HAS? -> false }T

cr ." t-sh-jdiag: " #ERRORS @ . ." failure(s)" cr
#ERRORS @ 0<> negate (bye)
