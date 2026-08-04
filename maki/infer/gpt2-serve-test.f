\ gpt2-serve-test.f - persistent framed GPT-2 service.

require lib/test.f
require lib/process.f
require lib/process-fork.f
require test/checker-assert.f
require maki/infer/gpt2-serve.f

package GPT2-SERVE
private

32 constant T-BUF-N
100 constant T-DELAY-MS

create T-BUF T-BUF-N allot
create T-SEND T-BUF-N allot
create T-LARGE BODY-CAP U32-N + allot
create T-PFD 8 allot

variable T-R
variable T-W

: T-PIPE ( -- )
   PIPE-PAIR T-W ! T-R ! ;

: T-CHILD-DIE ( n -- )
   s" " rot die ;

: T-PUT ( ptr u8 n -- ) {: src:ptr len:n :}
   T-W @ src len write len <> if 2 T-CHILD-DIE then ;

: T-DELAY ( -- )
   T-PFD 0 T-DELAY-MS poll 0<> if 2 T-CHILD-DIE then ;

: T-WAIT ( n -- )
   >PID PROC-WAIT-RC
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH ;

: T-IO-OK ( result<n,n> -- )
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH ;

: T-IO-ERR ( result<n,n> n -- ) {: want:n :}
   MATCH result
      ok OF drop 1 0 T= ENDOF
      err OF want T= ENDOF
   ;MATCH ;

: T-REQ-ERR ( result<option<n>,n> n -- ) {: want:n :}
   MATCH result
      err OF want T= ENDOF
      ok OF
         MATCH option
            none OF 1 0 T= ENDOF
            some OF drop 1 0 T= ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: T-REQ ( result<option<n>,n> n -- ) {: want:n :}
   MATCH result
      err OF drop 1 0 T= ENDOF
      ok OF
         MATCH option
            none OF 1 0 T= ENDOF
            some OF want T= ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: T-EOF ( result<option<n>,n> -- )
   MATCH result
      err OF drop 1 0 T= ENDOF
      ok OF
         MATCH option
            none OF 0 0 T= ENDOF
            some OF drop 1 0 T= ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: T-BL ( n -- CAD-NUM:byte-len )
   PROMPT-LEN
   MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: T-MAX-BOUND ( -- )
   s" max_tokens exactly at the generator cap is accepted" T-LABEL
   GPT2:MAX-TOKENS VALID-MAX
   MATCH result
      err OF drop 1 0 T= ENDOF
      ok OF drop 0 0 T= ENDOF
   ;MATCH
   s" max_tokens one above the generator cap is refused" T-LABEL
   4097 TOKEN-COUNT
   MATCH result
      err OF drop 1 0 T= ENDOF
      ok OF
         VALID-MAX
         MATCH result
            err OF GPT2:E-LIMIT T= ENDOF
            ok OF drop 1 0 T= ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: T-MIN-BODY ( -- )
   s" body length four accepts an empty prompt" T-LABEL
   4 T-SEND U32!
   1 T-SEND U32-N + U32!
   T-PIPE
   T-W @ T-SEND 8 write 8 T=
   T-W @ close
   T-R @ >FD READ-REQUEST 4 T-REQ
   REQ-MAX @ 1 T=
   REQ-PROMPT-U @ 0 T=
   T-R @ close ;

: T-LARGE! ( -- )
   BODY-CAP T-LARGE U32!
   1 T-LARGE U32-N + U32!
   BODY-CAP U32-N - 0 ?do $78 T-LARGE 8 + i + c! loop
   $41 T-LARGE 8 + c!
   $5A T-LARGE BODY-CAP U32-N + 1- + c! ;

: T-MAX-BODY ( -- )
   s" body length 4100 accepts all 4096 prompt bytes" T-LABEL
   T-LARGE!
   T-PIPE
   T-W @ T-LARGE BODY-CAP U32-N + write BODY-CAP U32-N + T=
   T-W @ close
   T-R @ >FD READ-REQUEST BODY-CAP T-REQ
   REQ-MAX @ 1 T=
   REQ-PROMPT-U @ BODY-CAP U32-N - T=
   PROMPT c@ $41 T=
   PROMPT BODY-CAP U32-N - 1- + c@ $5A T=
   T-R @ close ;

: T-READ ( n -- ) {: len:n :}
   T-R @ >FD T-BUF len READ-EXACT T-IO-OK ;

: T-CLEAN-EOF ( -- )
   s" clean EOF exists only before a prefix byte" T-LABEL
   T-PIPE
   T-W @ close
   T-R @ >FD READ-REQUEST T-EOF
   T-R @ close ;

: T-PARTIAL-PREFIX ( -- )
   s" EOF after a partial prefix is framing failure" T-LABEL
   T-PIPE
   T-W @ T-SEND 2 write 2 T=
   T-W @ close
   T-R @ >FD READ-REQUEST E-FRAME T-REQ-ERR
   T-R @ close ;

: T-OVERSIZED ( -- )
   s" body length counts bytes after the prefix and rejects 4101" T-LABEL
   T-PIPE
   $05 T-SEND c!
   $10 T-SEND 1 + c!
   0 T-SEND 2 + c!
   0 T-SEND 3 + c!
   T-W @ T-SEND U32-N write U32-N T=
   T-W @ close
   T-R @ >FD READ-REQUEST E-FRAME T-REQ-ERR
   T-R @ close ;

: T-UNDERSIZED ( -- )
   s" body length below the four-byte max_tokens field is rejected" T-LABEL
   T-PIPE
   3 T-SEND c!
   0 T-SEND 1 + c!
   0 T-SEND 2 + c!
   0 T-SEND 3 + c!
   T-W @ T-SEND U32-N write U32-N T=
   T-W @ close
   T-R @ >FD READ-REQUEST E-FRAME T-REQ-ERR
   T-R @ close ;

: T-READ-ERROR ( -- )
   s" raw read failure remains E-FS-IO" T-LABEL
   T-PIPE
   T-R @ close
   T-R @ >FD READ-REQUEST E-FS-IO T-REQ-ERR
   T-W @ close ;

: T-PREFIX-REQUEST! ( -- )
   5 T-SEND c!
   0 T-SEND 1 + c!
   0 T-SEND 2 + c!
   0 T-SEND 3 + c!
   1 T-SEND 4 + c!
   0 T-SEND 5 + c!
   0 T-SEND 6 + c!
   0 T-SEND 7 + c!
   $48 T-SEND 8 + c! ;

: T-PREFIX-CHILD ( -- )
   T-R @ close
   T-SEND 1 T-PUT
   T-DELAY
   T-SEND 1 + 8 T-PUT
   T-W @ close
   0 T-CHILD-DIE ;

: T-SPLIT-PREFIX ( -- )
   s" a prefix split after one byte is read exactly" T-LABEL
   T-PREFIX-REQUEST!
   T-PIPE
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if T-PREFIX-CHILD then
   T-W @ close
   T-R @ >FD READ-REQUEST 5 T-REQ
   REQ-MAX @ 1 T=
   REQ-PROMPT-U @ 1 T=
   PROMPT c@ $48 T=
   T-R @ close
   pid T-WAIT ;

: T-BODY-REQUEST! ( -- )
   9 T-SEND c!
   0 T-SEND 1 + c!
   0 T-SEND 2 + c!
   0 T-SEND 3 + c!
   1 T-SEND 4 + c!
   0 T-SEND 5 + c!
   0 T-SEND 6 + c!
   0 T-SEND 7 + c!
   $48 T-SEND 8 + c!
   $65 T-SEND 9 + c!
   $6C T-SEND 10 + c!
   $6C T-SEND 11 + c!
   $6F T-SEND 12 + c! ;

: T-BODY-CHILD ( -- )
   T-R @ close
   T-SEND 6 T-PUT
   T-DELAY
   T-SEND 6 + 7 T-PUT
   T-W @ close
   0 T-CHILD-DIE ;

: T-PARTIAL-BODY ( -- )
   s" EOF after a partial body is framing failure" T-LABEL
   T-BODY-REQUEST!
   T-PIPE
   T-W @ T-SEND 10 write 10 T=
   T-W @ close
   T-R @ >FD READ-REQUEST E-FRAME T-REQ-ERR
   T-R @ close ;

: T-SPLIT-BODY ( -- )
   s" a body split inside max_tokens is read exactly" T-LABEL
   T-BODY-REQUEST!
   T-PIPE
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if T-BODY-CHILD then
   T-W @ close
   T-R @ >FD READ-REQUEST 9 T-REQ
   REQ-MAX @ 1 T=
   REQ-PROMPT-U @ 5 T=
   PROMPT 5 s" Hello" T$=
   T-R @ close
   pid T-WAIT ;

: T-SUCCESS-FRAME ( -- )
   s" success body length excludes its u32 prefix" T-LABEL
   $61 OUTPUT c!
   $62 OUTPUT 1 + c!
   $63 OUTPUT 2 + c!
   T-PIPE
   T-W @ >FD 3 T-BL WRITE-SUCCESS T-IO-OK
   T-W @ close
   8 T-READ
   T-BUF c@ 4 T=
   T-BUF 1 + c@ 0 T=
   T-BUF 2 + c@ 0 T=
   T-BUF 3 + c@ 0 T=
   T-BUF 4 + c@ TAG-OK T=
   T-BUF 5 + 3 s" abc" T$=
   T-R @ close ;

: T-EMPTY-SUCCESS ( -- )
   s" empty continuation is a one-byte success body" T-LABEL
   T-PIPE
   T-W @ >FD 0 T-BL WRITE-SUCCESS T-IO-OK
   T-W @ close
   5 T-READ
   T-BUF c@ 1 T=
   T-BUF 1 + c@ 0 T=
   T-BUF 2 + c@ 0 T=
   T-BUF 3 + c@ 0 T=
   T-BUF 4 + c@ TAG-OK T=
   T-R @ close ;

: T-REFUSAL-FRAME ( -- )
   s" refusal body is tag plus exact signed i64-le error" T-LABEL
   T-PIPE
   T-W @ >FD GPT2:E-LIMIT WRITE-REFUSAL T-IO-OK
   T-W @ close
   REFUSAL-N T-READ
   T-BUF c@ 9 T=
   T-BUF 1 + c@ 0 T=
   T-BUF 2 + c@ 0 T=
   T-BUF 3 + c@ 0 T=
   T-BUF 4 + c@ TAG-ERR T=
   T-BUF 5 + c@ $DD T=
   T-BUF 6 + c@ $E9 T=
   7 begin dup REFUSAL-N < while
      T-BUF over + c@ $FF T=
      1+
   repeat drop
   T-R @ close ;

: T-WRITE-ERROR ( -- )
   s" raw write failure remains E-FS-IO" T-LABEL
   T-PIPE
   T-W @ >FD FD-NOSIGPIPE!
   T-R @ close
   T-W @ >FD s" x" WRITE-EXACT E-FS-IO T-IO-ERR
   T-W @ close ;

: T-TERMINAL-PRIMARY ( -- )
   s" generation refusal remains primary when its frame cannot be written" T-LABEL
   T-PIPE
   T-W @ >FD FD-NOSIGPIPE!
   T-R @ close
   T-W @ >FD GPT2:E-PROMPT TERMINAL-REFUSAL
      GPT2:E-PROMPT T-REQ-ERR
   T-W @ close ;

: T-SURFACE ( -- )
   s" GST-RUN ( -- ) GPT2-SERVE:RUN" CHECK-QUIET-CANDIDATE! -1 T=
   s" GPT2-SERVE:BL>N" XREF-FIND XREF-FOUND? TFALSE
   s" GSS-BL-PRIVATE ( CAD-NUM:byte-len -- n ) GPT2-SERVE:BL>N"
      CHECK-QUIET-CANDIDATE! 1 T=
   E-FRAME -5668 T= ;

: T-RUN ( -- )
   T-RESET
   T-SURFACE
   BODY-CAP 4100 T=
   T-MAX-BOUND
   T-MIN-BODY
   T-MAX-BODY
   T-CLEAN-EOF
   T-PARTIAL-PREFIX
   T-OVERSIZED
   T-UNDERSIZED
   T-READ-ERROR
   T-SPLIT-PREFIX
   T-PARTIAL-BODY
   T-SPLIT-BODY
   T-SUCCESS-FRAME
   T-EMPTY-SUCCESS
   T-REFUSAL-FRAME
   T-WRITE-ERROR
   T-TERMINAL-PRIMARY
   T-REPORT ;

T-RUN

;package
