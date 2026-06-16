\ fs-test.f — focused tests for tools/fs.f. Load after tools/fs.f.

variable #FAIL
variable #CASE
variable FS-SEEN
variable FS-SEEN-F

: T= {: got want :} ( got want -- )
   #CASE @ 1 + #CASE !
   got want <> IF
      [char] F emit #CASE @ .
      #FAIL @ 1 + #FAIL !
   THEN ;

: COUNT-FILE {: a u :} ( a u -- )
   FS-SEEN @ 1 + FS-SEEN !
   a u s" .f" HAS-EXT? IF FS-SEEN-F @ 1 + FS-SEEN-F ! THEN ;

: WALK-MISSING ( -- )
   s" no-such-habu-fs-path" ['] COUNT-FILE WALK-FILES ;

s" AGENTS.md" EXISTS? -1 T=
s" no-such-habu-fs-path" EXISTS? 0 T=
s" AGENTS.md" FILE? -1 T=
s" AGENTS.md" DIR? 0 T=
s" src" DIR? -1 T=
s" src" FILE? 0 T=
s" src/core/checker.f" s" .f" HAS-EXT? -1 T=
s" src/core/checker.f" s" .md" HAS-EXT? 0 T=
s" .jj" SKIP-DIR? -1 T=
s" .git" SKIP-DIR? -1 T=
s" .dots" SKIP-DIR? -1 T=
s" src" SKIP-DIR? 0 T=

0 FS-SEEN !  0 FS-SEEN-F !
s" examples" ' COUNT-FILE WALK-FILES
FS-SEEN @ 2 T=
FS-SEEN-F @ 2 T=

0 FS-SEEN !  0 FS-SEEN-F !
s" bench" ' COUNT-FILE WALK-FILES
FS-SEEN @ 0 > -1 T=

0 FS-SEEN !  0 FS-SEEN-F !
s" AGENTS.md" ' COUNT-FILE WALK-FILES
FS-SEEN @ 1 T=
FS-SEEN-F @ 0 T=

' WALK-MISSING catch E-FS-STAT T=

: REPORT
   #FAIL @ 0 = IF s" ok" type cr exit THEN
   #FAIL @ . s" fs-test: failures" 1 die ;
REPORT
