\ closed-source-residue-bad.f - a loaded file that leaves two cells. Every
\ require, include and --load runs a file inside LOAD-UNIT's source-unit-run,
\ where CALL-UNIT checks what it leaves (src/core/include.f), so loading this
\ file is refused E-EVAL-RESIDUE. test/closed-source-suite.f loads it through
\ `required` and asserts that refusal.
1 2
