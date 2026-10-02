\ closed-source-residue-bad.f - a loaded file that leaves two cells. Every
\ require, include and --load runs a file as a closed program
\ (src/core/include.f INCLUDE-EVALUATE over evaluate-closed), so loading this
\ file is refused E-EVAL-RESIDUE. test/closed-source-suite.f loads it through
\ `required` and asserts that refusal.
1 2
