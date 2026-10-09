\ A bare require resolves beside the program: the --load entry's directory is
\ the first root of the files it requires (src/core/include.f ENTRY-RESOLVE).
\ Only the entry is the file the command line named.
require r27.inc
R27:SIB . SCRIPT-NAMED-LOAD? .
