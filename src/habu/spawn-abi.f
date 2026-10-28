\ The request record spawn-fds reads, shared by its engine bodies
\ (src/habu/habu1.f BSPAWNFDS, src/habu/kernel-x64.f SPAWN-FDS-ROW) and the
\ code that builds one. Each field is one cell:
\
\    PATH  the program, a NUL-terminated path
\    ARGV  its arguments, a NULL-terminated array of NUL-terminated strings
\    ENVP  its environment, the same shape
\    CWD   the directory the child starts in, NUL-terminated, or 0 to stay
\    FDS   the descriptor map, an array of NFDS cells: the child's fd i is the
\          parent's fd FDS[i], and -1 leaves fd i closed
\    NFDS  the map's length; every child fd at or above it is closed
\
\ IT IS ITS OWN FILE for the reason src/habu/arith-abi.f is: the builder
\ payload tools/build-fixpoint.f assembles carries the emitters without the
\ checker boot prefix, so the offsets the emitters read sit in a file that
\ payload can carry on its own.
package SPAWN-ABI
public

0 constant PATH
8 constant ARGV
16 constant ENVP
24 constant CWD
32 constant FDS
40 constant NFDS
48 constant BYTES

;package
