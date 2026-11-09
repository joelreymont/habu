\ spawn-abi.f - the request record spawn-fds reads, shared by its engine bodies
\ (habu1.f BSPAWNFDS, kernel-x64.f SPAWN-FDS-ROW) and the code that builds one.
package SPAWN-ABI
public

\ One cell each. PATH, CWD: NUL-terminated paths; CWD 0 stays. ARGV, ENVP:
\ NULL-terminated arrays of NUL-terminated strings. FDS: NFDS cells, the child's
\ fd i is the parent's fd FDS[i], -1 leaves it closed; child fds at or above
\ NFDS are closed.
0 constant PATH
8 constant ARGV
16 constant ENVP
24 constant CWD
32 constant FDS
40 constant NFDS
48 constant BYTES

;package
