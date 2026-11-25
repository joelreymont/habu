\ exit-status.f - the status a process exits with, each named once.
\ A status from 64 to 78 means what sysexits(3) says; Habu's own reasons are 79
\ and above.

package EXIT-STATUS
public
0 constant SUCCESS              \ the process did what it was asked
1 constant FAILURE              \ a check the process ran failed: a test, a lint finding, a tool's comparison or child
64 constant USAGE               \ the command line is wrong
65 constant DATAERR             \ an input file is not in the form its reader expects
66 constant NOINPUT             \ an input the command line names does not exist
69 constant UNAVAILABLE         \ a program or service the process needs is missing or did not answer
70 constant REJECTED            \ Habu rejected the program's source
71 constant SYSTEM              \ the system gave no resource: memory, a mapping, a descriptor, a close
74 constant IOERR               \ a file or stream could not be read, written or searched
79 constant TASK-COMPILE        \ a definition or allot ran on a task's or callback's thread
81 constant BL-RANGE            \ the mapped code region is out of BL reach from __text
82 constant DISPATCH-UNSET      \ the engine has no entry
83 constant SEAL-VIOLATION      \ a kernel dictionary, code or replay primitive met an argument or state its contract forbids
84 constant SEAL-WORDLIST       \ a write into a wordlist the seal protects, or one past the wordlists it can protect
85 constant UNCAUGHT            \ a throw of a code outside 1..255 that nothing caught
86 constant ENCODE              \ the assembler cannot encode an instruction a builder asked for
87 constant IMAGE               \ the image writer cannot lay the image out
88 constant IMAGE-BOUNDS        \ a write past the image buffer
89 constant INTERNAL            \ a defect in Habu: an invariant failed, or a table no program fills is full
90 constant FULL                \ the program filled a fixed space
96 constant ADDRESS-CELL        \ an address-cell table is malformed: header, offset or kind
99 constant PROF-LIMIT          \ the profiler reached its sample limit and printed its report
102 constant STACK-BOUNDS       \ a physical VM stack access exceeds its active allocation
\ A callback thunk has no catch frame above it: a rejected callback, or an FFI call
\ on a context another thread owns, writes its record to fd 2 and exits 106.
106 constant CALLBACK           \ a C callback or FFI call found its region or slot unusable
107 constant POLICY             \ a sealed design named a word outside its admitted vocabulary
110 constant MEMORY-FAULT       \ a load or store by Habu code faulted
111 constant STREAM-GROW        \ a Wasm run's OUT or ERR could not grow: the memory rejected the pages
124 constant TIMEOUT            \ a child outlived its deadline, as timeout(1) exits
127 constant EXEC               \ a spawned program could not be run, as sh(1) exits
;package
