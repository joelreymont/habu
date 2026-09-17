\ repl-term.f - Linux/x86-64 terminal constants for the baked Habu REPL.
\ Linux's termios is one struct for every architecture: four 32-bit flag words,
\ then c_line, then c_cc, so the offsets read the same as the aarch64 seam's.

$5401 constant HBR-TIO-GET
$5402 constant HBR-TIO-SET
$B constant HBR-RAWMASK
12 constant HBR-LFLAG-OFF
23 constant HBR-VMIN-OFF
22 constant HBR-VTIME-OFF
-1 constant HBR-LFLAG-32?
