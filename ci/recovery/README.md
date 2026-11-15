# T0 bootstrap checkpoint

`hb` and `hb.map` are the clean Linux ARM64 T0 host built from
`f986e3046ea2f4a66332e26278bcc8eb1dc0552f`. `seed.sha256` pins both files.
The host was recovered through checked upstream stages under QEMU 10.0.13;
it is a bootstrap input, not a qualified current Habu release.

The workflow runs it on native ARM and builds the next two pinned source
trees. These historical builds use the normal kernel builder and engine
body in separate processes because their combined build leaves stale scope
state. The helpers preserve kernel format and checksum checks and select T0.
Each build phase retains the repository's five-minute limit.

The final host, map, source pins, kernel objects and diagnostics are retained
as an artifact without compression. No release is installed or deployed.
Later sealed builders must use the ordinary build command; `engine.f` is
only for these early, unsealed stages.
