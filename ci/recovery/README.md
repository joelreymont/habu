# T0 bootstrap checkpoint

`hb` and `hb.map` are the clean Linux ARM64 T0 host built from
`f986e3046ea2f4a66332e26278bcc8eb1dc0552f`. `seed.sha256` pins both files.
The host is a bootstrap input, not a qualified current Habu release.

The workflow provisions pinned source trees and verifies their Git tree IDs.
The two patch files stage exact upstream readers and manifests before their
callers. Those intermediate hosts build the next stage, never themselves.

Each build step invokes the checked Habu builder with a five-minute deadline.
Only the first two, unsealed stages use separate kernel and engine processes.
Every later stage uses the ordinary build command. The tier prelude selects
T0 until the host removes that switch and retains T0 alone.

Artifacts retain the latest successful compiler and map, every source pin and
output hash, and all build diagnostics, without compression. A failed build
stops the chain. Current-source convergence and native tests remain required
before a recovered compiler is qualified.
