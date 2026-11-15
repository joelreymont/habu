# T0 bootstrap checkpoint

The workflow resumes native ARM run `43726034664` from its verified clean
`a89f92b0b52fd5e2efb9618921afe02dc738f245` compiler. The artifact ID,
source tree, executable and map hashes are pinned; `checkpoint.sha256`
contains the expected file hashes. Artifact retention is 30 days.

Source provisioning checks each Git tree ID. Every remaining stage builds
an unchanged upstream revision with the preceding successful compiler.

Every build uses the ordinary checked Habu builder with a five-minute
deadline. The tier prelude selects T0 until the host removes that switch and
retains T0 alone. A compiler refusal stops the chain.

Artifacts retain the latest successful compiler and map, every source pin
and output hash, and all diagnostics, without compression. Current-source
convergence and native tests remain required before qualification.

The checked-in `hb`, map and `seed.sha256` preserve the initial clean
`f986e3046ea2f4a66332e26278bcc8eb1dc0552f` seed. The initial workflow
revision records its checked route to the native checkpoint.
