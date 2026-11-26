# Encoding Standard data provenance

This directory pins the index files and `encodings.json` of the WHATWG
Encoding Standard as published at `https://encoding.spec.whatwg.org/`, whose
snapshot was commit `7d2e3e08e11f57ba1b080390b490d53648c6f69c`
(`https://encoding.spec.whatwg.org/commit-snapshots/7d2e3e08e11f57ba1b080390b490d53648c6f69c/`).
Every index file states `Date: 2024-09-18`. The generator
(`tools/encoding/generate.f`) reads every file here but `LICENSE`. The
iso-8859-8-i encoding uses `index-iso-8859-8.txt`. The
`index-iso-2022-jp-katakana.txt` index is not pinned: only an encoder reads it.
`LICENSE` is the Standard's own license file at that commit: the Standard is
under CC BY 4.0, and portions of it in source code under the BSD 3-Clause
License.

| File | Source | SHA-256 |
|---|---|---|
| `encodings.json` | `https://encoding.spec.whatwg.org/encodings.json` | `078212b3697f60b81225b6671bd9da2604497abff0aa8e96e4d46605c02ac9e7` |
| `index-ibm866.txt` | `https://encoding.spec.whatwg.org/index-ibm866.txt` | `1796f4955dbc5c94e3951a940e9250c819580ce5dd1c9ac396ef73a26e938264` |
| `index-iso-8859-2.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-2.txt` | `86a09a772c5ff8631dc47b8838f0b7751dc31d53fd2eb4262f0fc858e638468b` |
| `index-iso-8859-3.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-3.txt` | `d49c03f3e8cab4fbf861b4471ce121beb937dbd56dfd3106b997326c4b97da0d` |
| `index-iso-8859-4.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-4.txt` | `d93b8b6606e1a12ccf28418a629911b497cb7a354c8a740207992059c50917b3` |
| `index-iso-8859-5.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-5.txt` | `b42bf0bb8d49701efbd8c0fb2bc7330985ce84c576ac51efe86aeff6b47aea5d` |
| `index-iso-8859-6.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-6.txt` | `84b979a90c6a4fb2a2dffe37994647cd4836f918550b46d329b0d798f4b84d52` |
| `index-iso-8859-7.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-7.txt` | `fc1d4295d73dfd0dd6e1b2c2ee3de7a52c6e94ddb09358161d02a2adb08a5151` |
| `index-iso-8859-8.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-8.txt` | `25a8f4366e7935f2c5d59f34e59c400683a1df86546776e7d4e527ae7235cc9e` |
| `index-iso-8859-10.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-10.txt` | `6c1863fca2ad99eed23cc77cce075fcd3cac6bd7b50a584d3067456c2180f16e` |
| `index-iso-8859-13.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-13.txt` | `5c5fe48042ef633a88d1aeed72dd3a2e1fb5202b70c5b7696b53e76ac5e56752` |
| `index-iso-8859-14.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-14.txt` | `310ec047fbd33d8c02de309f0bc8562e453e7d873cc2093005a5d954fc8f0032` |
| `index-iso-8859-15.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-15.txt` | `1171d20bd7c98b5755e8802480d2799527a99847531284cd8d860ffda7cde449` |
| `index-iso-8859-16.txt` | `https://encoding.spec.whatwg.org/index-iso-8859-16.txt` | `e7bee177fdf60c5167b7c44351cd25f51caf9780889493c7855c0eb222a31660` |
| `index-koi8-r.txt` | `https://encoding.spec.whatwg.org/index-koi8-r.txt` | `482889b92f244a03fd33ceee13a248a5fd479a3d1633019cc3b21910b8824f52` |
| `index-koi8-u.txt` | `https://encoding.spec.whatwg.org/index-koi8-u.txt` | `3b8bfcaa2b7fb125e9790cc4964c4c9e51eee2c9c6e2a80ba7dfd8901546a2c6` |
| `index-macintosh.txt` | `https://encoding.spec.whatwg.org/index-macintosh.txt` | `5e2b0aa162f3032cf6c96a119f59f698da80c16d58014080a6bd246d7ade3c38` |
| `index-windows-874.txt` | `https://encoding.spec.whatwg.org/index-windows-874.txt` | `81b42398e8bef2acc004f38ed6ae2c4d162dfdc367e5ddfcb1cb752ac8814a1e` |
| `index-windows-1250.txt` | `https://encoding.spec.whatwg.org/index-windows-1250.txt` | `857f28be0681aae376f1671ab388d8e141cff9648a68803b53887bfdf950f464` |
| `index-windows-1251.txt` | `https://encoding.spec.whatwg.org/index-windows-1251.txt` | `bf29b6293d8b5f8011804c575b78a3b589a3fd6a4ace29ff788e3445157a13c0` |
| `index-windows-1252.txt` | `https://encoding.spec.whatwg.org/index-windows-1252.txt` | `2c5fa192d566f92b8b9e6e116255b1f94016b4ece5b9dacb3b98be70b8c60795` |
| `index-windows-1253.txt` | `https://encoding.spec.whatwg.org/index-windows-1253.txt` | `38390cd918ac2c78c47be62bd486598e7d129e0adceb140c8a7f0165ddbbf3f5` |
| `index-windows-1254.txt` | `https://encoding.spec.whatwg.org/index-windows-1254.txt` | `88b7acb81b616bec879521d371eee5766b9306eb050b814e4dce9468e0725822` |
| `index-windows-1255.txt` | `https://encoding.spec.whatwg.org/index-windows-1255.txt` | `4a3ca10dd8ef4ce06b929f6bf66d7ea8932ab3c06e07162b0f8a9536a692e6ac` |
| `index-windows-1256.txt` | `https://encoding.spec.whatwg.org/index-windows-1256.txt` | `a3e9e6fcf4c3eda90275c791d5d7dad08700374ebe67b17cabdfff1e48f984c1` |
| `index-windows-1257.txt` | `https://encoding.spec.whatwg.org/index-windows-1257.txt` | `7e56420f3460befff856365b37733f9eabc1342e82be5999ce34c5eb518fddda` |
| `index-windows-1258.txt` | `https://encoding.spec.whatwg.org/index-windows-1258.txt` | `2236cdac45f5970629881d09908d13af96965f1c67617160734cbefb54aa3176` |
| `index-x-mac-cyrillic.txt` | `https://encoding.spec.whatwg.org/index-x-mac-cyrillic.txt` | `a01be0035d0cfbb8da0af780eeb6e7c6169ab26d748d03d15ccd4cd969c5b786` |
| `index-gb18030.txt` | `https://encoding.spec.whatwg.org/index-gb18030.txt` | `746b3c55f1a8ec4b90b451f384437a17fd37cd51cd668456a28071a758d10784` |
| `index-gb18030-ranges.txt` | `https://encoding.spec.whatwg.org/index-gb18030-ranges.txt` | `874c6b6f6f74cf7d427ad228d5b41ddd9354fffd92a2259bf429f86e6baa7a1e` |
| `index-big5.txt` | `https://encoding.spec.whatwg.org/index-big5.txt` | `08e24270c8e95d998c994c03f907e972480dc01f58743e078654cc466203c8ff` |
| `index-jis0208.txt` | `https://encoding.spec.whatwg.org/index-jis0208.txt` | `341dcde7e8b984e9c7bbf5ed75c8da7c6087d47083a1a2b3ed558bfd5bef9468` |
| `index-jis0212.txt` | `https://encoding.spec.whatwg.org/index-jis0212.txt` | `6ac698a473822b282ff4f4d4f17873b970612ef9bd232e03941854c09b67c7ee` |
| `index-euc-kr.txt` | `https://encoding.spec.whatwg.org/index-euc-kr.txt` | `89af20dd867c84cefb710b1790229786cfef2bf11916361a210d81b90381e267` |
| `LICENSE` | `https://raw.githubusercontent.com/whatwg/encoding/7d2e3e08e11f57ba1b080390b490d53648c6f69c/LICENSE` | `85dc6f5ccb57a6fe8c33d158f9fc8fc7ee5655a5d3db2cdd131c6a3d0f48a864` |

The generator verifies each input's digest before parsing it and writes
`src/lib/encoding/data.f` atomically.
