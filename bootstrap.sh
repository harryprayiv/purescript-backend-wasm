#!/usr/bin/env bash
# One-command provisioning for a fresh clone. Idempotent: safe to re-run; each step is a
# no-op or cheap once it has run. The artifacts it produces (node_modules, output/, lib/,
# runtime/runtime.wasm) are all gitignored, so a fresh clone needs this once before the
# benchmarks, the tests, or any `purs-wasm build` will work.
#
# Run inside the dev toolchain:
#   nix develop -c ./bootstrap.sh        # one-shot
#   ./bootstrap.sh                       # from within an existing `nix develop`
set -euo pipefail
cd "$(dirname "$0")"

# 0. Fail fast with a clear message if the toolchain is not on PATH (i.e. you are not in the
#    dev shell). This is the one prerequisite the script cannot install for you.
for tool in pnpm spago purs node; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "FAIL: '$tool' is not on PATH. Enter the dev shell first:  nix develop" >&2
    exit 1
  }
done

# 1. Node / FFI deps (binaryen, cbor). Populates binaryen/node_modules, whose wasm-as and
#    wasm-merge binaries steps 3 and 4 invoke.
echo "==> [1/4] pnpm install"
pnpm install

# 2. Compile the workspace -> output/. Produces the CLI (PursWasm.CLI.Main) the bench and
#    tests invoke, and the installer (UlibTooling.Main) step 3 runs; also fetches the spago
#    package set into .spago/, which step 3 reads.
echo "==> [2/4] spago build"
spago build

# 3. Build + install the ulib shadow library into ./lib (gitignored). The compiler resolves
#    shadowed modules (Data.Foldable, Data.Array, ...) as merged wasm providers from here, so
#    bundles come out self-contained (no JS-loader fallback). Skips if ./lib is populated.
echo "==> [3/4] ulib lib"
node ulib-tooling/index.dev.js install

# 4. Assemble the shared runtime (runtime/runtime.wasm, gitignored) from runtime.wat so
#    wasm-merge has it to link into every bundle.
echo "==> [4/4] runtime.wasm"
( cd compiler && npm run build:runtime )

echo ""
echo "OK: provisioned. Try:  (cd bench && npm run base)"