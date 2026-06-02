// Verify Example.Main's `eval` and `printExpr`, compiled to wasm by `bin`.
//
//   1. build ps:    spago build -p example --output example/output
//   2. build wasm:  node ../bin/index.dev.js build -I ./example/output -O ./example/output-wasm -e Example.MainCheck
//   3. run:         node run-check.mjs
//
// The module is self-contained (the runtime is merged in via wasm-merge), so it
// instantiates with no imports. Wasm GC requires a recent runtime (Node 22+).
//
// The export ABI is i32-only, so `Example.MainCheck` exposes nullary `Int`
// entry points: `evalTest*` return the evaluated number, and `printTest*` compare
// `printExpr`'s output against the expected rendering inside wasm (1 = match).

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const wasmPath = fileURLToPath(
  new URL("./output-wasm/Example.MainCheck/index.wasm", import.meta.url),
);
const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {});
const x = instance.exports;

console.log("wasm exports:", Object.keys(x).sort().join(", "));
console.log();

let ok = true;
const check = (label, got, want) => {
  const pass = got === want;
  ok = ok && pass;
  console.log(`${pass ? "✓" : "✗"} ${label}: got ${got}, want ${want}`);
};

// eval — testExpr1 = 1 + 2 * (-3) = -5 ; testExpr2 = 3*5 - 2 + 4*(2+3) = 33
console.log("eval:");
check("eval testExpr1  (1 + 2 * (-3))", x.evalTest1(), -5);
check("eval testExpr2  (3*5 - 2 + 4*(2+3))", x.evalTest2(), 33);

// printExpr — compared to the expected string inside wasm (1 = exact match)
console.log("\nprintExpr (1 = matches expected string):");
check('printExpr testExpr1 == "1 + 2 * -3"', x.printTest1(), 1);
check('printExpr testExpr2 == "3 * 5 - 2 + 4 * (2 + 3)"', x.printTest2(), 1);

console.log();
console.log(ok ? "ALL CHECKS PASSED" : "SOME CHECKS FAILED");
process.exit(ok ? 0 : 1);
