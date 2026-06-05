import { execFileSync } from "node:child_process";

// Run a tool synchronously, inheriting stdio; throws on a non-zero exit.
export const execFileImpl = (cmd) => (args) => () => {
  execFileSync(cmd, args, { stdio: "inherit" });
};

// The distinct host-import module names a wasm binary declares (excluding the
// internal "rt" runtime), i.e. the user `foreign import` modules a JS loader must
// satisfy (ADR 0014). Reads the import section via WebAssembly.Module.imports.
export const importModulesImpl = (bytes) => () => {
  const mod = new WebAssembly.Module(bytes);
  const set = new Set();
  for (const { module } of WebAssembly.Module.imports(mod)) {
    if (module !== "rt") set.add(module);
  }
  return Array.from(set);
};

// The dotted import module names of a corefn.json, via a cheap transient `JSON.parse`
// (the tree is GC'd, only the small import-name list is kept) — for file-level
// reachability pruning before the expensive full decode. A real app's output dir holds
// far more modules than one entry needs; decoding them all OOMs the build.
export const corefnImportsImpl = (json) => {
  try {
    const j = JSON.parse(json);
    return (j.imports || []).map((i) => i.moduleName.join("."));
  } catch {
    return [];
  }
};

// Module name -> its `.purs` source path, parsed from spago's `cache-db.json` (ADR 0016).
// Each entry maps source files (`.purs`/`.js`) to [timestamp, hash]; we take the `.purs`.
// Paths are relative to the build's working directory. Returns a plain object (= Object).
export const cacheDbSourcesImpl = (json) => {
  const out = {};
  try {
    const db = JSON.parse(json);
    for (const mod of Object.keys(db)) {
      const purs = Object.keys(db[mod]).find((k) => k.endsWith(".purs"));
      if (purs) out[mod] = purs;
    }
  } catch {
    /* no/!valid cache-db → no source reconstruction (externs-only fallback) */
  }
  return out;
};
