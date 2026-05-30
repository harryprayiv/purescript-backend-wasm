import { readFileSync } from "node:fs";

export const readFixture = (path) => () => readFileSync(path, "utf8");

export const instantiate = (bytes) => () => {
  const module = new WebAssembly.Module(bytes);
  return new WebAssembly.Instance(module, {});
};

export const callI32x0 = (inst) => (name) => () => inst.exports[name]();

export const callI32x1 = (inst) => (name) => (a) => () => inst.exports[name](a);

export const callI32x2 = (inst) => (name) => (a) => (b) => () =>
  inst.exports[name](a, b);
