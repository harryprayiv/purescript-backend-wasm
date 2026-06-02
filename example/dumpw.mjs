import binaryen from "binaryen";
import { readFileSync } from "fs";
const wat = binaryen.readBinary(new Uint8Array(readFileSync(process.argv[2]))).emitText();
console.log("=== exports ===");
console.log((wat.match(/\(export [^\n]*/g)||[]).join("\n"));
