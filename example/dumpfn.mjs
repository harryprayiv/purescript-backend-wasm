import binaryen from "binaryen";
import { readFileSync } from "fs";
const wat = binaryen.readBinary(new Uint8Array(readFileSync(process.argv[2]))).emitText();
const want = process.argv[3];
const lines = wat.split("\n");
// type section
if (process.argv[4] === "types") { console.log(lines.filter(l=>l.startsWith(" (type ")).join("\n")); process.exit(0); }
let cap=false,buf=[];
for (const l of lines){
  if(!cap && l.startsWith(` (func ${want} `)) cap=true;
  else if(cap && l.startsWith(" (func ")) break;
  if(cap) buf.push(l);
}
console.log(buf.join("\n"));
