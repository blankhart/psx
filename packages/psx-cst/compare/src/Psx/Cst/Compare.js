// Structural equality of a PSX tree and a cst-parser tree. They have the
// same shape, except that PSX wraps cst-parser's recovered errors in
// `PsxRecovered`.
const unwrap = (x) =>
  x !== null && typeof x === "object" && x.constructor && x.constructor.name === "PsxRecovered" ? x.value0 : x;

const equal = (a, b) => {
  a = unwrap(a);
  b = unwrap(b);
  if (a === b) return true;
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  if (!Array.isArray(a) && a.constructor && b.constructor && a.constructor.name !== b.constructor.name) return false;
  const ka = Object.keys(a);
  const kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) if (!equal(a[k], b[k])) return false;
  return true;
};

export const sameTreeImpl = (a) => (b) => equal(a, b);
