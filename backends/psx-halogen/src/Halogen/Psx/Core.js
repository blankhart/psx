// The `value` of an event's target, as Halogen's own value handlers read it.
export const targetValue = function (event) {
  const target = event.target;
  return target != null && typeof target.value === "string" ? target.value : "";
};
