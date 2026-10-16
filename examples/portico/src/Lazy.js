export const rendered = function(count) {
  window.lazyRenders = (window.lazyRenders || 0) + 1;
  return count;
};
