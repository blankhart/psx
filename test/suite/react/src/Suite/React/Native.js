import { Just } from '../Data.Maybe/index.js';
export const attribute = name => value => [name, value];
export const boolean = attribute;
export const props = attrs => children => {
  const result = Object.fromEntries(attrs);
  if (children.length) result.children = children;
  if (result.checked !== undefined || result.value !== undefined) result.onChange ??= () => {};
  return result;
};
export const onClick = effect => ['onClick', () => effect()];
export const onChange = effect => ['onChange', () => effect()];
export const onInput = f => ['onInput', e => f(e.currentTarget.value)()];
export const onKeyDown = f => ['onKeyDown', e => { const msg = f(e.key); if (msg instanceof Just) msg.value0(); }];
