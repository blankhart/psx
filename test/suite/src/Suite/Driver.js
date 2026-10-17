// Playwright and esbuild, as little of them as the driver needs.

import { gzipSync } from 'node:zlib';
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import { build } from 'esbuild';
import { chromium } from 'playwright-core';

const aff = (thunk) => (onError, onSuccess) => {
  thunk().then(onSuccess, onError);
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess();
};

const selector = (testId) => `[data-testid=${JSON.stringify(testId)}]`;

export const bundleImpl = (module) => (outfile) =>
  aff(() =>
    build({
      stdin: {
        contents: `import { main } from ${JSON.stringify(module)};\nmain(window.psxFixture)();\n`,
        resolveDir: process.cwd(),
      },
      bundle: true,
      minify: true,
      format: 'iife',
      platform: 'browser',
      target: 'es2020',
      legalComments: 'none',
      logLevel: 'silent',
      outfile,
    }).then(() => {})
  );

export const launchImpl = (executablePath) =>
  aff(() => chromium.launch(executablePath === '' ? {} : { executablePath }));

export const closeImpl = (browser) => aff(() => browser.close());

export const openImpl = (browser) => (file) =>
  aff(async () => {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', (error) => errors.push(error.message));
    page.on('console', (message) => {
      if (message.type() === 'error') errors.push(message.text());
    });
    page.psxErrors = errors;
    await page.goto(pathToFileURL(file).href);
    return page;
  });

export const closePageImpl = (page) => aff(() => page.close());

export const errors = (page) => () => page.psxErrors.slice();

export const clickImpl = (page) => (testId) => aff(() => page.click(selector(testId)));

export const clickNthImpl = (page) => (prefix) => (index) =>
  aff(() => page.dispatchEvent(`[data-testid^=${JSON.stringify(prefix)}] >> nth=${index}`, 'click'));

export const fillImpl = (page) => (testId) => (text) =>
  aff(() => page.fill(selector(testId), text));

export const pressImpl = (page) => (testId) => (key) =>
  aff(() => page.press(selector(testId), key));

// One step, done inside the page and timed there, so the time leaves out
// Playwright's round trips. The clock starts just after a frame has been
// painted and stops at the last change the step makes to the DOM, with
// layout forced there, whether the framework makes it in the event handler
// or at a later frame. Each step waits for two frames and a task, as
// `settle` does, for its changes to arrive.
export const timedImpl = (page) => (kind) => (testId) => (text) =>
  aff(() =>
    page.evaluate(
      async ({ kind, testId, text }) => {
        const element =
          kind === 'clickNth'
            ? document.querySelectorAll(`[data-testid^=${JSON.stringify(testId)}]`)[Number(text)] ?? null
            : document.querySelector(`[data-testid=${JSON.stringify(testId)}]`);
        if (element === null) throw new Error(`no element has data-testid ${JSON.stringify(testId)}`);
        const frame = () => new Promise((resolve) => requestAnimationFrame(resolve));
        const task = () => new Promise((resolve) => setTimeout(resolve));
        await frame();
        await task();
        let last = null;
        const observer = new MutationObserver(() => {
          void document.body.offsetHeight;
          last = performance.now();
        });
        observer.observe(document.body, {
          subtree: true,
          childList: true,
          attributes: true,
          characterData: true,
        });
        const start = performance.now();
        if (kind === 'click' || kind === 'clickNth') {
          // SVG elements have no click().
          element.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, view: window }));
        } else if (kind === 'type') {
          element.value = text;
          element.dispatchEvent(new Event('input', { bubbles: true }));
        } else {
          element.dispatchEvent(new KeyboardEvent('keydown', { key: text, bubbles: true }));
          element.dispatchEvent(new KeyboardEvent('keyup', { key: text, bubbles: true }));
        }
        void document.body.offsetHeight;
        const handled = performance.now();
        await frame();
        await frame();
        await task();
        observer.disconnect();
        return (last === null ? handled : Math.max(handled, last)) - start;
      },
      { kind, testId, text }
    )
  );

// Frameworks render at the next animation frame, or soon after, so a step is
// followed by two frames and a task before anything looks at the DOM.
export const settleImpl = (page) =>
  aff(() =>
    page.evaluate(
      () =>
        new Promise((resolve) =>
          requestAnimationFrame(() => requestAnimationFrame(() => setTimeout(resolve)))
        )
    )
  );

// The children of the page's root, as plain data: comments left out, a form
// control's live value and checked state in place of those attributes, and
// an element in the SVG namespace named `svg:<name>`, so that one created in
// the wrong namespace does not pass for it.
export const snapshotImpl = (page) =>
  aff(() =>
    page.evaluate(() => {
      const controls = new Set(['INPUT', 'TEXTAREA', 'SELECT']);
      const checkable = new Set(['checkbox', 'radio']);
      const svg = 'http://www.w3.org/2000/svg';
      const read = (node) => {
        if (node.nodeType === Node.TEXT_NODE) return { text: node.data };
        if (node.nodeType !== Node.ELEMENT_NODE) return null;
        const tag = node.namespaceURI === svg ? `svg:${node.localName}` : node.localName;
        let attributes = [...node.attributes].map((a) => [a.name, a.value]);
        if (controls.has(node.tagName)) {
          attributes = attributes.filter(([name]) => name !== 'value' && name !== 'checked');
          if (checkable.has(node.type)) {
            if (node.checked) attributes.push(['checked', '']);
          } else {
            attributes.push(['value', node.value]);
          }
        }
        return {
          tag,
          attributes,
          children: [...node.childNodes].map(read).filter((n) => n !== null),
        };
      };
      return [...document.getElementById('root').childNodes]
        .map(read)
        .filter((n) => n !== null);
    })
  );

export const toNodes = (element) => (text) => (tuple) => (nodes) => {
  const convert = (node) =>
    'text' in node
      ? text(node.text)
      : element(node.tag)(node.attributes.map(([n, v]) => tuple(n)(v)))(
          node.children.map(convert)
        );
  return nodes.map(convert);
};

export const now = () => performance.now();

export const gzipSize = (file) => () => gzipSync(readFileSync(file), { level: 9 }).length;
