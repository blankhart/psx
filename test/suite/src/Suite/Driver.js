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

export const fillImpl = (page) => (testId) => (text) =>
  aff(() => page.fill(selector(testId), text));

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

// The children of the page's root, as plain data: comments left out, and a
// form control's live value and checked state in place of those attributes.
export const snapshotImpl = (page) =>
  aff(() =>
    page.evaluate(() => {
      const controls = new Set(['INPUT', 'TEXTAREA', 'SELECT']);
      const checkable = new Set(['checkbox', 'radio']);
      const read = (node) => {
        if (node.nodeType === Node.TEXT_NODE) return { text: node.data };
        if (node.nodeType !== Node.ELEMENT_NODE) return null;
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
          tag: node.localName,
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
