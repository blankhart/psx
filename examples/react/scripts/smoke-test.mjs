import assert from 'node:assert/strict';
import { chromium } from 'playwright-core';
import { resolve, basename } from 'node:path';
const browser = await chromium.launch({ executablePath: process.env.CHROME_PATH || '/usr/bin/chromium' });
try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.route('http://psx.test/**', route => {
    const name = basename(new URL(route.request().url()).pathname);
    return route.fulfill({ path: resolve('examples/react/dist', name), contentType: name.endsWith('.js') ? 'text/javascript' : 'text/html' });
  });
  await page.goto('http://psx.test/Counter.html');
  const original = await page.locator('[data-testid=row-a]').elementHandle();
  await page.locator('[data-testid=increment]').click();
  await page.locator('[data-testid=count]').filter({ hasText: '1' }).waitFor();
  assert.equal(await page.locator('[data-testid=controlled]').inputValue(), '1');
  assert.equal(await original.evaluate(node => node === document.querySelector('[data-testid=row-a]')), true);
  assert.deepEqual(await page.locator('li').allTextContents(), ['b', 'a']);
  assert.equal(await page.locator('circle').evaluate(node => node.namespaceURI), 'http://www.w3.org/2000/svg');
  await page.goto('http://psx.test/HelloWorld.html');
  await page.getByText('Hello, React!').waitFor();
  assert.deepEqual(errors, []);
  console.log('React PSX examples passed');
} finally { await browser.close(); }
