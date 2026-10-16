// Loads every built example in headless Chrome and exercises it. Run
// `spago build` and `node scripts/build-examples.mjs` first.
//
// Chrome is found through CHROME_PATH, or in the usual places.

import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { basename, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import puppeteer from 'puppeteer-core';

const dist = fileURLToPath(new URL('../examples/dist/', import.meta.url));

const executablePath = [
  process.env.CHROME_PATH,
  '/usr/bin/google-chrome',
  '/usr/bin/google-chrome-stable',
  '/usr/bin/chromium',
  '/usr/bin/chromium-browser',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
].find((path) => path && existsSync(path));
if (!executablePath) throw new Error('Chrome not found. Set CHROME_PATH.');

// Serves examples/dist by file name alone, and RouterHistory.html for anything
// else, which is the fallback that History API routing needs from a server.
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css' };
const server = createServer((request, response) => {
  const name = basename(new URL(request.url, 'http://localhost').pathname);
  const file = existsSync(dist + name) && types[extname(name)] ? name : 'RouterHistory.html';
  response.writeHead(200, { 'Content-Type': types[extname(file)] });
  response.end(readFileSync(dist + file));
});
await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
const origin = `http://127.0.0.1:${server.address().port}`;

const browser = await puppeteer.launch({ executablePath, args: ['--no-sandbox'] });

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const text = (page, selector) => page.$eval(selector, (elm) => elm.textContent);
const count = (page, selector) => page.$$eval(selector, (elms) => elms.length);
// Portico renders at the next animation frame, and starts effects after that,
// so an interaction is followed by a couple of frames before looking. A
// render in a view transition happens when the transition calls back, which
// is later still.
const settle = (page) =>
  page.evaluate(async () => {
    const frames = () =>
      new Promise((resolve) =>
        requestAnimationFrame(() => requestAnimationFrame(() => setTimeout(resolve)))
      );
    await frames();
    // Until no transition is pending, and none started in the last frames.
    let seen = -1;
    while (seen !== window.viewTransitions || window.pendingTransitions.size > 0) {
      seen = window.viewTransitions;
      await Promise.allSettled([...window.pendingTransitions]);
      await frames();
    }
  });
const click = async (page, selector) => {
  await page.click(selector);
  await settle(page);
};
const clickText = async (page, selector, label) => {
  await page.$$eval(
    selector,
    (elms, label) => elms.find((elm) => elm.textContent === label).click(),
    label
  );
  await settle(page);
};

// What the Redux DevTools monitor sends when an action is picked.
const jump = async (page, actionId) => {
  await page.evaluate(
    (actionId) =>
      window.devtoolsListener({
        type: 'DISPATCH',
        payload: { type: 'JUMP_TO_ACTION', actionId },
        state: 'ignored',
      }),
    actionId
  );
  await settle(page);
};

const tests = {
  HelloWorld: async (page) => {
    assert.equal(await text(page, 'main'), 'Hello, world!');
  },

  Counter: async (page) => {
    await clickText(page, 'button', '+');
    await clickText(page, 'button', '+');
    await clickText(page, 'button', '-');
    assert.equal(await text(page, 'main'), '-1+');
  },

  Counters: async (page) => {
    await clickText(page, 'button', 'Add');
    await clickText(page, 'button', 'Add');
    await clickText(page, 'button', '+');
    assert.equal(await text(page, 'main'), 'Add-1+Remove-0+Remove');

    // What `Portico.Devtools` told the stand-in for the Redux DevTools extension.
    assert.deepEqual(await page.evaluate(() => window.devtoolsLog), [
      ['init', []],
      ['send', 'Add', [0]],
      ['send', 'Add', [0, 0]],
      ['send', 'CounterMsg:0:Increment', [1, 0]],
    ]);
    // Time travel replays messages from the initial model.
    await jump(page, 1);
    assert.equal(await text(page, 'main'), 'Add-0+Remove');
    await jump(page, 3);
    assert.equal(await text(page, 'main'), 'Add-1+Remove-0+Remove');
    await jump(page, 2);
    assert.equal(await text(page, 'main'), 'Add-0+Remove-0+Remove');

    // A message after a jump back continues from there, and the monitor is
    // told the new history.
    await page.evaluate(() => (window.devtoolsLog = []));
    await clickText(page, 'button', 'Remove');
    assert.equal(await text(page, 'main'), 'Add-0+Remove');
    assert.deepEqual(await page.evaluate(() => window.devtoolsLog), [
      ['init', []],
      ['send', 'Add', [0]],
      ['send', 'Add', [0, 0]],
      ['send', 'Remove:0', [0]],
    ]);
  },

  Form: async (page) => {
    // `onInput` reads the value of a `<select>`.
    await page.select('#flavor', 'chocolate');
    await settle(page);
    assert.equal(await text(page, '#chosen'), 'chocolate');

    // Properties that leave the trait list are unset.
    const panel = () =>
      page.$eval('#panel', (elm) => [elm.className, elm.querySelector('button').disabled]);
    assert.deepEqual(await panel(), ['', true]);
    await click(page, '#open');
    assert.deepEqual(await panel(), ['open', false]);
    await click(page, '#open');
    assert.deepEqual(await panel(), ['', true]);

    // A message sent from inside a patch, by an element the patch removes.
    await page.type('#note', 'Done!');
    await settle(page);
    assert.equal(await count(page, '#note'), 0);
    assert.equal(await text(page, '#blurs'), '1');

    // An invalid form is not submitted; a valid one sends its fields.
    await click(page, '#order button');
    assert.equal(await text(page, '#submitted'), '');
    await page.type('#order input', 'Ada');
    await page.select('#order select', 'l');
    await click(page, '#order button');
    assert.equal(await text(page, '#submitted'), 'name=Ada&size=l&action=save');
  },

  Events: async (page) => {
    // The input is emptied at the next frame, so the test waits for it as a
    // person would.
    for (const [word, key] of [['milk', 'Enter'], ['eggs', 'Escape'], ['tea', 'Enter']]) {
      await page.type('#draft', word);
      await page.keyboard.press(key);
      await settle(page);
    }
    assert.equal(await text(page, '#items'), 'milktea');

    // Mapped traits.
    await page.focus('#pad');
    for (const key of ['ArrowUp', 'ArrowUp', 'ArrowRight', 'Shift']) {
      await page.keyboard.press(key);
    }
    await settle(page);
    assert.equal(await text(page, '#pad'), '1,-2');

    // Listener options.
    await click(page, '#inner');
    await click(page, '#passive');
    assert.equal(await page.evaluate(() => location.hash), '#passive');
    await click(page, '#active');
    assert.equal(await page.evaluate(() => location.hash), '#passive');
    assert.equal(await text(page, '#clicks'), 'outer inner passive active');
  },

  Dice: async (page) => {
    assert.match(await text(page, 'main'), /^Nothing/);
    await clickText(page, 'button', 'Roll');
    const rolled = await text(page, 'main');
    assert.match(rolled, /^\(Just [1-6]\)/);

    // Time travel works on a model of constructors, which JSON cannot hold.
    await jump(page, 0);
    assert.equal(await text(page, 'main'), 'NothingRoll');
    await jump(page, 2);
    assert.equal(await text(page, 'main'), rolled);
  },

  Prompt: async (page) => {
    assert.equal(await text(page, 'main div'), 'Hello, (Just "Ada")!');
  },

  Affjax: async (page) => {
    // `P.focus` in `init`.
    assert.equal(await page.evaluate(() => document.activeElement.tagName), 'INPUT');
    assert.equal(await text(page, 'main div'), 'Not Fetched...');
  },

  AnimatedList: async (page) => {
    for (let i = 0; i < 6; i++) await clickText(page, 'button', 'Add');
    assert.equal(await count(page, '.list > .bounce-enter-active'), 6);
    await sleep(800);
    assert.equal(await count(page, '.list > *'), 6);
    assert.equal(await count(page, '.list > [class]:not([class=""])'), 0);

    // Removal waits for the leave animation.
    await click(page, '.list > :nth-child(2)');
    assert.equal(await count(page, '.list > *'), 6);
    assert.equal(await count(page, '.list > .bounce-leave-active'), 1);
    await sleep(800);
    assert.equal(await text(page, '.list'), '13456');

    // Shuffling until the order changes moves the same keyed elements around.
    await page.$$eval('.list > *', (elms) => elms.forEach((elm, i) => (elm.dataset.was = i)));
    let moved = false;
    for (let i = 0; i < 20 && !moved; i++) {
      await clickText(page, 'button', 'Shuffle');
      moved = (await text(page, '.list')) !== '13456';
    }
    assert.ok(moved, 'shuffle never changed the order');
    await page.waitForSelector('.list > .bounce-move');
    await sleep(800);
    assert.equal(await count(page, '.list > [class]:not([class=""])'), 0);
    assert.equal(await count(page, '.list > [data-was]'), 5);
    assert.equal([...(await text(page, '.list'))].sort().join(''), '13456');

    // Adding, removing and reordering close together leaves a consistent list.
    await clickText(page, 'button', 'Add');
    await click(page, '.list > :nth-child(1)');
    await clickText(page, 'button', 'Shuffle');
    await clickText(page, 'button', 'Add');
    await sleep(1200);
    assert.equal(await count(page, '.list > *'), 6);
    assert.equal(await count(page, '.list > [class]:not([class=""])'), 0);
    assert.equal(new Set(await text(page, '.list')).size, 6);

    // With reduced motion requested, nothing animates.
    await page.emulateMediaFeatures([{ name: 'prefers-reduced-motion', value: 'reduce' }]);
    await clickText(page, 'button', 'Add');
    assert.equal(await count(page, '.list > [class]:not([class=""])'), 0);
    await click(page, '.list > :nth-child(1)');
    assert.equal(await count(page, '.list > *'), 6);
  },

  SvgTextPath: async (page) => {
    const info = () =>
      page.evaluate(() => {
        const textPath = document.querySelector('textPath');
        return (
          textPath && {
            namespace: textPath.namespaceURI,
            href: textPath.getAttribute('href'),
            length: textPath.getComputedTextLength(),
          }
        );
      });
    const shown = await info();
    assert.equal(shown.namespace, 'http://www.w3.org/2000/svg');
    assert.equal(shown.href, '#curve');
    assert.ok(shown.length > 100, 'text is not laid out on the path');

    // Let the text finish fading in: hidden while still at opacity 0, it has
    // nothing to fade out and goes at once.
    await sleep(800);
    await clickText(page, 'button', 'Hide');
    assert.equal(await count(page, 'svg text.fade-leave-active'), 1);
    await sleep(800);
    assert.equal(await info(), null);

    await clickText(page, 'button', 'Show');
    assert.equal(await count(page, 'svg text.fade-enter-active'), 1);
    await sleep(800);
    assert.equal((await info()).namespace, 'http://www.w3.org/2000/svg');
    assert.equal(await count(page, 'svg text[class]:not([class=""])'), 0);
  },

  JsFrameworkBenchmark: async (page) => {
    const label = (n) => text(page, `tbody tr:nth-child(${n}) td:nth-child(2) a`);
    const id = (n) => text(page, `tbody tr:nth-child(${n}) td:nth-child(1)`);

    await click(page, '#run');
    assert.equal(await count(page, 'tbody tr'), 1000);
    await click(page, '#add');
    assert.equal(await count(page, 'tbody tr'), 2000);
    assert.equal(await id(2000), '2000');
    await click(page, '#run');
    assert.equal(await count(page, 'tbody tr'), 1000);
    assert.equal(await id(1), '2001');

    // Rows are keyed and lazy: a swap moves the two elements, and an update
    // leaves the elements of untouched rows alone.
    await page.$$eval('tbody tr', (rows) => rows.forEach((row, i) => (row.dataset.was = i + 1)));
    const [second, last] = [await label(2), await label(999)];
    await click(page, '#swaprows');
    assert.equal(await label(2), last);
    assert.equal(await label(999), second);
    assert.equal(await page.$eval('tbody tr:nth-child(2)', (row) => row.dataset.was), '999');
    await click(page, '#swaprows');

    const first = await label(1);
    await click(page, '#update');
    assert.equal(await label(1), first + ' !!!');
    assert.equal(await label(2), second);
    assert.equal(await label(11), (await label(11)).replace(/( !!!)?$/, ' !!!'));
    assert.equal(await count(page, 'tbody tr[data-was]'), 1000);

    await click(page, 'tbody tr:nth-child(5) td:nth-child(2) a');
    assert.equal(await count(page, 'tbody tr.danger'), 1);
    await click(page, 'tbody tr:nth-child(7) td:nth-child(2) a');
    assert.equal(await page.$eval('tbody tr.danger', (row) => row.dataset.was), '7');

    // The glyph has no size without the benchmark's stylesheet.
    await page.$eval('tbody tr:nth-child(5) td:nth-child(3) span', (span) => span.click());
    await settle(page);
    assert.equal(await count(page, 'tbody tr'), 999);
    assert.equal(await page.$eval('tbody tr:nth-child(5)', (row) => row.dataset.was), '6');

    await click(page, '#clear');
    assert.equal(await count(page, 'tbody tr'), 0);
  },

  Popover: async (page) => {
    const is = (selector, pseudo) =>
      page.$eval(selector, (elm, pseudo) => elm.matches(pseudo), pseudo);

    await click(page, '#menu-button');
    assert.equal(await is('#menu', ':popover-open'), true);
    await page.keyboard.press('Escape');
    assert.equal(await is('#menu', ':popover-open'), false);

    await click(page, '#open-dialog');
    assert.equal(await is('#dialog', ':modal'), true);
    await click(page, '#close-dialog');
    assert.equal(await is('#dialog', '[open]'), false);

    assert.equal(await page.$eval('#outside', (elm) => elm.inert), true);
    assert.equal(await page.$eval('#menu', (elm) => elm.dataset.kind), 'menu');
    assert.equal(
      await page.$eval('#progress', (elm) => getComputedStyle(elm).getPropertyValue('--progress')),
      '0.4'
    );
  },

  Lazy: async (page) => {
    const renders = () => page.evaluate(() => window.lazyRenders);
    assert.equal(await renders(), 1);

    // The view around it changes, the lazy view's argument does not.
    await click(page, '#other');
    await click(page, '#retag');
    assert.equal(await text(page, '#other'), 'Other 1');
    assert.equal(await renders(), 1);

    // The skipped subtree dispatches through the newest tagger.
    await click(page, '#inner');
    assert.equal(await text(page, '#last'), 'clicked under tag 1');

    await click(page, '#count');
    assert.equal(await renders(), 2);
    assert.equal(await text(page, '#inner'), '1');
  },

  Subscriptions: async (page) => {
    const ticks = async () => Number(await text(page, '#ticks'));
    await sleep(100);
    assert.equal(await ticks(), 0);

    // The timer runs while the model has it in `subscriptions`.
    await click(page, '#toggle');
    await sleep(200);
    assert.ok((await ticks()) > 0, 'the timer did not start');
    await click(page, '#toggle');
    const stopped = await ticks();
    await sleep(200);
    assert.equal(await ticks(), stopped);

    // Each keystroke's search stops the previous one.
    await page.type('#search', 'abc');
    await sleep(300);
    assert.equal(await text(page, '#results'), 'abc');
  },

  Embed: async (page) => {
    const shown = () => page.$$eval('.app', (elms) => elms.map((elm) => elm.textContent));
    const keydownListeners = async () => {
      const client = await page.createCDPSession();
      const { result } = await client.send('Runtime.evaluate', { expression: 'window' });
      const { listeners } = await client.send('DOMDebugger.getEventListeners', {
        objectId: result.objectId,
      });
      await client.detach();
      return listeners.filter((listener) => listener.type === 'keydown').length;
    };

    await page.keyboard.press('a');
    await settle(page);
    assert.deepEqual(await shown(), ['one: 1', 'two: 1']);
    assert.equal(await keydownListeners(), 2);

    // The page sends a message in through the handle.
    await page.evaluate(() => window.bumpTwo());
    await settle(page);
    assert.deepEqual(await shown(), ['one: 1', 'two: 11']);

    // Unmounting one leaves the other alone, and takes its listener with it.
    await page.evaluate(() => window.unmountOne());
    await page.keyboard.press('a');
    await settle(page);
    assert.deepEqual(await shown(), ['two: 12']);
    assert.deepEqual(
      await page.$eval('main > :first-child', (elm) => [elm.outerHTML, elm.childNodes.length]),
      ['<div class=""></div>', 0]
    );
    assert.equal(await keydownListeners(), 1);
  },

  WebSocket: async (page) => {
    // Whether the echo server answers is not this test's business.
    assert.match(await text(page, 'main'), /^Connection status: /);
    await page.type('input', '!');
    await settle(page);
    assert.equal(await page.$eval('input', (input) => input.value), 'Echo Me!');

    // The socket lives as long as the model wants it.
    assert.equal(await page.evaluate(() => window.sockets.length), 1);
    await click(page, '#connect');
    assert.equal(await page.evaluate(() => window.sockets[0].closed), true);
    assert.match(await text(page, 'main'), /^Connection status: Disconnected/);
    await click(page, '#connect');
    assert.equal(await page.evaluate(() => window.sockets.length), 2);
  },

  Router: async (page) => {
    await router(page, (url) => '#' + url);
  },

  RouterHistory: async (page) => {
    assert.equal(new URL(page.url()).pathname, '/');
    await router(page, (url) => url);

    // A cold load of a deep link, as a server with a fallback would serve it.
    await page.goto(origin + '/posts/2/edit');
    assert.equal(await text(page, '#description'), 'Description: edit post #2');
  },
};

// Shared by the two routing modes; `location` turns a route's URL into what
// the address bar should show after the origin.
const router = async (page, location) => {
  const address = () => page.evaluate(() => location.pathname + location.search + location.hash);
  const description = () => text(page, '#description');
  const base = new URL(page.url()).pathname;
  const shows = async (url) =>
    assert.equal(await address(), location(url) === url ? url : base + location(url));

  const routes = await count(page, '.route');
  assert.equal(routes, 6);
  assert.equal(await count(page, '.route.round-trips'), routes);

  const expected = [
    ['/', 'index'],
    ['/posts', 'all posts (unsorted)'],
    ['/posts?sortBy=id', 'all posts (sorting on id)'],
    ['/posts?sortBy=title', 'all posts (sorting on title)'],
    ['/posts/1/edit', 'edit post #1'],
    ['/posts/2/edit', 'edit post #2'],
  ];

  for (const control of ['a', 'button']) {
    for (let i = routes; i >= 1; i--) {
      await click(page, `.route:nth-of-type(${i}) ${control}`);
      const [url, described] = expected[i - 1];
      assert.equal(await description(), 'Description: ' + described);
      assert.equal(await text(page, '#url'), `Url: {${url}}`);
      await shows(url);
      assert.equal(
        await page.$eval('.route.active', (elm, i) => elm.matches(`:nth-of-type(${i})`), i),
        true
      );
      assert.equal(await count(page, '.route.active'), 1);
    }
  }

  // `navigate` tells the application without a synthetic `popstate`
  await page.evaluate(() => {
    window.pops = 0;
    window.addEventListener('popstate', () => window.pops++);
  });
  await click(page, '.route:nth-of-type(3) button');
  assert.equal(await description(), 'Description: all posts (sorting on id)');
  // (A change of fragment fires a real one, so only History mode can tell.)
  if (location('/') === '/') assert.equal(await page.evaluate(() => window.pops), 0);

  // Navigating to where we already are adds no history entry.
  const length = await page.evaluate(() => history.length);
  await click(page, '.route:nth-of-type(5) button');
  await click(page, '.route:nth-of-type(5) button');
  assert.equal(await page.evaluate(() => history.length), length + 1);

  // Route changes animate in a view transition, unless motion is reduced.
  const transitions = () => page.evaluate(() => window.viewTransitions);
  const before = await transitions();
  assert.ok(before > 0, 'no view transition was started');
  await page.emulateMediaFeatures([{ name: 'prefers-reduced-motion', value: 'reduce' }]);
  await click(page, '.route:nth-of-type(4) button');
  assert.equal(await description(), 'Description: all posts (sorting on title)');
  assert.equal(await transitions(), before);
  await page.emulateMediaFeatures([]);

  // With unsaved changes, the application refuses to follow links.
  await click(page, '#unsaved');
  await click(page, '.route:nth-of-type(2) a');
  assert.equal(await description(), 'Description: all posts (sorting on title)');
  assert.equal(await text(page, '#refused'), 'Refused: /posts');
  await shows('/posts?sortBy=title');
  await click(page, '#unsaved');

  await click(page, '.route:nth-of-type(5) a');
  await click(page, '#nonsense');
  assert.equal(await description(), 'Description: Error 404');
  assert.equal(await count(page, '.route.active'), 0);

  await page.goBack();
    await settle(page);
  assert.equal(await description(), 'Description: edit post #1');
  await page.goForward();
    await settle(page);
  assert.equal(await description(), 'Description: Error 404');
};

const overHttp = new Set(['RouterHistory']);
const only = process.argv.slice(2);
let failed = 0;

for (const [name, test] of Object.entries(tests)) {
  if (only.length && !only.includes(name)) continue;
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(error));
  page.on('dialog', (dialog) => dialog.accept('Ada'));
  await page.evaluateOnNewDocument(() => {
    // Records the sockets the WebSocket example opens and closes.
    window.sockets = [];
    const NativeWebSocket = window.WebSocket;
    window.WebSocket = class extends NativeWebSocket {
      constructor(...args) {
        super(...args);
        window.sockets.push(this);
      }
      close() {
        this.closed = true;
        super.close();
      }
    };
    // Counts the view transitions the Router example starts.
    window.viewTransitions = 0;
    window.pendingTransitions = new Set();
    const startViewTransition = document.startViewTransition;
    document.startViewTransition = function(...args) {
      window.viewTransitions++;
      const transition = startViewTransition.apply(this, args);
      const done = transition.finished.finally(() => window.pendingTransitions.delete(done));
      window.pendingTransitions.add(done);
      return transition;
    };
    window.devtoolsLog = [];
    window.__REDUX_DEVTOOLS_EXTENSION__ = {
      connect: () => ({
        init: (state) => window.devtoolsLog.push(['init', state]),
        send: (action, state) => window.devtoolsLog.push(['send', action.type, state]),
        subscribe: (listener) => (window.devtoolsListener = listener),
      }),
    };
  });
  try {
    await page.goto(overHttp.has(name) ? origin + '/' : `file://${dist}${name}.html`);
    assert.ok((await count(page, 'main > *, main')) > 0, 'nothing was mounted');
    await test(page);
    assert.deepEqual(errors, []);
    console.log(`ok       ${name}`);
  } catch (error) {
    failed++;
    console.log(`FAILED   ${name}`);
    console.log(error);
  }
  await page.close();
}

await browser.close();
server.close();
process.exit(failed ? 1 : 0);
