// Render a PDF via Chrome's built-in pdfium viewer and screenshot the page.
// usage: node chrome-shot.js <abs-pdf-path> <out-png> [widthCSSpx]
const { chromium } = require('playwright');
(async () => {
  const [pdf, out, wArg] = process.argv.slice(2);
  const width = parseInt(wArg || '1224', 10); // 612pt * 2
  const browser = await chromium.launch({ headless: true, args: ['--no-sandbox'] });
  const page = await browser.newPage({ viewport: { width, height: Math.round(width * 792 / 612) } });
  await page.goto('file://' + pdf + '#toolbar=0&navpanes=0&scrollbar=0&view=Fit',
                  { waitUntil: 'load', timeout: 30000 });
  await page.waitForTimeout(2500);
  await page.screenshot({ path: out });
  await browser.close();
  console.log('wrote', out);
})().catch(e => { console.error(e); process.exit(1); });
