#!/usr/bin/env node
/**
 * Checks for @flaky-tagged tests.
 *
 * Locally (no CI env var): prints a warning table, exits 0.
 * In CI: fails if any @flaky tag exists without an adjacent "// FLAKY: <reason>" comment.
 *
 * To allow a tag temporarily, add a reason on the same line:
 *   test('name @flaky', /* FLAKY: race in WS reconnect, fix sprint 12 *‌/ async ...
 *
 * Run via: npm run check:flaky
 */
import { execSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join, relative } from 'path';
import { readFileSync } from 'fs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const organismsDir = join(__dirname, '..', '3_organisms');
const isCI = Boolean(process.env.CI);

// Find all files containing @flaky
let grepOutput = '';
try {
  grepOutput = execSync(`grep -rn "@flaky" "${organismsDir}" --include="*.ts"`, {
    encoding: 'utf8',
  });
} catch {
  // grep exits non-zero when no matches — that's fine
}

if (!grepOutput.trim()) {
  console.log('check-flaky: 0 @flaky tests. ✓');
  process.exit(0);
}

// Parse each match into { file, line, text }
const matches = grepOutput
  .trim()
  .split('\n')
  .map(line => {
    const m = line.match(/^(.+?):(\d+):(.*)$/);
    if (!m) return null;
    return { file: m[1], lineNum: m[2], text: m[3] };
  })
  .filter(Boolean);

// Classify: acknowledged = has "FLAKY:" comment on the same line
const unacknowledged = matches.filter(m => !/FLAKY:/i.test(m.text));
const acknowledged   = matches.filter(m =>  /FLAKY:/i.test(m.text));

// Print summary table
console.log(`\n⚠  ${matches.length} test(s) tagged @flaky:\n`);
for (const m of matches) {
  const rel = relative(join(__dirname, '..'), m.file);
  const title = (m.text.match(/'([^']*@flaky[^']*)'/) || m.text.match(/"([^"]*@flaky[^"]*)"/) || [])[1] ?? m.text.trim();
  const ack = /FLAKY:/i.test(m.text) ? `  (${m.text.match(/FLAKY:\s*([^*\/]+)/i)?.[1]?.trim()})` : '  ← needs FLAKY: comment';
  console.log(`   ${rel}:${m.lineNum}  "${title}"${ack}`);
}
console.log('');

if (isCI && unacknowledged.length > 0) {
  console.error(
    `check-flaky: ${unacknowledged.length} @flaky tag(s) have no FLAKY: reason comment.\n` +
    `Add a comment on the same line explaining why, e.g.:\n` +
    `  test('name @flaky', /* FLAKY: race in WS reconnect */ async ({ page }) => { ... })\n`
  );
  process.exit(1);
}

if (!isCI) {
  console.warn('check-flaky: warnings above — investigate before pushing to CI.');
}

process.exit(0);
