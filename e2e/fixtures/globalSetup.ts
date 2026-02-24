/**
 * globalSetup — runs once before the entire suite.
 *
 * Responsibilities:
 *   1. Seed persistent test user accounts from fixtures/seed.sql
 *   2. Save per-role auth storageState files so tests don't re-login each time
 *
 * Auth state files are written to fixtures/.auth/ (gitignored).
 * If this script fails the entire suite aborts — fix the DB or app first.
 */

import { chromium, FullConfig } from '@playwright/test';
import { execSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import { TEST_ACCOUNTS } from './credentials';

const ROOT = path.resolve(__dirname, '..');
const AUTH_DIR = path.join(ROOT, 'fixtures', '.auth');
const SEED_SQL = path.join(ROOT, 'fixtures', 'seed.sql');

// ── 1. Seed the database ──────────────────────────────────────────────────────

function seedDatabase() {
  // Resolve psql — prefer Homebrew libpq on macOS, fall back to PATH.
  const psqlCandidates = [
    '/opt/homebrew/opt/libpq/bin/psql',
    'psql',
  ];
  const psql = psqlCandidates.find(p => {
    try { execSync(`${p} --version`, { stdio: 'ignore' }); return true; } catch { return false; }
  });

  if (!psql) {
    throw new Error('psql not found — install libpq or add psql to PATH');
  }

  const env = {
    ...process.env,
    PGHOST: process.env.PGHOST ?? 'localhost',
    PGPORT: process.env.PGPORT ?? '5432',
    PGDATABASE: process.env.PGDATABASE ?? 'p215',
    ...(process.env.PGPASSWORD ? { PGPASSWORD: process.env.PGPASSWORD } : {}),
    ...(process.env.PGUSER ? { PGUSER: process.env.PGUSER } : {}),
  };

  execSync(`${psql} -f "${SEED_SQL}"`, { env, stdio: 'inherit' });
}

// ── 2. Save auth state ────────────────────────────────────────────────────────

const AUTH_USERS: Array<{ file: string; email: string; password: string }> = [
  { file: path.join(AUTH_DIR, 'user.json'),  email: TEST_ACCOUNTS.main.email,    password: TEST_ACCOUNTS.main.password },
  { file: path.join(AUTH_DIR, 'user2.json'), email: TEST_ACCOUNTS.support1.email, password: TEST_ACCOUNTS.support1.password },
];

async function saveAuthStates(baseURL: string) {
  fs.mkdirSync(AUTH_DIR, { recursive: true });

  const browser = await chromium.launch();

  for (const { file, email, password } of AUTH_USERS) {
    const ctx = await browser.newContext();
    const page = await ctx.newPage();

    await page.goto(`${baseURL}/login`);
    await page.getByPlaceholder('example@example.com').fill(email);
    await page.locator('input[name="password"]').fill(password);
    await page.getByRole('button', { name: 'Log In' }).click();
    // Login is a full-page server redirect; first load may hit a cold DB.
    await page.waitForURL('**/studies**', { timeout: 15_000 });

    await ctx.storageState({ path: file });
    await ctx.close();
  }

  await browser.close();
}

// ── Entry point ───────────────────────────────────────────────────────────────

export default async function globalSetup(config: FullConfig) {
  const baseURL = config.projects[0]?.use?.baseURL ?? 'http://localhost:8080';

  console.log('  [globalSetup] Seeding database…');
  seedDatabase();

  console.log('  [globalSetup] Saving auth state…');
  await saveAuthStates(baseURL);

  console.log('  [globalSetup] Done.');
}
