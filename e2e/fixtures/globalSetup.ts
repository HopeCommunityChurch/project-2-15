/**
 * globalSetup — runs once before the entire suite.
 *
 * Seeds the E2E church from fixtures/seed.sql. Individual test users are
 * created dynamically by fixtures/userFactory.ts — no auth state files needed.
 *
 * If this script fails the entire suite aborts — fix the DB or app first.
 */

import { FullConfig } from '@playwright/test';
import { execSync } from 'child_process';
import * as path from 'path';

const ROOT = path.resolve(__dirname, '..');
const SEED_SQL = path.join(ROOT, 'fixtures', 'seed.sql');

// ── Seed the database ─────────────────────────────────────────────────────────

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

// ── Entry point ───────────────────────────────────────────────────────────────

export default async function globalSetup(_config: FullConfig) {
  console.log('  [globalSetup] Seeding database…');
  seedDatabase();
  console.log('  [globalSetup] Done.');
}
