#!/usr/bin/env node
import { execSync } from 'child_process';
import { readFileSync, existsSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function getBranchName() {
  if (process.env.GITHUB_REF_NAME) return process.env.GITHUB_REF_NAME;
  try {
    return execSync('git rev-parse --abbrev-ref HEAD', { encoding: 'utf8' }).trim();
  } catch {
    return '';
  }
}

/** @param {string} filePath @returns {{ description: string, tests: string[] }} */
function loadSuite(filePath) {
  const raw = JSON.parse(readFileSync(filePath, 'utf8'));
  if (typeof raw.description !== 'string' || raw.description.trim() === '')
    throw new Error(`Suite ${filePath}: missing or empty "description"`);
  if (!Array.isArray(raw.tests) || raw.tests.length === 0)
    throw new Error(`Suite ${filePath}: "tests" must be a non-empty array`);
  for (const t of raw.tests)
    if (typeof t !== 'string' || !t.startsWith('3_organisms/'))
      throw new Error(`Suite ${filePath}: invalid test glob "${t}" — must start with 3_organisms/`);
  return raw;
}

const branch = getBranchName();

// Normalise the branch name to a filesystem-safe slug: "/" → "-"
// e.g. "feature/my-thing" → "feature-my-thing"
//      "version-history"  → "version-history"
const slug = branch.replace(/\//g, '-');
const branchFile = join(__dirname, '..', '4_branch_suites', `${slug}.json`);
if (existsSync(branchFile)) {
  const suite = loadSuite(branchFile);
  process.stdout.write(suite.tests.join(' '));
  process.exit(0);
}

process.stdout.write('--grep @smoke');
