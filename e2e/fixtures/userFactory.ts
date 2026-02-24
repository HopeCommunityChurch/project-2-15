/**
 * Dynamic test user factory.
 *
 * Creates a unique user directly in the DB for each test that needs one.
 * Tests never share accounts — each gets its own UUID-addressed user.
 *
 * The password is always TEST_PASSWORD. Its bcrypt hash is pre-computed so we
 * don't need bcrypt at runtime — we just insert the known hash.
 *
 * Factory users accumulate in the DB across runs. That is intentional: no
 * teardown is needed because every email is a UUID and tests never query by
 * count or assert on global state.
 */

import { Pool } from 'pg';
import { randomUUID } from 'crypto';

// All factory users share this password and its pre-computed bcrypt hash (cost 10).
export const TEST_PASSWORD = 'TestPassword1!';
const TEST_PASSWORD_HASH = '$2b$10$A1JuCGUj2U.6MoSs30iwjuMxdHhyE.Cf7Ag78oM0qmExbfAVOjzsW';

// All factory users are members of the seeded E2E church.
const E2E_CHURCH_ID = '00000000-0000-0000-0000-000000000001';

// One pool is shared for the lifetime of the worker process.
let pool: Pool | null = null;

function getPool(): Pool {
  if (!pool) {
    pool = new Pool({ host: 'localhost', port: 5432, database: 'p215' });
  }
  return pool;
}

export interface TestUser {
  userId: string;
  email: string;
  password: string;
}

/**
 * Inserts a fresh user into the DB and returns its credentials.
 * Safe to call in parallel across workers — every call produces a unique userId/email.
 */
export async function createTestUser(): Promise<TestUser> {
  const userId = randomUUID();
  const email = `test-${randomUUID().slice(0, 8)}@e2e.local`;
  const name = `Test ${userId.slice(0, 8)}`;

  const client = await getPool().connect();
  try {
    await client.query(
      `INSERT INTO "user" ("userId", "email", "name", "churchId") VALUES ($1, $2, $3, $4)`,
      [userId, email, name, E2E_CHURCH_ID],
    );
    await client.query(
      `INSERT INTO "user_password" ("userId", "password") VALUES ($1, $2)`,
      [userId, TEST_PASSWORD_HASH],
    );
  } finally {
    client.release();
  }

  return { userId, email, password: TEST_PASSWORD };
}
