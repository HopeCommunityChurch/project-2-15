import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: '3_organisms',
  globalSetup: require.resolve('./fixtures/globalSetup'),
  timeout: 30_000,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 4 : 2,
  fullyParallel: true,
  reporter: process.env.CI
    ? [['html'], ['list'], ['json', { outputFile: 'test-results/results.json' }]]
    : [['html'], ['list']],
  use: {
    baseURL: process.env.BASE_URL ?? 'http://localhost:8080',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
    trace: 'on-first-retry',
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
});
