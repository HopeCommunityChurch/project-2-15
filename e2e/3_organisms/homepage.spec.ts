import { test, expect } from '../fixtures/appPage';

test('should show "Study Simple" on the homepage', async ({ page }) => {
  await page.goto('/');
  await expect(page.getByText('Study Simple')).toBeVisible();
});
