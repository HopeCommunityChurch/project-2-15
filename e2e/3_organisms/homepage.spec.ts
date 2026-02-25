import { test } from '../fixtures/appPage';

test.describe('homepage', () => {
  test('should show "Study Simple" on the homepage @smoke', async ({ page, home }) => {
    await page.goto('/');
    await home.assertHeroHeadingVisible();
  });
});
