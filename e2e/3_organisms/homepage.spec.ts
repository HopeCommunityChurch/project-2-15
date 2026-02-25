import { test } from '../fixtures/appPage';

test('should show "Study Simple" on the homepage', async ({ page, home }) => {
  await page.goto('/');
  await home.assertHeroHeadingVisible();
});
