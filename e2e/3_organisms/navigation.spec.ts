import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { goToStudies } from '../2_molecules/goToStudies';
import { openStudyFromStudiesPage } from '../2_molecules/openStudyFromStudiesPage';
import { randomUUID } from 'crypto';

test.describe('navigation', () => {
  test('navigate to studies from editor logo lands on studies page @smoke', async ({
    page,
    studies,
    freshUser,
  }) => {
    const title = `Nav Home ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    // Click the home logo link in the editor header (/home = studies page for logged-in users).
    await page.locator('header a[href="/home"]').first().click();
    await page.waitForURL('**/home**', { timeout: 10_000 });
    // The studies page should list the study just created.
    await studies.assertStudyVisible(title);
  });

  test('open study from studies list loads the editor', async ({ page, editor, freshUser }) => {
    const title = `Nav Open ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await goToStudies(page);
    await openStudyFromStudiesPage(page, title);
    await editor.assertVisible();
  });

  test('direct URL to non-existent study shows not-found or not-authorized response', async ({
    page,
    editor,
    freshUser,
  }) => {
    await login(page, freshUser.email, freshUser.password);
    const fakeId = randomUUID();
    await page.goto(`/study/${fakeId}`);
    await page.waitForLoadState('networkidle');
    // The app renders a not-found or not-authorized template — there must be no
    // interactive editor present for the fake study ID.
    await editor.assertNotVisible();
  });
});
