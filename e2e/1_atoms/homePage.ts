import { Page } from '@playwright/test';
import type { IHomePageAtoms } from '../types/atoms';

export class HomePageAtoms implements IHomePageAtoms {
  constructor(private page: Page) {}

  // ── Nav bar (desktop) ─────────────────────────────────────────────────────

  /** `header .buttons a[href="/login"] button.lightBlue` */
  async clickNavLogIn(): Promise<void> {
    await this.page.locator('header .buttons').getByRole('link', { name: 'Log In' }).click();
  }

  /** `header .buttons a[href="/signup"] button.blue` */
  async clickNavSignUp(): Promise<void> {
    await this.page.locator('header .buttons').getByRole('link', { name: 'Sign Up' }).click();
  }

  /** `header .menu a[href*="teaching"]` — opens in a new tab, just clicks the link */
  async clickNavTeachings(): Promise<void> {
    await this.page.locator('header menu.menu').getByRole('link', { name: 'Teachings' }).click();
  }

  /** `header .menu a[href*="equipping"]` — opens in a new tab */
  async clickNavEquipping(): Promise<void> {
    await this.page.locator('header menu.menu').getByRole('link', { name: 'Equipping' }).click();
  }

  /** `header .menu a[href*="subsplash"]` — opens in a new tab */
  async clickNavMessaging(): Promise<void> {
    await this.page.locator('header menu.menu').getByRole('link', { name: 'Messaging' }).click();
  }

  /** `header .hamburger` — opens the mobile nav `<dialog id="mobileNav">` */
  async clickHamburger(): Promise<void> {
    await this.page.locator('header .hamburger').click();
  }

  // ── Mobile nav (inside <dialog id="mobileNav">) ───────────────────────────

  /** `#mobileNav a[href="/login"] button` */
  async clickMobileNavLogIn(): Promise<void> {
    await this.page.locator('#mobileNav').getByRole('link', { name: 'Log In' }).click();
  }

  /** `#mobileNav a[href="/signup"] button` */
  async clickMobileNavSignUp(): Promise<void> {
    await this.page.locator('#mobileNav').getByRole('link', { name: 'Sign Up' }).click();
  }

  // ── Hero section ──────────────────────────────────────────────────────────

  /** `#homeContent .blueRoundedContainer a[href="/signup"] button.buttonContainer` */
  async clickBeginYourJourney(): Promise<void> {
    await this.page.getByRole('link', { name: 'BEGIN YOUR JOURNEY' }).click();
  }

  // ── Footer ────────────────────────────────────────────────────────────────

  /** `footer menu a[href="/login"]` — only visible when unauthenticated */
  async clickFooterLogin(): Promise<void> {
    await this.page.locator('footer').getByRole('link', { name: 'Login' }).click();
  }

  /** `footer menu a[href="mailto:support@p215.church"]` */
  async clickFooterContactUs(): Promise<void> {
    await this.page.locator('footer').getByRole('link', { name: 'Contact Us' }).click();
  }

}
