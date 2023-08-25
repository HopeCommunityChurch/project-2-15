import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import Meme from "./meme.png";
import * as classes from "./styles.module.scss";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

export function Four04() {
  return (
    <>
      <div class={classes.container}>
        <div class={classes.content}>
          <div>
            <div class={classes.header}>404</div>
            <div class={classes.subheader}>Error: no page found</div>
            {/* Return to Home Button */}
            <div class={classes.returnHome}>
              <a href="/" class={classes.homeButton}>
                Return to Home
              </a>
            </div>
            <img class={classes.image} src={Meme} />
          </div>

          <div>
            {/* Helpful Resources or Popular Links */}
            <div class={classes.resources}>
              <h3>Popular Links:</h3>
              <ul>
                <li>
                  <a href="https://experiencethehope.com/teaching" target="_blank">
                    Teachings
                  </a>
                </li>
                <li>
                  <a href="https://experiencethehope.com/equipping" target="_blank">
                    Equipping
                  </a>
                </li>
                <li>
                  <a href="https://messaging.subsplash.com/25FXCW/auth" target="_blank">
                    Messaging
                  </a>
                </li>
              </ul>
            </div>

            {/* Contact Information */}
            <div class={classes.contactInfo}>
              <h3>Contact Us:</h3>
              <p>
                If you believe this is an error, please{" "}
                <a href="mailto:support@example.com">contact us</a>.
              </p>
            </div>

            {/* Feedback Mechanism */}
            <div class={classes.feedback}>
              <h3>Found an issue?</h3>
              <p>
                Help us improve by reporting the broken link <a href="/feedback">here</a>.
              </p>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
