import { Button3D } from "../../Components/Button3D/Button3D";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { A } from "@solidjs/router";

import bookFaceGirl from "./book-face-girl.jpg";
import peopleTalking from "./people-talking.png";

import * as classes from "./styles.module.scss";

export function LandingHomePage() {
  return (
    <div class={classes.grid}>
      <PreLoginTopNav />
      <div class={classes.mainContent}>
        <div class={classes.section1}>
          <div class={classes.leftText}>
            <h1 class={classes.sectionTitle}>
              Making Bible <br></br>Study Simple!
            </h1>
            <p class={classes.sectionDescription}>
              Take the complexity out of Bible study and delve deeper into the Word with others
            </p>
            <div class={classes.blueRoundedContainer}>
              <div>
                <h3>Intuitive Study Tools</h3>
                <p>Unlock the treasures of God’s word</p>
              </div>
              <Button3D>BEGIN YOUR JOURNEY</Button3D>
            </div>
          </div>
          <div class={classes.rightImage}>
            <img src={bookFaceGirl} />
          </div>
        </div>
        <div class={classes.section2}>
          <div class={classes.leftImage}>
            <img src={peopleTalking} />
          </div>
          <div class={classes.rightText}>
            <h1 class={classes.sectionTitle}>
              Dive in with <div style={{ color: "#0057d1" }}>Community!</div>
            </h1>
            <p class={classes.sectionDescription}>
              Disciple each other and seek God’s Truth through His Word
            </p>
            <Button3D>SIGN UP</Button3D>
          </div>
        </div>
      </div>
      <footer class={classes.footer}>
        <ul>
          <li>
            <A href="/app/login">Login</A>
          </li>
          <li>
            <a href="#">Founding Church</a>
          </li>
          <li>
            <a href="#">Contact Us</a>
          </li>
          <li>
            <a href="#">Terms of Service</a>
          </li>
          <li>
            <a href="#">Privacy Policy</a>
          </li>
        </ul>
        <p class={classes.copyright}>
          Copyright © 2023 Hope Community Church. All Rights Reserved.
        </p>
      </footer>
    </div>
  );
}
