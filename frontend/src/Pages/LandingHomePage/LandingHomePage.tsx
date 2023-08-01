import React from "react";
import { Button3D } from "../../Components/Button3D/Button3D";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import bookFaceGirl from "./book-face-girl.jpg";
import peopleTalking from "./people-talking-at-table.png";

import * as classes from "./styles.module.scss";

export function LandingHomePage() {
  return (
    <div className={classes.grid}>
      <PreLoginTopNav />
      <div className={classes.mainContent}>
        <div className={classes.section1}>
          <div className={classes.leftText}>
            <h1 className={classes.sectionTitle}>
              Making Bible <br></br>Study Simple!
            </h1>
            <p className={classes.sectionDescription}>
              Take the complexity out of Bible study and delve deeper into the Word with others
            </p>
            <div className={classes.blueRoundedContainer}>
              <div>
                <h3>Intuitive Study Tools</h3>
                <p>Unlock the treasures of God’s word</p>
              </div>
              <Button3D children={undefined}>BEGIN YOUR JOURNEY</Button3D>
            </div>
          </div>
          <div className={classes.rightImage}>
            <img src={bookFaceGirl} />
          </div>
        </div>
        <div className={classes.section2}>
          <div className={classes.leftImage}>
            <img src={peopleTalking} />
          </div>
          <div className={classes.rightText}>
            <h1 className={classes.sectionTitle}>
              Dive in with <div style={{ color: "#0057d1" }}>Community!</div>
            </h1>
            <p className={classes.sectionDescription}>
              Disciple each other and seek God’s Truth through His Word
            </p>
            <Button3D children={undefined}>SIGN UP</Button3D>
          </div>
        </div>
      </div>
      <footer className={classes.footer}>
        <ul>
          <li>
            <a href="#">Login</a>
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
        <p className={classes.copyright}>
          Copyright © 2023 Hope Community Church. All Rights Reserved.
        </p>
      </footer>
    </div>
  );
}
