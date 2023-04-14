import React from "react";
import { Button3D } from "../../Components/Button3D/Button3D";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import bookFaceGirl from "./book-face-girl.jpg";

import * as classes from "./styles.module.scss";

export function LandingHomePage() {
  return (
    <div className={classes.grid}>
      <PreLoginTopNav />
      <div className={classes.mainContent}>
        <div className={classes.section1}>
          <div className={classes.leftText}>
            <h1 className={classes.sectionTitle}>Making Bible Study Simpler than ever!</h1>
            <p className={classes.sectionDescription}>
              Take the complexity out of Bible study and delve deeper into the Word with others
            </p>
            <div className={classes.blueRoundedContainer}>
              <div>
                <h3>Intuitive Study Tools</h3>
                <p>Unlock the treasures of God’s word</p>
              </div>
              <Button3D>BEGIN YOUR JOURNEY</Button3D>
            </div>
          </div>
          <div className={classes.rightImage}>
            <img src={bookFaceGirl} />
          </div>
        </div>
      </div>
      <footer>Footer</footer>
    </div>
  );
}
