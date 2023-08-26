import Logo from "../../Assets/p215-circle.svg";
import ShareIcon from "../../Assets/share-icon-white.svg";
import { Button } from "../Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "../../Pages/LoginPage/login";
import CloseXIcon from "../../Assets/x.svg";
import SearchIcon from "../../Assets/magnifying_glass.svg";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "../../Hooks/useOutsideClickClose";

export function StudyTopNav() {
  const otherUsersCurrentlyViewing = [
    {
      name: "Jonathan Solas",
      email: "jonathsola@gmail.com",
    },
    {
      name: "Olivia Rodriguez",
      email: "oliveronald23@gmail.com",
    },
    {
      name: "Jason Avilez",
      email: "chocolatejason@gmail.com",
    },
    {
      name: "Jordan Smoove",
      email: "toosmoove25@gmail.com",
    },
  ];
  const otherUsersInSharedStudy = [
    {
      name: "Jonathan Solas",
      email: "jonathsola@gmail.com",
    },
    {
      name: "Olivia Rodriguez",
      email: "oliveronald23@gmail.com",
    },
    {
      name: "Jason Avilez",
      email: "chocolatejason@gmail.com",
    },
    {
      name: "Jordan Smoove",
      email: "toosmoove25@gmail.com",
    },
  ];
  const [showDropdown, setShowDropdown] = createSignal(false);
  const [showShareModal, setShowShareModal] = createSignal(false);

  function applyUniqueColorsToElements() {
    // List of colors
    const colors = [
      "#00AE8D",
      "#009176",
      "#EBB72F",
      "#F08D2B",
      "#00C068",
      "#00A057",
      "#CE491A",
      "#1688CB",
      "#E64138",
      "#BA302A",
      "#904BA5",
      "#AEB5B9",
      "#2B3E4F",
      "#8B9471",
      "#A9834D",
      "#A03731",
    ];

    // Shuffle function based on the Fisher-Yates algorithm
    function shuffleArray<T>(array: T[]): T[] {
      for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
      }
      return array;
    }

    // Shuffled list of colors
    let shuffledColors = shuffleArray([...colors]);
    // Current index to track used colors
    let currentIndex = 0;

    // Function to get a unique color until all are used up
    const getUniqueColor = (): string => {
      if (currentIndex >= shuffledColors.length) {
        // Reshuffle the colors once all have been used
        shuffledColors = shuffleArray([...colors]);
        currentIndex = 0;
      }
      return shuffledColors[currentIndex++];
    };

    // Apply unique colors to each element
    document.querySelectorAll(`.${classes.otherUsersCurrentlyViewing} div`).forEach((el) => {
      (el as HTMLElement).style.backgroundColor = getUniqueColor();
    });
  }

  // On component mount
  createEffect(() => {
    applyUniqueColorsToElements();
  });

  createEffect(() => {
    useClickOutsideClose(showDropdown, setShowDropdown, classes.profileDropdown);
  });

  const nav = useNavigate();

  return (
    <>
      <header class={classes.header}>
        <img class={classes.logo} src={Logo} />
        <div class={classes.studyHeaderText}>
          <p>Study Title</p>
          <div>
            <p>Study Template</p>
            <p class={classes.hideBelow750px}>|</p>
            <p class={classes.hideBelow750px}>Study Collaborators</p>
          </div>
        </div>
        <div class={`${classes.otherUsersCurrentlyViewing} ${classes.hideBelow750px}`}>
          {otherUsersCurrentlyViewing.map((user) => (
            <div class={classes.userInitials}>{user.name.slice(0, 2).toUpperCase()}</div>
          ))}
        </div>
        <div class={classes.shareButton} onClick={() => setShowShareModal(true)}>
          <Button type="Blue">
            <img src={ShareIcon} />
            <p>Share</p>
          </Button>
        </div>
        <div>
          {loginState()}
          {
            // @ts-ignore
            match(loginState() as LoginUser)
              .with({ state: "notLoggedIn" }, () => (
                <>
                  <Button type="lightBlue" onClick={() => nav("/app/login")}>
                    Log In
                  </Button>
                  <Button type="Blue" onClick={() => nav("/app/signup")}>
                    Sign Up
                  </Button>
                </>
              ))
              .with({ state: "loggedIn" }, ({ user }) => (
                <>
                  <div
                    class={classes.profileButton}
                    onClick={() => setShowDropdown(!showDropdown())}
                  >
                    {user.name.slice(0, 2).toUpperCase()}
                  </div>
                  <div
                    class={`${classes.profileDropdown} ${
                      showDropdown() ? classes.showDropdown : ""
                    }`}
                  >
                    <ul>
                      <li>
                        <a href="/admin" class={classes.fullWidthLink}>
                          Admin Area
                        </a>
                      </li>
                      <li>
                        <a href="/logout" class={classes.fullWidthLink}>
                          Sign Out
                        </a>
                      </li>
                    </ul>
                  </div>
                </>
              ))
              .exhaustive()
          }
        </div>
      </header>
      <Show when={showShareModal()}>
        <div class={classes.modalBackground} onClick={() => setShowShareModal(false)}>
          <div class={classes.shareModal} onClick={(e) => e.stopPropagation()}>
            <h3>Share "Wednesday Night Romans Study"</h3>
            <img
              src={CloseXIcon}
              alt="Close Modal"
              class={classes.closeModalIcon}
              onClick={() => setShowShareModal(false)}
            />
            <form>
              <div class={classes.autocomplete}>
                <img src={SearchIcon} class={classes.searchIcon} alt="Search" />
                <input type="text" placeholder="Add an email..." />
              </div>
              <label for="inviteByEmail">People in shared study</label>
              {/* For Owner */}
              <div class={classes.personWithAccess}>
                <div class={classes.userInitials}>
                  {/* {user.name.slice(0, 2).toUpperCase()} */}
                  JH
                </div>
                <div class={classes.nameAndEmail}>
                  <p>James Hendrie (you)</p>
                  <p class={classes.email}>jhendrie25@gmail.com</p>
                </div>
                <div class={classes.accessPermissions}>Owner</div>
              </div>
              {/* For Others in study */}
              {otherUsersInSharedStudy.map((user) => (
                <div class={classes.personWithAccess}>
                  <div class={classes.userInitials}>{user.name.slice(0, 2).toUpperCase()}</div>
                  <div class={classes.nameAndEmail}>
                    <p>{user.name}</p>
                    <p class={classes.email}>{user.email}</p>
                  </div>
                  <div class={classes.accessPermissions}>Collaborator</div>{" "}
                </div>
              ))}
            </form>
          </div>
        </div>
      </Show>
      <div class={classes.headerThingy}></div>
    </>
  );
}
