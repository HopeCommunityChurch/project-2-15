import Logo from "Assets/p215-full-logo.svg";
import { Button } from "Components/Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "Pages/LoginPage/login";
import SearchIcon from "Assets/magnifying_glass.svg";
import CloseXIcon from "Assets/x.svg";
import PadlockIcon from "Assets/padlock.svg";
import ArrowIcon from "Assets/arrow.svg";
import NotificationBell from "Assets/notification-bell.svg";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "../../../Hooks/useOutsideClickClose";

export function AdminTopNav() {
  const [showDropdown, setShowDropdown] = createSignal(false);
  const [showModal, setShowModal] = createSignal(false);
  const [studyTitleValue, setStudyTitleValue] = createSignal("");
  const [showNotificationsDropdown, setShowNotificationsDropdown] = createSignal(false);

  const notificationList = [
    {
      studyName: "Romans August 2023 Study",
      inviterName: "Jean Smith",
      date: "01/30/2023",
    },
    {
      studyName: "Colossians August 2023 Study",
      inviterName: "Jean Smith",
      date: "01/30/2023",
    },
    {
      studyName: "Men's thursday night 5G study",
      inviterName: "Jean Smith",
      date: "01/30/2023",
    },
    {
      studyName: "Study with a really long name just so we can test the limits of sizing for this",
      inviterName: "Jean Smith",
      date: "01/30/2023",
    },
  ];

  createEffect(() => {
    useClickOutsideClose(showDropdown, setShowDropdown, classes.profileDropdown);
  });

  createEffect(() => {
    useClickOutsideClose(
      showNotificationsDropdown,
      setShowNotificationsDropdown,
      classes.notificationDropdown
    );
  });

  const nav = useNavigate();

  return (
    <>
      <header class={classes.header}>
        <img class={classes.logo} src={Logo} onClick={() => nav("/app/studies")} />
        <nav class={classes.menu}>
          <ul>
            <li class={classes.desktopOnlyItem}>
              <a href="https://experiencethehope.com/teaching" target="_blank">
                Teachings
              </a>
            </li>
            <li class={classes.desktopOnlyItem}>
              <a href="https://experiencethehope.com/equipping" target="_blank">
                Equipping
              </a>
            </li>
            <li class={classes.desktopOnlyItem}>
              <a href="https://messaging.subsplash.com/25FXCW/auth" target="_blank">
                Messaging
              </a>
            </li>
            <li class={classes.desktopOnlyItem2}>
              <Button type="Blue" onClick={() => setShowModal(true)}>
                + New Template
              </Button>
            </li>
            <li class={classes.notificationBellContainer}>
              <div
                class={classes.notificationBell}
                onClick={() => setShowNotificationsDropdown(!showNotificationsDropdown())}
              >
                <img src={NotificationBell} />
                <div class={classes.notificationBadge}>{notificationList.length}</div>
              </div>
            </li>
            <Show when={showNotificationsDropdown()}>
              <div class={classes.notificationDropdown}>
                {notificationList.map((notification) => (
                  <div class={classes.notificationItem}>
                    <div class={classes.notificationText}>
                      <p>You've been invited to join {notification.studyName}</p>
                      <div class={classes.notificationDateAndName}>
                        <p class={classes.notificationDate}>{notification.date}</p>
                        <p>{notification.inviterName}</p>
                      </div>
                    </div>
                    <div class={classes.notificationButtons}>
                      <Button
                        type="Blue"
                        onClick={() => {
                          /* accept logic */
                        }}
                      >
                        Accept
                      </Button>

                      <Button
                        type="lightBlue"
                        onClick={() => {
                          /* accept logic */
                        }}
                      >
                        Reject
                      </Button>
                    </div>
                  </div>
                ))}
              </div>
            </Show>
          </ul>
        </nav>
        <div class={classes.buttons}>
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
                      <li class={classes.mobileOnlyItem}>
                        <Button type="Blue" onClick={() => setShowModal(true)}>
                          + New Study
                        </Button>
                      </li>
                      <li>
                        <a href="/app/studies" class={classes.fullWidthLink}>
                          Home
                        </a>
                      </li>
                      <li>
                        <a href="/app/account" class={classes.fullWidthLink}>
                          My Account
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
      <Show when={showModal()}>
        <div class={classes.modalBackground} onClick={() => setShowModal(false)}>
          <div class={classes.modal} onClick={(e) => e.stopPropagation()}>
            <h3>Create New Study Template</h3>
            <img
              src={CloseXIcon}
              alt="Close Modal"
              class={classes.closeModalIcon}
              onClick={() => setShowModal(false)}
            />
            <form>
              <label for="studyTitle">Title</label>
              <input
                type="text"
                id="studyTitle"
                placeholder="New Template Title..."
                value={studyTitleValue()}
                onInput={(e) => setStudyTitleValue(e.target.value)}
              />

              <button
                type="submit"
                disabled={studyTitleValue().trim() === ""}
                class={studyTitleValue().trim() === "" ? classes.disabledButton : ""}
              >
                Create
              </button>
            </form>
          </div>
        </div>
      </Show>
      <div class={classes.headerThingy}></div>
    </>
  );
}

const Hamburger = () => (
  <svg xmlns="http://www.w3.org/2000/svg" width="52" height="24" viewBox="0 0 52 24">
    <g id="Group_9" data-name="Group 9" transform="translate(-294 -47)">
      <rect
        id="Rectangle_3"
        data-name="Rectangle 3"
        width="42"
        height="4"
        rx="2"
        transform="translate(304 47)"
        fill="#574c4c"
      />
      <rect
        id="Rectangle_5"
        data-name="Rectangle 5"
        width="42"
        height="4"
        rx="2"
        transform="translate(304 67)"
        fill="#574c4c"
      />
      <rect
        id="Rectangle_4"
        data-name="Rectangle 4"
        width="52"
        height="4"
        rx="2"
        transform="translate(294 57)"
        fill="#574c4c"
      />
    </g>
  </svg>
);
