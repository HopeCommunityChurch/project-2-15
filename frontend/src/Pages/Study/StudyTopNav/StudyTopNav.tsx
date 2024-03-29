import Logo from "Assets/p215-circle.svg";
import ShareIcon from "Assets/share-icon-white.svg";
import { Button } from "Components/Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser, handleLogout } from "Pages/LoginPage/login";
import CloseXIcon from "Assets/x.svg";
import SearchIcon from "Assets/magnifying_glass.svg";
import SavingIcon from "./saving.svg";
import SavedIcon from "./saved.svg";
import HamburgerMenuIcon from "Assets/hamburger-menu-icon.svg";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "Hooks/useOutsideClickClose";
import { DocRaw } from "Types";
import * as Network from "Utils/Network";

type StudyTopNavProps = {
  isSidebarOpen: () => boolean;
  isTopbarOpen: () => boolean;
  setSidebarOpen: (value: boolean) => void;
  saving: () => boolean;
  savingError: () => string | null;
  doc: DocRaw;
};

export function StudyTopNav(props: StudyTopNavProps) {
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
  const [showDeleteStudyModal, setShowDeleteStudyModal] = createSignal(false);
  const [emailInputErrorMsg, setEmailInputErrorMsg] = createSignal("");
  const [emailTags, setEmailTags] = createSignal<string[]>([]);
  const [isStudyNameEditable, setIsStudyNameEditable] = createSignal(false);
  const [studyName, setStudyName] = createSignal(props.doc.name);
  const [previousStudyName, setPreviousStudyName] = createSignal(props.doc.name);

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

  const handleKeyPress = (event: KeyboardEvent) => {
    const target = event.target as HTMLInputElement;

    if (event.key === "Enter" || event.keyCode === 13) {
      event.preventDefault();
      const inputValue = target.value.trim();
      const emailRegex = /^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}$/;
      const isValidEmail = emailRegex.test(inputValue);

      if (!isValidEmail) {
        setEmailInputErrorMsg("Please enter a valid email address.");
      } else {
        setEmailInputErrorMsg("");
        setEmailTags([...emailTags(), inputValue]);
        target.value = ""; // Clear input field after adding tag
      }
    }
  };

  // On component mount
  createEffect(() => {
    applyUniqueColorsToElements();
  });

  createEffect(() => {
    useClickOutsideClose(showDropdown, setShowDropdown, classes.profileDropdown);
  });

  const nav = useNavigate();

  function logoutClick(e: MouseEvent) {
    e.preventDefault();
    handleLogout().then(() => {
      nav("/app/login");
    });
  }

  function handlePaste(event) {
    event.preventDefault();
    const clipboardData = (event as any).clipboardData || (window as any).clipboardData;
    const pastedText = clipboardData.getData("text/plain");

    // Replace all newline characters with a single space
    const cleanedText = pastedText.replace(/\n/g, " ");

    // Insert the cleaned text at the current selection point
    const selection = window.getSelection();
    const range = selection.getRangeAt(0);
    range.deleteContents();
    range.insertNode(document.createTextNode(cleanedText));

    // Update the content state with the new content
    setStudyName(event.target.innerHTML);
  }

  function submitStudyNameChange(e) {
    setIsStudyNameEditable(false);
    // Reset the scroll to show beginning of the title for long names
    if (e.target instanceof HTMLElement) {
      e.target.scrollLeft = 0;
    }

    if (previousStudyName() != studyName()) {
      setPreviousStudyName(studyName());

      Network.request<DocRaw>("/document/meta/" + props.doc.docId, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          name: studyName(),
        }),
      })
        .then((res) => {
          //@ts-ignore
          match(res)
            .with({ state: "error" }, ({ body }) => console.error(body))
            .with({ state: "success" }, ({ body }) => {})
            .exhaustive();
        })
        .catch((err) => {
          console.error(err);
        });
    }
  }

  function handleDeleteStudy() {
    Network.request("/document/" + props.doc.docId, {
      method: "DELETE",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({}),
    })
      .then((res) => {
        console.log(res);
        nav("/app/studies");
        setShowDeleteStudyModal(false);
      })
      .catch((err) => {
        console.error(err);
      });
  }

  return (
    <>
      <header class={`${classes.header} ${props.isTopbarOpen() ? "" : classes.collapsed}`}>
        <img
          src={HamburgerMenuIcon}
          onClick={() => props.setSidebarOpen(!props.isSidebarOpen())}
          class={classes.hamburgerMenuMobile}
        />

        <img class={classes.logo} src={Logo} onClick={() => nav("/app/studies")} />
        <p
          class={classes.studyHeaderInput}
          contentEditable={isStudyNameEditable()}
          onClick={(e) => {
            setIsStudyNameEditable(true);
            if (e.target instanceof HTMLElement) {
              e.target.focus();
            }
          }}
          onBlur={submitStudyNameChange}
          onInput={(e) => {
            if (e.target instanceof HTMLElement) {
              setStudyName(e.target.innerText);
            }
          }}
          onPaste={handlePaste}
          onKeyDown={(e) => {
            if ((e.key === "Enter" || e.key === "Escape") && e.target instanceof HTMLElement) {
              e.preventDefault();
              e.target.blur();
            }
          }}
        >
          {props.doc.name}
        </p>

        <Show when={props.savingError() === null}>
          <span class={classes.saving}>
            |
            {props.saving() ? (
              <p>
                <span>saving</span>
                <img src={SavingIcon} class={classes.savingUpdateIcon} />
              </p>
            ) : (
              <p>
                <span>saved</span>
                <img src={SavedIcon} class={classes.saveUpdateIcon} />
              </p>
            )}
          </span>
        </Show>
        <Show when={props.savingError() !== null}>
          <p>
            |<span class={classes.savingError}>Error saving</span>
          </p>
        </Show>

        <div>
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
                        <A href="/app/studies" class={classes.fullWidthLink}>
                          Home
                        </A>
                      </li>
                      <li>
                        <span
                          onClick={(e) => {
                            setShowDeleteStudyModal(!showDeleteStudyModal());
                            setShowDropdown(!showDropdown());
                          }}
                          class={classes.fullWidthLink}
                        >
                          Delete Study
                        </span>
                      </li>
                      <li>
                        <a
                          href="https://forms.gle/koJrP31Vh9TfvPcq7"
                          class={classes.fullWidthLink}
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          Give Feedback
                        </a>
                      </li>
                      <li>
                        <span onClick={logoutClick} class={classes.fullWidthLink}>
                          Sign Out
                        </span>
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
            <form
              onsubmit={(e) => {
                e.preventDefault();
              }}
            >
              <div class={classes.emailSearchField}>
                <img src={SearchIcon} class={classes.searchIcon} alt="Search" />

                <input
                  type="text"
                  placeholder="Add an email..."
                  onKeyPress={handleKeyPress}
                  class={emailInputErrorMsg() ? `${classes.errorInput}` : ""}
                />
              </div>
              {emailInputErrorMsg() && <p class={classes.errorMsg}>{emailInputErrorMsg()}</p>}
              <div class={classes.emailTagsContainer}>
                {emailTags().map((tag) => (
                  <span class={classes.emailTag}>
                    {tag}
                    <img
                      src={CloseXIcon}
                      alt="Close"
                      class={classes.removeTagIcon}
                      onclick={() => setEmailTags(emailTags().filter((email) => email !== tag))}
                    />
                  </span>
                ))}
              </div>
              <Show when={emailTags().length > 0}>
                <div class={classes.notifyPeopleCheckbox}>
                  <label>
                    Notify People
                    <input type="checkbox" id="notifyPeople" checked />
                    <span class={classes.checkmark}></span>
                  </label>
                </div>
                <textarea placeholder="Message" class={classes.inviteMessage} />
                <div class={classes.shareBottomButtons}>
                  <button onclick={() => setEmailTags([])}>Cancel</button>
                  <button type="submit">Invite</button>
                </div>
              </Show>
              <Show when={emailTags().length === 0}>
                <label>People in shared study</label>
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
              </Show>
            </form>
          </div>
        </div>
      </Show>
      <Show when={showDeleteStudyModal()}>
        <div class={classes.modalBackground} onClick={() => setShowDeleteStudyModal(false)}>
          <div
            class={`${classes.shareModal} ${classes.deleteStudyModal}`}
            onClick={(e) => e.stopPropagation()}
          >
            <h3>Are you sure?</h3>
            <p>
              "<strong>{studyName()}</strong>" will be moved to the trash. Warning: your study
              cannot be recovered.
            </p>
            <div class={classes.shareBottomButtons}>
              <button onClick={() => setShowDeleteStudyModal(false)}>Cancel</button>
              <button type="submit" onClick={handleDeleteStudy}>
                Yes, delete
              </button>
            </div>
          </div>
        </div>
      </Show>
    </>
  );
}
