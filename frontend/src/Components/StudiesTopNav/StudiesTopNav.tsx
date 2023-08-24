import Logo from "./P215.png";
import { Button } from "../Button/Button";
import { createSignal, createEffect, Show } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "../../Pages/LoginPage/login";
import SearchIcon from "../../Assets/magnifying_glass.svg";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";

export function StudiesTopNav() {
  const [showDropdown, setShowDropdown] = createSignal(false);
  const [showModal, setShowModal] = createSignal(false);
  const [studyBookValue, setStudyBookValue] = createSignal("");
  const [selectedBook, setSelectedBook] = createSignal(null);
  const [focusedBookIndex, setFocusedBookIndex] = createSignal(-1);

  const books = [
    "Genesis",
    "Exodus",
    "Leviticus",
    "Numbers",
    "Deuteronomy",
    "Joshua",
    "Judges",
    "Ruth",
    "1 Samuel",
    "2 Samuel",
    "1 Kings",
    "2 Kings",
    "1 Chronicles",
    "2 Chronicles",
    "Ezra",
    "Nehemiah",
    "Esther",
    "Job",
    "Psalms",
    "Proverbs",
    "Ecclesiastes",
    "Song of Solomon",
    "Isaiah",
    "Jeremiah",
    "Lamentations",
    "Ezekiel",
    "Daniel",
    "Hosea",
    "Joel",
    "Amos",
    "Obadiah",
    "Jonah",
    "Micah",
    "Nahum",
    "Habakkuk",
    "Zephaniah",
    "Haggai",
    "Zechariah",
    "Malachi",
    "Matthew",
    "Mark",
    "Luke",
    "John",
    "Acts",
    "Romans",
    "1 Corinthians",
    "2 Corinthians",
    "Galatians",
    "Ephesians",
    "Philippians",
    "Colossians",
    "1 Thessalonians",
    "2 Thessalonians",
    "1 Timothy",
    "2 Timothy",
    "Titus",
    "Philemon",
    "Hebrews",
    "James",
    "1 Peter",
    "2 Peter",
    "1 John",
    "2 John",
    "3 John",
    "Jude",
    "Revelation",
  ];

  const filteredBooks = () => {
    return books
      .filter((book) => book.toLowerCase().includes(studyBookValue().toLowerCase()))
      .slice(0, 5);
  };

  const selectBook = (book) => {
    setSelectedBook(book);
    setStudyBookValue("");
  };

  createEffect(() => {
    if (showDropdown()) {
      const dropdown = document.querySelector(".profileDropdown") as HTMLElement;
      const rect = dropdown.getBoundingClientRect();

      if (rect.right > window.innerWidth) {
        dropdown.style.right = "0";
      }
      if (rect.bottom > window.innerHeight) {
        dropdown.style.top = `${-rect.height}px`;
      }
    }
  });

  const nav = useNavigate();

  return (
    <>
      <header class={classes.header}>
        <img class={classes.logo} src={Logo} />
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
                + New Study
              </Button>
            </li>
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
      <Show when={showModal()}>
        <div class={classes.modalBackground} onClick={() => setShowModal(false)}>
          <div class={classes.modal} onClick={(e) => e.stopPropagation()}>
            <h3>Create New Study</h3>
            <form>
              <label for="studyTitle">Title</label>
              <input type="text" id="studyTitle" placeholder="New Title..." />
              <label for="studyBook">Book</label>
              <div class={classes.autocomplete}>
                {selectedBook() ? (
                  <span class={classes.tag} onClick={() => setSelectedBook(null)}>
                    {selectedBook()}
                    <span class={classes.removeTag}>X</span>
                  </span>
                ) : (
                  <>
                    <img src={SearchIcon} class={classes.searchIcon} alt="Search" />
                    <input
                      type="text"
                      id="studyBook"
                      placeholder="Select Book"
                      value={studyBookValue()}
                      onInput={(e) => {
                        setStudyBookValue(e.target.value);
                        if (filteredBooks().length > 0) {
                          setFocusedBookIndex(0);
                        } else {
                          setFocusedBookIndex(-1); // Reset index when there are no matches
                        }
                      }}
                      onKeyDown={(e) => {
                        switch (e.key) {
                          case "ArrowDown":
                            e.preventDefault(); // Prevent cursor movement
                            if (focusedBookIndex() < filteredBooks().length - 1) {
                              setFocusedBookIndex(focusedBookIndex() + 1);
                            }
                            break;
                          case "ArrowUp":
                            e.preventDefault(); // Prevent cursor movement
                            if (focusedBookIndex() > 0) {
                              setFocusedBookIndex(focusedBookIndex() - 1);
                            }
                            break;
                          case "Enter":
                            e.preventDefault(); // Prevent form submission
                            if (focusedBookIndex() !== -1) {
                              selectBook(filteredBooks()[focusedBookIndex()]);
                            }
                            break;
                          default:
                            break;
                        }
                      }}
                    />
                  </>
                )}
                <Show when={studyBookValue() && !selectedBook()}>
                  <ul class={classes.dropdownList}>
                    {filteredBooks().length > 0 ? (
                      filteredBooks().map((book, index) => (
                        <li
                          onClick={() => selectBook(book)}
                          class={focusedBookIndex() === index ? classes.focusedBook : ""}
                        >
                          {book}
                        </li>
                      ))
                    ) : (
                      <li class={classes.noResult}>There are no books that match your search</li>
                    )}
                  </ul>
                </Show>
              </div>
              <button type="submit">Create</button>
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
