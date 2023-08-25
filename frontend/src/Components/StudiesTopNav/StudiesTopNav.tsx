import Logo from "./P215.png";
import { Button } from "../Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "../../Pages/LoginPage/login";
import SearchIcon from "../../Assets/magnifying_glass.svg";
import CloseXIcon from "../../Assets/x.svg";
import PadlockIcon from "../../Assets/padlock.svg";
import ArrowIcon from "../../Assets/arrow.svg";
import NotificationBell from "../../Assets/notification-bell.svg";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "../../Hooks/useOutsideClickClose";

export function StudiesTopNav() {
  const initialStudyItems = [
    {
      Name: "Relate (Opening Prayer)",
      Description:
        "Starting with a prayer to connect personally and spiritually, asking for guidance and understanding.",
      Required: true,
      Custom: false,
    },
    {
      Name: "Review",
      Description:
        "Looking back on previous studies or chapters to recall the context and overarching narrative.",
      Required: true,
      Custom: false,
    },
    {
      Name: "Outline",
      Description: "Creating a structured summary of the passage's main points and themes.",
      Required: true,
      Custom: false,
    },
    {
      Name: "Questions",
      Description: "Be curious, my friend.",
      Required: true,
      Custom: false,
    },
  ];
  const [showDropdown, setShowDropdown] = createSignal(false);
  const [showStudyBlockDropdown, setShowStudyBlockDropdown] = createSignal(false);
  const [selectedStudyItems, setSelectedStudyItems] = createSignal(initialStudyItems);
  const [showModal, setShowModal] = createSignal(false);
  const [studyBookValue, setStudyBookValue] = createSignal("");
  const [selectedBook, setSelectedBook] = createSignal(null);
  const [focusedBookIndex, setFocusedBookIndex] = createSignal(-1);
  const [studyBlockValue, setStudyBlockValue] = createSignal("");
  const [showTooltip, setShowTooltip] = createSignal(false);
  const [studyTitleValue, setStudyTitleValue] = createSignal("");
  const [showNotificationsDropdown, setShowNotificationsDropdown] = createSignal(false);

  const filteredStudyBlockItems = () => {
    return studyBlockItems.filter(
      (item) =>
        item.Name.toLowerCase().includes(studyBlockValue().toLowerCase()) ||
        item.Description.toLowerCase().includes(studyBlockValue().toLowerCase())
    );
  };

  const studyBlockItems = [
    {
      Name: "Break Down Segments",
      Description: "Dividing the scripture into smaller sections or verses to analyze in detail.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Read ESV, Listen NLT",
      Description:
        "Reading the passage in the English Standard Version (ESV) and listening to its narration in the New Living Translation (NLT) to gain different perspectives.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Topics",
      Description:
        "Identifying specific subjects or themes present in the scripture to further study or discuss.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Principle",
      Description:
        "Extracting the core teachings or truths that can be applied universally from the passage.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Summarize",
      Description: "Crafting a concise recap of everything learned from the passage.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Application Questions",
      Description:
        "Formulating questions to prompt deeper reflection on how to practically apply the scripture's teachings.",
      Required: false,
      Custom: false,
    },
    {
      Name: "Inconsistencies",
      Description:
        "Recognizing and discussing any contradictions or differences found within the scripture or when compared to other passages.",
      Required: false,
      Custom: false,
    },
  ];

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

  const filteredBooks = () => {
    return books
      .filter((book) => book.toLowerCase().includes(studyBookValue().toLowerCase()))
      .slice(0, 5);
  };

  const selectBook = (book) => {
    setSelectedBook(book);
    setStudyBookValue("");
    if (studyTitleValue().trim() === "") {
      setStudyTitleValue(`${selectedBook() ? selectedBook() : "Romans"} ${formattedDate} Study`);
    }
  };

  // Helper function to add or remove a study item from the selectedStudyItems array
  const toggleStudyItem = (item) => {
    const currentItems = selectedStudyItems();
    const itemExists = currentItems.some((selectedItem) => selectedItem.Name === item.Name);

    if (itemExists) {
      setSelectedStudyItems(currentItems.filter((selectedItem) => selectedItem.Name !== item.Name));
    } else {
      setSelectedStudyItems([...currentItems, { ...item }]);
    }
  };

  const moveItemUp = (index) => {
    if (index === 0) return; // First item cannot move up
    const newItems = [...selectedStudyItems()];
    const temp = newItems[index];
    newItems[index] = newItems[index - 1];
    newItems[index - 1] = temp;
    setSelectedStudyItems(newItems);
  };

  const moveItemDown = (index) => {
    if (index === selectedStudyItems().length - 1) return; // Last item cannot move down
    const newItems = [...selectedStudyItems()];
    const temp = newItems[index];
    newItems[index] = newItems[index + 1];
    newItems[index + 1] = temp;
    setSelectedStudyItems(newItems);
  };

  const addCustomBlock = () => {
    const currentItems = selectedStudyItems();
    const newCustomBlock = {
      Name: "",
      Description: "",
      Custom: true,
      Required: false,
    };
    setSelectedStudyItems([...currentItems, newCustomBlock]);
  };

  const updateStudyItem = (newName, newDescription, index) => {
    const updatedItems = [...selectedStudyItems()];
    updatedItems[index].Name = newName;
    updatedItems[index].Description = newDescription;
    setSelectedStudyItems(updatedItems);
  };

  createEffect(() => {
    useClickOutsideClose(showDropdown, setShowDropdown, classes.profileDropdown);
  });

  createEffect(() => {
    useClickOutsideClose(
      showStudyBlockDropdown,
      setShowStudyBlockDropdown,
      classes.studyBlockDropdownContainer,
      ["preventClose"]
    );
  });

  createEffect(() => {
    useClickOutsideClose(
      showNotificationsDropdown,
      setShowNotificationsDropdown,
      classes.notificationDropdown
    );
  });

  const nav = useNavigate();
  const currentDate = new Date();
  const formattedDate =
    currentDate.toLocaleString("default", { month: "long" }) + " " + currentDate.getFullYear();

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
                placeholder="New Title..."
                value={studyTitleValue()}
                onInput={(e) => setStudyTitleValue(e.target.value)}
              />
              <p class={classes.fieldDescription}>
                Ex: "
                <em>
                  {selectedBook() ? selectedBook() : "Romans"} {formattedDate} Study
                </em>
                " or "
                <em>Wednesday Night {selectedBook() ? selectedBook() : "Colossians"} Study</em>"
              </p>
              <label for="studyBook">Book</label>
              <div class={classes.autocomplete}>
                {selectedBook() ? (
                  <span class={classes.tag} onClick={() => setSelectedBook(null)}>
                    {selectedBook()}
                    <img src={CloseXIcon} alt="Close" class={classes.removeTagIcon} />
                  </span>
                ) : (
                  <>
                    <img src={SearchIcon} class={classes.searchIcon} alt="Search" />
                    <input
                      type="text"
                      id="studyBook"
                      placeholder="Bible Book Name..."
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
              <Show when={selectedBook()}>
                <label>Study Block</label>
                <div class={classes.studyBlockDropdownContainer}>
                  <div class={classes.autocomplete}>
                    <img src={SearchIcon} class={classes.searchIcon} alt="Search" />
                    <input
                      type="text"
                      id="studyBlock"
                      placeholder="Add Study Items"
                      value={studyBlockValue()}
                      onInput={(e) => setStudyBlockValue(e.target.value)}
                      onFocus={() => setShowStudyBlockDropdown(true)}
                    />
                    <Show when={studyBlockValue()}>
                      <img
                        src={CloseXIcon}
                        class={`${classes.closeIcon} preventClose`}
                        alt="Clear"
                        onClick={(e) => {
                          e.stopPropagation();
                          setStudyBlockValue("");
                        }}
                      />
                    </Show>
                    <Button type="Blue">Reset</Button>
                  </div>
                  <Show when={showStudyBlockDropdown()}>
                    <div class={classes.studyBlockDropdown}>
                      <div class={classes.studyBlockCategory}>
                        {filteredStudyBlockItems().length > 0 ? (
                          filteredStudyBlockItems().map((item) => (
                            <div
                              class={`${classes.studyBlockItem} ${
                                selectedStudyItems().some(
                                  (selectedItem) => selectedItem.Name === item.Name
                                )
                                  ? classes.selectedItem
                                  : ""
                              }`}
                              onClick={() => toggleStudyItem(item)}
                            >
                              <strong>{item.Name}</strong>
                              <p>{item.Description}</p>
                            </div>
                          ))
                        ) : (
                          <p class={classes.studyBlockNoResults}>
                            No study block items match your search.
                            <br />
                            Maybe you're looking to create a{" "}
                            <a onClick={addCustomBlock}>Custom Block?</a>
                          </p>
                        )}
                      </div>
                    </div>
                  </Show>
                </div>
                <div class={classes.studyBlockPreview}>
                  <div class={classes.reorderable}>
                    {selectedStudyItems().map((item, index) => {
                      const [localName, setLocalName] = createSignal(item.Name);
                      const [localDescription, setLocalDescription] = createSignal(
                        item.Description
                      );
                      return (
                        <>
                          <div class={classes.reorderableItem}>
                            <div class={classes.upAndDownArrows}>
                              <img
                                class={classes.arrowUp}
                                src={ArrowIcon}
                                onClick={() => moveItemUp(index)}
                              />
                              <img src={ArrowIcon} onClick={() => moveItemDown(index)} />
                            </div>
                            <div class={classes.reordableItemNameAndDescription}>
                              <Show when={!item.Required}>
                                <img
                                  src={CloseXIcon}
                                  alt="Close"
                                  class={classes.removeReordableItemIcon}
                                  onClick={() => {
                                    // Create a new array without the item at the current index
                                    const updatedStudyItems = [...selectedStudyItems()].filter(
                                      (_, idx) => idx !== index
                                    );
                                    setSelectedStudyItems(updatedStudyItems);
                                  }}
                                />
                              </Show>
                              {item.Custom ? (
                                <>
                                  <label>Custom Name</label>
                                  <br />
                                  <input
                                    class={classes.customReordableItemName}
                                    type="text"
                                    value={localName()}
                                    onInput={(e) => setLocalName(e.target.value)}
                                    onBlur={() =>
                                      updateStudyItem(localName(), item.Description, index)
                                    }
                                  />
                                  <br />
                                </>
                              ) : (
                                <div class={classes.reordableItemNameContainer}>
                                  <p class={classes.reordableItemName}>{item.Name}</p>
                                  <Show when={item.Required}>
                                    <img src={PadlockIcon} class={classes.padlockIcon} />
                                  </Show>
                                </div>
                              )}
                              {item.Custom ? (
                                <>
                                  <label>Custom Description</label>
                                  <br />
                                  <input
                                    class={classes.customReordableItemDescription}
                                    type="text"
                                    value={localDescription()}
                                    onInput={(e) => setLocalDescription(e.target.value)}
                                    onBlur={() =>
                                      updateStudyItem(item.Name, localDescription(), index)
                                    }
                                  />
                                </>
                              ) : (
                                <p class={classes.reordableItemDescription}>{item.Description}</p>
                              )}
                            </div>
                          </div>
                        </>
                      );
                    })}
                  </div>
                </div>
              </Show>
              <div
                class={classes.tooltipContainer}
                onMouseEnter={() => {
                  if (!selectedBook() || studyTitleValue().trim() === "") {
                    setShowTooltip(true);
                  }
                }}
                onMouseLeave={() => setShowTooltip(false)}
              >
                <button
                  type="submit"
                  disabled={!selectedBook() || studyTitleValue().trim() === ""}
                  class={
                    !selectedBook() || studyTitleValue().trim() === "" ? classes.disabledButton : ""
                  }
                >
                  Create
                </button>
                {showTooltip() && (
                  <div class={classes.tooltip}>
                    Please name your study and select a book to create
                  </div>
                )}
              </div>
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
