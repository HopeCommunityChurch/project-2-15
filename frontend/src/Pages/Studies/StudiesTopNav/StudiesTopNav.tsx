import Logo from "Assets/p215.png";
import { Button } from "Components/Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { PublicUser, GroupStudyRaw } from "Types";
import * as T from "Types";
import SearchIcon from "Assets/magnifying_glass.svg";
import CloseXIcon from "Assets/x.svg";
import PadlockIcon from "Assets/padlock.svg";
import ArrowIcon from "Assets/arrow.svg";
import NotificationBell from "Assets/notification-bell.svg";
import * as Network from "Utils/Network";
import { handleLogout } from "Pages/LoginPage/login";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "Hooks/useOutsideClickClose";

export type StudiesTopNavProps = {
  currentUser: PublicUser;
};

export function StudiesTopNav(props: StudiesTopNavProps) {
  const [showDropdown, setShowDropdown] = createSignal(false);
  const [showModal, setShowModal] = createSignal(false);
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

  // Helper function to add or remove a study item from the selectedStudyItems array

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

  function logoutClick(e: MouseEvent) {
    e.preventDefault();
    handleLogout().then(() => {
      nav("/app/login");
    });
  }

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
          <div class={classes.profileButton} onClick={() => setShowDropdown(!showDropdown())}>
            {props.currentUser.name.slice(0, 2).toUpperCase()}
          </div>
          <div class={`${classes.profileDropdown} ${showDropdown() ? classes.showDropdown : ""}`}>
            <ul>
              <li class={classes.mobileOnlyItem}>
                <Button type="Blue" onClick={() => setShowModal(true)}>
                  + New Study
                </Button>
              </li>
              {/* <li>
                <a href="/app/account" class={classes.fullWidthLink}>
                  My Account
                </a>
              </li> */}
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
        </div>
      </header>
      <Show when={showModal()}>
        <AddStudy setShowModal={setShowModal} currentUser={props.currentUser} />
      </Show>
      <div class={classes.headerThingy}></div>
    </>
  );
}

type AddStudyProp = {
  setShowModal: (v: Boolean) => void;
  currentUser: PublicUser;
};

function AddStudy(prop: AddStudyProp) {
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

  const setShowModal = prop.setShowModal;
  const [studyTitleValue, setStudyTitleValue] = createSignal("");
  const [selectedBook, setSelectedBook] = createSignal(null);
  const [studyBookValue, setStudyBookValue] = createSignal("");
  const [focusedBookIndex, setFocusedBookIndex] = createSignal(-1);
  const [studyBlockValue, setStudyBlockValue] = createSignal("");
  const [showTooltip, setShowTooltip] = createSignal(false);
  const [selectedStudyItems, setSelectedStudyItems] = createSignal(initialStudyItems);

  const [showStudyBlockDropdown, setShowStudyBlockDropdown] = createSignal(false);

  const filteredBooks = () => {
    return books
      .filter((book) => book.toLowerCase().includes(studyBookValue().toLowerCase()))
      .slice(0, 5);
  };

  const filteredStudyBlockItems = () => {
    return studyBlockItems.filter(
      (item) =>
        item.Name.toLowerCase().includes(studyBlockValue().toLowerCase()) ||
        item.Description.toLowerCase().includes(studyBlockValue().toLowerCase())
    );
  };

  const currentDate = new Date();
  const formattedDate =
    currentDate.toLocaleString("default", { month: "long" }) + " " + currentDate.getFullYear();

  const selectBook = (book) => {
    setSelectedBook(book);
    setStudyBookValue("");
    if (studyTitleValue().trim() === "") {
      setStudyTitleValue(`${selectedBook() ? selectedBook() : "Romans"} ${formattedDate} Study`);
    }
  };

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
    useClickOutsideClose(
      showStudyBlockDropdown,
      setShowStudyBlockDropdown,
      classes.studyBlockDropdownContainer,
      ["preventClose"]
    );
  });

  const bookSelectOnKey = (e: KeyboardEvent) => {
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
  };

  const bookSelectOnInput = (e: InputEvent & { target: HTMLInputElement }) => {
    setStudyBookValue(e.target.value);
    if (filteredBooks().length > 0) {
      setFocusedBookIndex(0);
    } else {
      setFocusedBookIndex(-1); // Reset index when there are no matches
    }
  };

  const nav = useNavigate();

  function createStudySubmitted(e: Event) {
    e.preventDefault();
    if (studyTitleValue().trim() === "") {
      return;
    }
    apiCreateDocument({
      name: studyTitleValue(),
    }).then((r) => {
      //@ts-ignore
      match(r)
        .with({ state: "error" }, (err) => console.log(r))
        .with({ state: "success" }, ({ body }) => {
          const docId = body.docId;
          nav("/app/study/" + docId);
        })
        .exhaustive();
    });
  }

  return (
    <div class={classes.modalBackground} onClick={() => setShowModal(false)}>
      <div class={classes.modal} onClick={(e) => e.stopPropagation()}>
        <h3>Create New Study</h3>
        <img
          src={CloseXIcon}
          alt="Close Modal"
          class={classes.closeModalIcon}
          onClick={() => setShowModal(false)}
        />
        <form onSubmit={(e) => createStudySubmitted(e)}>
          <label for="studyTitle">Title</label>
          <input
            type="text"
            id="studyTitle"
            placeholder="New Title..."
            value={studyTitleValue()}
            onInput={(e) => setStudyTitleValue(e.target.value)}
          />
          <p class={classes.fieldDescription}>
            Ex: "<em>Romans {formattedDate} Study</em>" or "
            <em>Wednesday Night Colossians Study</em>"
          </p>
          <div
            class={classes.tooltipContainer}
            onMouseEnter={() => {
              if (studyTitleValue().trim() === "") {
                setShowTooltip(true);
              }
            }}
            onMouseLeave={() => setShowTooltip(false)}
          >
            <button
              type="submit"
              disabled={studyTitleValue().trim() === ""}
              onClick={createStudySubmitted}
              class={studyTitleValue().trim() === "" ? classes.disabledButton : ""}
            >
              Create
            </button>
            {showTooltip() && <div class={classes.tooltip}>Please name your study to create</div>}
          </div>
        </form>
      </div>
    </div>
  );
}

type CreateStudy = {
  name: string;
};

type ApiCreateDocument = {
  name: string;
  document: any;
  studyTemplateId?: T.StudyTemplateId;
};

async function apiCreateDocument(
  crStudy: CreateStudy
): Promise<Network.SimpleNetworkState<T.DocRaw>> {
  const body: ApiCreateDocument = {
    name: crStudy.name,
    document: blankDoc,
  };
  return Network.request("/document", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });
}

const blankDoc = {
  type: "doc",
  content: [
    {
      type: "section",
      content: [
        { type: "sectionHeader", content: [{ text: "Untitled", type: "text" }] },
        { type: "studyBlocks", content: [{ type: "questions" }] },
      ],
    },
  ],
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
