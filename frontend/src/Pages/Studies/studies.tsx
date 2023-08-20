import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { LoggedInTopNav } from "../../Components/LoggedInTopNav/LoggedInTopNav";
import { match, P } from "ts-pattern";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { Study } from "../../Types";

import RedIcon from "../../Assets/red-icon.svg";
import GreenIcon from "../../Assets/green-icon.svg";
import YellowIcon from "../../Assets/yellow-icon.svg";
import BlueIcon from "../../Assets/blue-icon.svg";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

async function getStudies(): Promise<Network.NetworkState<Array<Study>>> {
  return Network.request("/study");
}

export function StudiesPage() {
  console.log("test");
  const [result] = createResource([], () => getStudies(), { initialValue: { state: "loading" } });

  interface Study {
    "Study Title": string;
    Book: string;
    "Shared with": string;
    datestamp: string;
  }

  const studiesData: { studies: Study[] } = {
    studies: [
      {
        "Study Title": "Men’s Wednesday Bible Study",
        Book: "Genesis",
        "Shared with": "John D, Alice M, Robert T",
        datestamp: "Dec 15, 2022",
      },
      {
        "Study Title": "Solo Titus Study",
        Book: "Exodus",
        "Shared with": "Michael S",
        datestamp: "Dec 14, 2022",
      },
      {
        "Study Title": "Sunday Group Study",
        Book: "Leviticus",
        "Shared with": "Sarah J, Emily R",
        datestamp: "Dec 13, 2022",
      },
      {
        "Study Title": "Youth Proverbs Exploration",
        Book: "Numbers",
        "Shared with": "",
        datestamp: "Dec 12, 2022",
      },
      {
        "Study Title": "Women’s Psalms Reflection",
        Book: "Deuteronomy",
        "Shared with": "Anna L, Lisa K, Paul B",
        datestamp: "Dec 11, 2022",
      },
      {
        "Study Title": "Couples’ Ecclesiastes Dive",
        Book: "Joshua",
        "Shared with": "Steve P, Nancy W",
        datestamp: "Dec 10, 2022",
      },
      {
        "Study Title": "Elders’ Isaiah Review",
        Book: "Judges",
        "Shared with": "George H, Karen D, Tom R",
        datestamp: "Dec 9, 2022",
      },
      {
        "Study Title": "Kids’ Daniel Adventure",
        Book: "Ruth",
        "Shared with": "Liam G",
        datestamp: "Dec 8, 2022",
      },
      {
        "Study Title": "Men’s Romans Deep Dive",
        Book: "1 Samuel",
        "Shared with": "Oscar F, Zoe A, Ivan L",
        datestamp: "Dec 7, 2022",
      },
      {
        "Study Title": "Ladies’ Revelation Exploration",
        Book: "2 Samuel",
        "Shared with": "Grace T, Ella V",
        datestamp: "Dec 6, 2022",
      },
      {
        "Study Title": "Teens’ Acts Journey",
        Book: "1 Kings",
        "Shared with": "Lucas M, Mia S, Noah Z",
        datestamp: "Dec 5, 2022",
      },
      {
        "Study Title": "Solo John Reflection",
        Book: "2 Kings",
        "Shared with": "",
        datestamp: "Dec 4, 2022",
      },
      {
        "Study Title": "Wednesday Mark Study",
        Book: "1 Chronicles",
        "Shared with": "Sophia C",
        datestamp: "Dec 3, 2022",
      },
      {
        "Study Title": "Thursday Luke Exploration",
        Book: "2 Chronicles",
        "Shared with": "Benjamin E, Ava D",
        datestamp: "Dec 2, 2022",
      },
      {
        "Study Title": "Friday Matthew Dive",
        Book: "Ezra",
        "Shared with": "Oliver H, Amelia B, Charlie J",
        datestamp: "Dec 1, 2022",
      },
    ],
  };

  const [studies, setStudies] = createSignal(studiesData.studies);

  function getBookIcon(book: string) {
    switch (book) {
      case "Amos":
      case "1 Chronicles":
      case "2 Chronicles":
      case "Daniel":
      case "Deuteronomy":
      case "Ecclesiastes":
      case "Esther":
      case "Exodus":
      case "Ezekiel":
      case "Ezra":
      case "Genesis":
      case "Habakkuk":
        return <img src={RedIcon} />;
      case "Haggai":
      case "Hosea":
      case "Isaiah":
      case "Jeremiah":
      case "Job":
      case "Joel":
      case "Jonah":
      case "Joshua":
      case "Judges":
      case "Lamentations":
      case "Leviticus":
      case "Malachi":
      case "Micah":
      case "Nahum":
      case "Nehemiah":
      case "Numbers":
      case "Obadiah":
      case "Proverbs":
      case "Psalms":
      case "Ruth":
      case "1 Samuel":
      case "2 Samuel":
      case "Song of Solomon":
      case "Zechariah":
        return <img src={BlueIcon} />;
      case "Zephaniah":
      case "1 Corinthians":
      case "2 Corinthians":
      case "Colossians":
      case "1 John":
      case "2 John":
      case "3 John":
      case "Acts":
      case "Ephesians":
      case "Galatians":
      case "Hebrews":
      case "James":
        return <img src={GreenIcon} />;
      case "John":
      case "Jude":
      case "Luke":
      case "Mark":
      case "Matthew":
      case "1 Peter":
      case "2 Peter":
      case "Philemon":
      case "Philippians":
      case "Revelation":
      case "Romans":
      case "1 Thessalonians":
      case "2 Thessalonians":
      case "1 Timothy":
      case "2 Timothy":
      case "Titus":
        return <img src={YellowIcon} />;
      // ... other cases for other books ...
      default:
        return <img src={YellowIcon} />; // some default icon
    }
  }
  return (
    <>
      <LoggedInTopNav />
      <div class={classes.studiesPage}>
        <div class={classes.pageHeader}>
          <h1>My Studies</h1>
          <a href="/app/trash">View Trash</a>
        </div>
        <table class={classes.tableContainer}>
          <thead>
            <tr>
              <th>Study Title</th>
              <th>Book</th>
              <th>Shared with</th>
              <th>Last Opened by Me</th>
            </tr>
          </thead>
          <tbody>
            {studies().map((study) => (
              <tr class={classes.tableRow} onClick={() => (window.location.href = "/details")}>
                <td>{study["Study Title"]}</td>
                <td class={classes.bibleBook}>
                  {getBookIcon(study.Book)}
                  {study.Book}
                </td>
                <td class={classes.sharedWith}>{study["Shared with"]}</td>
                <td>{study.datestamp}</td>
              </tr>
            ))}
          </tbody>
        </table>
        {/* {
          // @ts-ignore
          match(result())
            .with({ state: "loading" }, () => <>loading</>)
            .with({ state: "error" }, ({ body }) => <>error</>)
            .with({ state: "success" }, ({ body }) => (
              <div>
                <For each={body}>{(study) => <ViewStudy study={study} />}</For>
              </div>
            ))
            .with({ state: "notloaded" }, () => <>not loaded</>)
            .exhaustive()
        } */}
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
};

function ViewStudy(props: ViewStudyProps) {
  return <div>{props.study.name}</div>;
}
