import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";

import { DateTimeFormatter } from "@js-joda/core";
import { Locale } from "@js-joda/locale_en-us";
import { AdminTopNav } from "../../Components/AdminTopNav/AdminTopNav";
import { match, P } from "ts-pattern";
import { useNavigate } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { PublicUser, Study, StudyRaw, toStudyFromRaw } from "../../Types";

import { LoginUser, loginState } from "../LoginPage/login";

async function getStudies(): Promise<Network.NetworkState<Array<Study>>> {
  return Network.request("/study").then((study) =>
    // This is ugly, need a better way to do this.
    Network.mapNetworkState(study, (s: Array<StudyRaw>) => s.map(toStudyFromRaw))
  );
}

export function AdminArea() {
  const nav = useNavigate();

  //@ts-ignore
  return match(loginState() as LoginUser)
    .with({ state: "notLoggedIn" }, () => {
      nav("/app/login");
      return <> </>;
    })
    .with({ state: "loggedIn" }, ({ user }) => Studies(user))
    .exhaustive();
}

function Studies(me: PublicUser) {
  const [result] = createResource([], () => getStudies(), { initialValue: { state: "loading" } });
  const dummyTemplateData = [
    {
      Title: "1 Chronicles",
      LastEditDate: "Aug 28th, 2023",
      NumOfSections: 12,
    },
    {
      Title: "Obadiah",
      LastEditDate: "Aug 25th, 2023",
      NumOfSections: 22,
    },
    {
      Title: "Jude",
      LastEditDate: "Aug 18th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "Song of Solomon",
      LastEditDate: "Aug 15th, 2023",
      NumOfSections: 12,
    },
    {
      Title: "2 John",
      LastEditDate: "Aug 09th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "1 Chronicles",
      LastEditDate: "Jul 30th, 2023",
      NumOfSections: 20,
    },
    {
      Title: "Micah",
      LastEditDate: "Jul 20th, 2023",
      NumOfSections: 19,
    },
    {
      Title: "Ruth",
      LastEditDate: "Jul 15th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "1 Chronicles",
      LastEditDate: "Jul 07th, 2023",
      NumOfSections: 22,
    },
    {
      Title: "2 Kings",
      LastEditDate: "Jun 30th, 2023",
      NumOfSections: 22,
    },
    {
      Title: "Ecclesiastes",
      LastEditDate: "Jun 27th, 2023",
      NumOfSections: 17,
    },
    {
      Title: "Psalms",
      LastEditDate: "Jun 17th, 2023",
      NumOfSections: 13,
    },
    {
      Title: "Amos",
      LastEditDate: "Jun 11th, 2023",
      NumOfSections: 12,
    },
    {
      Title: "Matthew",
      LastEditDate: "Jun 07th, 2023",
      NumOfSections: 19,
    },
    {
      Title: "Hosea",
      LastEditDate: "Jun 03th, 2023",
      NumOfSections: 13,
    },
    {
      Title: "2 Peter",
      LastEditDate: "May 28th, 2023",
      NumOfSections: 16,
    },
    {
      Title: "Revelation",
      LastEditDate: "May 22th, 2023",
      NumOfSections: 20,
    },
    {
      Title: "1 Peter",
      LastEditDate: "May 14th, 2023",
      NumOfSections: 13,
    },
    {
      Title: "Esther",
      LastEditDate: "May 10th, 2023",
      NumOfSections: 20,
    },
    {
      Title: "Habakkuk",
      LastEditDate: "May 01th, 2023",
      NumOfSections: 13,
    },
    {
      Title: "1 Kings",
      LastEditDate: "Apr 22th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "2 John",
      LastEditDate: "Apr 16th, 2023",
      NumOfSections: 15,
    },
    {
      Title: "Job",
      LastEditDate: "Apr 09th, 2023",
      NumOfSections: 14,
    },
    {
      Title: "Lamentations",
      LastEditDate: "Apr 06th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "Zephaniah",
      LastEditDate: "Apr 03th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "Joel",
      LastEditDate: "Mar 31th, 2023",
      NumOfSections: 23,
    },
    {
      Title: "Haggai",
      LastEditDate: "Mar 21th, 2023",
      NumOfSections: 14,
    },
    {
      Title: "2 John",
      LastEditDate: "Mar 15th, 2023",
      NumOfSections: 14,
    },
    {
      Title: "2 Timothy",
      LastEditDate: "Mar 05th, 2023",
      NumOfSections: 16,
    },
    {
      Title: "Ezra",
      LastEditDate: "Feb 23th, 2023",
      NumOfSections: 12,
    },
    {
      Title: "Proverbs",
      LastEditDate: "Feb 19th, 2023",
      NumOfSections: 14,
    },
    {
      Title: "Amos",
      LastEditDate: "Feb 11th, 2023",
      NumOfSections: 11,
    },
    {
      Title: "1 Kings",
      LastEditDate: "Feb 08th, 2023",
      NumOfSections: 15,
    },
    {
      Title: "Romans",
      LastEditDate: "Feb 01th, 2023",
      NumOfSections: 21,
    },
    {
      Title: "Joel",
      LastEditDate: "Jan 27th, 2023",
      NumOfSections: 16,
    },
    {
      Title: "3 John",
      LastEditDate: "Jan 20th, 2023",
      NumOfSections: 23,
    },
    {
      Title: "Hosea",
      LastEditDate: "Jan 11th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "Proverbs",
      LastEditDate: "Jan 08th, 2023",
      NumOfSections: 18,
    },
    {
      Title: "Exodus",
      LastEditDate: "Jan 05th, 2023",
      NumOfSections: 22,
    },
    {
      Title: "Habakkuk",
      LastEditDate: "Dec 30th, 2022",
      NumOfSections: 15,
    },
    {
      Title: "Philippians",
      LastEditDate: "Dec 26th, 2022",
      NumOfSections: 13,
    },
    {
      Title: "Hosea",
      LastEditDate: "Dec 23th, 2022",
      NumOfSections: 20,
    },
    {
      Title: "2 Timothy",
      LastEditDate: "Dec 13th, 2022",
      NumOfSections: 16,
    },
    {
      Title: "Micah",
      LastEditDate: "Dec 10th, 2022",
      NumOfSections: 17,
    },
    {
      Title: "1 Corinthians",
      LastEditDate: "Dec 07th, 2022",
      NumOfSections: 18,
    },
    {
      Title: "Isaiah",
      LastEditDate: "Nov 29th, 2022",
      NumOfSections: 16,
    },
    {
      Title: "Galatians",
      LastEditDate: "Nov 21th, 2022",
      NumOfSections: 14,
    },
    {
      Title: "Song of Solomon",
      LastEditDate: "Nov 14th, 2022",
      NumOfSections: 19,
    },
    {
      Title: "Joel",
      LastEditDate: "Nov 09th, 2022",
      NumOfSections: 19,
    },
    {
      Title: "2 Timothy",
      LastEditDate: "Oct 31th, 2022",
      NumOfSections: 15,
    },
    {
      Title: "Hosea",
      LastEditDate: "Oct 27th, 2022",
      NumOfSections: 22,
    },
    {
      Title: "Esther",
      LastEditDate: "Oct 23th, 2022",
      NumOfSections: 15,
    },
    {
      Title: "Luke",
      LastEditDate: "Oct 20th, 2022",
      NumOfSections: 17,
    },
    {
      Title: "3 John",
      LastEditDate: "Oct 11th, 2022",
      NumOfSections: 17,
    },
    {
      Title: "Lamentations",
      LastEditDate: "Oct 05th, 2022",
      NumOfSections: 20,
    },
    {
      Title: "Mark",
      LastEditDate: "Sep 26th, 2022",
      NumOfSections: 23,
    },
    {
      Title: "Mark",
      LastEditDate: "Sep 18th, 2022",
      NumOfSections: 23,
    },
    {
      Title: "1 Kings",
      LastEditDate: "Sep 12th, 2022",
      NumOfSections: 21,
    },
    {
      Title: "1 Samuel",
      LastEditDate: "Sep 08th, 2022",
      NumOfSections: 23,
    },
    {
      Title: "Jude",
      LastEditDate: "Sep 03th, 2022",
      NumOfSections: 17,
    },
  ];
  return (
    <>
      <AdminTopNav />
      <div class={classes.studiesPage}>
        <div class={classes.pageHeader}>
          <h1>Study Templates</h1>
          <a href="/app/trash">View Trash</a>
        </div>
        <table class={classes.tableContainer}>
          <thead>
            <tr>
              <th>Template Title</th>
              <th>Last edit date</th>
              <th>Number of Sections</th>
            </tr>
          </thead>
          <tbody>
            {
              // @ts-ignore
              match(result())
                .with({ state: "loading" }, () => <>loading</>)
                .with({ state: "error" }, ({ body }) => <>error</>)
                .with({ state: "success" }, ({ body }) => (
                  // <For each={body}>{(study) => <ViewStudy study={study} me={me} />}</For>
                  // Temporarily used dummy data
                  <For each={dummyTemplateData}>
                    {(study) => (
                      <tr
                        class={classes.tableRow}
                        onClick={() => (window.location.href = "/details")}
                      >
                        <td>{study.Title}</td>
                        <td>{study.LastEditDate}</td>
                        <td>{study.NumOfSections}</td>
                      </tr>
                    )}
                  </For>
                ))
                .with({ state: "notloaded" }, () => <>not loaded</>)
                .exhaustive()
            }
          </tbody>
        </table>
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
  me: PublicUser;
};

const dtFormat = DateTimeFormatter.ofPattern("MMMM d, yyyy").withLocale(Locale.US);

function ViewStudy(props: ViewStudyProps) {
  console.log("test");
  const nav = useNavigate();
  const peoples = props.study.docs.flatMap((doc) =>
    doc.editors.filter((e) => e.userId != props.me.userId).map((e) => e.name)
  );
  let shared = "no one";
  if (peoples.length > 0) {
    // I think this should intersperse, I haven't tried it. Should probably put
    // this in a utilities file somewhere.
    shared = peoples.slice(1).reduce((prev, cur) => cur + ", " + prev, peoples[0]);
  }
  const myDoc = props.study.docs.find((doc) =>
    doc.editors.some((e) => e.userId == props.me.userId)
  );
  const updated = myDoc.updated.format(dtFormat);
  return (
    <tr class={classes.tableRow} onClick={() => (window.location.href = "/details")}>
      <td>{props.study.name}</td>
      <td>{shared}</td>
      <td>{updated}</td>
    </tr>
  );
}
