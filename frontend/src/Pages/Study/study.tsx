import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { match, P } from "ts-pattern";
import { useParams } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { Study } from "../../Types";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

export function StudyPage() {
  const params = useParams();
  const documentID = params.documentID;
  console.log(documentID);
  return <>xdfg</>;
}

type ViewStudyProps = {
  study: Study;
};

function ViewStudy(props: ViewStudyProps) {
  return <div>{props.study.name}</div>;
}
