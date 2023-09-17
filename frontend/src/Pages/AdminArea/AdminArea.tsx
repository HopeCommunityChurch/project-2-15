import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";

import { DateTimeFormatter } from "@js-joda/core";
import { Locale } from "@js-joda/locale_en-us";
import { AdminTopNav } from "./AdminTopNav/AdminTopNav";
import { match, P } from "ts-pattern";
import { useNavigate } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import * as T from "../../Types";

import { LoginUser, loginState } from "../LoginPage/login";


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

function Studies(me: T.PublicUser) {
  return (
    <div> not built yey</div>
  );
}

