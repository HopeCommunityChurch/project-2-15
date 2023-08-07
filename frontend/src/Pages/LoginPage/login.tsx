
import { createSignal } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import * as classes from "./styles.module.scss";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;


export function LoginPage() {
  const [login, setLogin] = createSignal(false);
  return (
    <>
      <PreLoginTopNav />
      <div>
        login: <input /> <br />
        password: <input /> <br />
        <Button type="lightBlue">Login</Button>
      </div>
    </>
  );
}
