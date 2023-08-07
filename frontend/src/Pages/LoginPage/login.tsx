
import { createEffect, createSignal } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;


export function LoginPage() {
  const initialLoginStatue = {
    state: "loading"
  };
  const [login, setLogin] = createSignal(initialLoginStatue);
  const [email, setEmail] = createSignal("");
  const [password, setPassword] = createSignal("");

  const loginPushed = () => {
    Network.request("/auth/password", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        body: JSON.stringify({
          email: email(),
          password: password(),
        }),
      },
    }).then( (result) => {
      console.log(result);
    }).catch( (err) => {
      console.log(err);
    });
  };

  return (
    <>
      <PreLoginTopNav />
      <div>
        login: <input onKeyUp={e => setEmail(e.currentTarget.value)}/> <br />
        password: <input onKeyUp={e => setPassword(e.currentTarget.value)}/> <br />
        <Button type="lightBlue" onClick={loginPushed} >Login</Button>
      </div>
    </>
  );
}
