import { match } from "ts-pattern";

export type NetworkLoading = {
  state: "loading";
};

export const notLoaded : NetworkNotLoaded = { state: "notloaded" };

export type NetworkNotLoaded = {
  state: "notloaded";
};

export type NetworkError420NotFound = {
  status: 420;
  error: "NotFound";
  content: {
    model: string;
    id: string;
  };
};

export type NetworkError420AuthError = {
  status: 420;
  error: "AuthError";
};

export type NetworkError420Other = {
  status: 420;
  error: string;
  content: any;
};

export type NetworkErrorInternal = {
  status: 500;
};

export type NetworkError400 = {
  status: 400;
  body : string;
};

export type NetworkErrorCookie = {
  status: 401;
};

export type NetworkError = {
  state: "error";
  body: NetworkError420Other
        | NetworkError420NotFound
        | NetworkError420AuthError
        | NetworkError400
        | NetworkErrorInternal
        | NetworkErrorCookie
};

export type NetworkSuccess<t> = {
  state: "success";
  body: t
};

export type NetworkState<t> =
 | NetworkNotLoaded
 | NetworkLoading
 | NetworkError
 | NetworkSuccess<t>

export function mapNetworkState<a, b>(v : NetworkState<a>, f : (a: a) => b) : NetworkState<b> {
  switch (v.state) {
    case "success":
      return {
        state: "success",
        body: f(v.body),
      };
    case "notloaded":
      return v;
    case "error":
      return v;
    case "loading":
      return v;
  }
}

export const baseUrl = "/api";

export type SimpleNetworkState<t> =
  | NetworkSuccess<t>
  | NetworkError

export async function request<t>(url : string, opts = {})
  : Promise<SimpleNetworkState<t>> {
  const response = await fetch(baseUrl + url, opts);
  const status = response.status;
  if (status == 204) {
    return {
      state: "success",
      body: null,
    };
  }
  if (status >= 200 && status < 300) {
    const body = await response.json();
    return {
      state: "success",
      body: body,
    };
  }

  if (status == 420) {
    const body = await response.json();
    return {
      state: "error",
      body: {
        status: 420,
        error: body.error,
        content: body.content,
      },
    };
  }

  if (status == 400) {
    const body = await response.text();
    return {
      state: "error",
      body: {
        status: 400,
        body: body,
      },
    };
  }

  if (status == 401) {
    return {
      state: "error",
      body: {
        status: 401,
      },
    };
  }
}

