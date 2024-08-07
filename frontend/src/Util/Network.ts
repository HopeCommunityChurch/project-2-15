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

export type NetworkError420DocumentUpdate = {
  status: 420;
  error: "DocumentUpdatedNotMatch";
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

export type NetworkError404 = {
  status: 404;
};

export type NetworkErrorOther = {
  status: number;
  body: string;
};

export type NetworkErrorCookie = {
  status: 401;
};

export type NetworkError = {
  state: "error";
  body: NetworkError420Other
        | NetworkError420NotFound
        | NetworkError420AuthError
        | NetworkError420DocumentUpdate
        | NetworkError400
        | NetworkError404
        | NetworkErrorInternal
        | NetworkErrorCookie
        | NetworkErrorOther
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

export async function request<t>(url : string, opts:any = {})
  : Promise<SimpleNetworkState<t>> {
  let timeout = null;
  if(opts.signal == null) {
    const timeoutTime = 8000;
    const controller = new AbortController();
    timeout = setTimeout( () => controller.abort(), timeoutTime);
    opts.signal = controller.signal;
  }
  const response = await fetch(baseUrl + url, opts);
  if (timeout != null) {
    clearTimeout(timeout);
  }
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

  if (status == 404) {
    return {
      state: "error",
      body: {
        status: 404,
      },
    };
  }

  if (status >= 400 && status < 600) {
    const body = await response.text();
    return {
      state: "error",
      body: {
        status: status,
        body: body
      },
    };
  }

}
