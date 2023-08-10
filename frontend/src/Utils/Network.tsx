
export type NetworkLoading = {
  state: "loading";
};

export type NetworkNotLoaded = {
  state: "NotLoaded";
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

export const baseUrl = "/api";

export async function request<t>(url : string, opts = {})
  : Promise<NetworkSuccess<t> | NetworkError> {
  const response = await fetch(baseUrl + url, opts);
  console.log(response);
  const status = response.status;
  if (status == 204) {
    return {
      state: "success",
      body: null,
    };
  }
  console.log(status);
  if (status >= 200 && status < 300) {
    const body = await response.json();
    return {
      state: "success",
      body: body,
    };
  }

  console.log(status);
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

  console.log(status);
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

  console.log(status);
  if (status == 401) {
    return {
      state: "error",
      body: {
        status: 401,
      },
    };
  }
}

