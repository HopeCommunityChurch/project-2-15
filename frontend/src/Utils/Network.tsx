
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

export type NetworkError = {
  state: "error";
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


export async function request<t>(url : URL, opts = {}) : Promise<NetworkSuccess<t> | NetworkError> {
  const response = await fetch(url, opts);
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
}
