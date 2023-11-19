import * as T from "Types";

export type SendOpenDoc = {
  tag: "OpenDoc",
  contents: T.DocId,
};

export type SendUpdated = {
  tag: "Updated",
  contents: any,
};

export type SendListenToDoc = {
  tag: "ListenToDoc",
  contents: T.DocId,
};

export type SendMsg =
  | SendOpenDoc
  | SendUpdated
  | SendListenToDoc


export function sendMsg (ws : WebSocket, msg : SendMsg) {
  if(ws.readyState == 1) {
    ws.send(JSON.stringify(msg));
  }
};



export type RecDocListenStart = {
  tag: "DocListenStart",
  contents: any,
};

export type RecDocUpdated = {
  tag: "DocUpdated",
  contents: any,
};

export type RecNotFound = {
  tag: "NotFound",
};

export type RecUnauthorized = {
  tag: "Unauthorized",
};

export type RecParseError = {
  tag: "ParseError",
};

export type RecMsg =
  | RecDocListenStart
  | RecDocUpdated
  | RecNotFound
  | RecUnauthorized
  | RecParseError
