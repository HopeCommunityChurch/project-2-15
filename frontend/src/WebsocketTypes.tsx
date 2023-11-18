import * as T from "Types";

export type SendOpenDoc = {
  tag: "OpenDoc",
  content: T.DocId,
};

export type SendUpdated = {
  tag: "Updated",
  content: any,
};

export type SendListenToDoc = {
  tag: "ListenToDoc",
  content: T.DocId,
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
  content: any,
};

export type RecDocUpdated = {
  tag: "DocUpdated",
  content: any,
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
