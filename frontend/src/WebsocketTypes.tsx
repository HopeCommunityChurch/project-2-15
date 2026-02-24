import * as T from "Types";
import { when } from "ts-pattern/dist/patterns";

export type SendOpenDoc = {
  tag: "OpenDoc",
  contents: T.DocId,
};

export type SendUpdated = {
  tag: "Updated",
  contents: {
    version: number,
    steps: any[],
    clientId: string,
    selection?: { anchor: number, head: number },
  },
};

export type SendListenToDoc = {
  tag: "ListenToDoc",
  contents: T.DocId,
};

export type SendStopListenToDoc = {
  tag: "StopListenToDoc",
  contents: T.DocId,
};


export type SendSaveDoc = {
  tag: "SaveDoc",
  contents: {
    document: any,
  },
};

export type SendUpdateName = {
  tag: "UpdateName",
  contents: string,
};


export type SendMsg =
  | SendOpenDoc
  | SendUpdated
  | SendListenToDoc
  | SendStopListenToDoc
  | SendSaveDoc
  | SendUpdateName



type RecDocListenStart = {
  tag: "DocListenStart",
  contents: any,
};

type RecDocOpened = {
  tag: "DocOpened",
  contents: any,
};

type RecDocOpenedOther = {
  tag: "DocOpenedOther",
};


type RecDocUpdated = {
  tag: "DocUpdated",
  contents: { version: number, steps: any[], clientIds: string[], docId: string },
};

type RecDocConfirmed = {
  tag: "DocConfirmed",
  contents: { version: number, steps: any[], clientIds: string[] },
};

type RecDocConflict = {
  tag: "DocConflict",
  contents: { steps: any[], clientIds: string[] },
};

type RecDocSaved = {
  tag: "DocSaved",
  contents: {
    time: string,
  },
};


type RecDocNameUpdated = {
  tag: "DocNameUpdated",
};


type RecNotFound = {
  tag: "NotFound",
};

type RecUnauthorized = {
  tag: "Unauthorized",
};

type RecParseError = {
  tag: "ParseError",
};

type RecMsg =
  | RecDocListenStart
  | RecDocUpdated
  | RecDocConfirmed
  | RecDocConflict
  | RecDocSaved
  | RecDocOpenedOther
  | RecDocOpened
  | RecDocNameUpdated
  | RecNotFound
  | RecUnauthorized
  | RecParseError


export class WsOpenEvent extends Event {
  constructor() {
    super("open");
  }
}

export class DocListenStartEvent extends Event {
  document: any;
  version: number;
  snapVersion: number;
  pendingSteps: any[];
  pendingClientIds: string[];
  constructor(contents: { document: any, version: number, snapVersion: number,
                           pendingSteps?: any[], pendingClientIds?: string[] }) {
    super("DocListenStart");
    this.document = contents.document;
    this.version = contents.version;
    this.snapVersion = contents.snapVersion;
    this.pendingSteps = contents.pendingSteps ?? [];
    this.pendingClientIds = contents.pendingClientIds ?? [];
  }
}


export class DocSavedEvent extends Event {
  time : string;
  constructor(time : string) {
    super("DocSaved");
    this.time = time;
  }
}

export class DocUpdatedEvent extends Event {
  update: { version: number, steps: any[], clientIds: string[], docId: string };
  constructor(update: { version: number, steps: any[], clientIds: string[], docId: string }) {
    super("DocUpdated");
    this.update = update;
  }
}

export class DocOpenedEvent extends Event {
  doc : T.DocRaw;
  snapVersion : number;
  pendingSteps : any[];
  pendingClientIds : string[];
  constructor(contents : { doc: T.DocRaw, snapVersion: number, pendingSteps?: any[], pendingClientIds?: string[] }) {
    super("DocOpened");
    this.doc = contents.doc;
    this.snapVersion = contents.snapVersion;
    this.pendingSteps = contents.pendingSteps ?? [];
    this.pendingClientIds = contents.pendingClientIds ?? [];
  }
}

export class DocNameUpdated extends Event {
  constructor() {
    super("DocNameUpdated");
  }
}

export class DocConfirmedEvent extends Event {
  payload: { version: number, steps: any[], clientIds: string[] };
  constructor(payload: { version: number, steps: any[], clientIds: string[] }) {
    super("DocConfirmed");
    this.payload = payload;
  }
}

export class DocConflictEvent extends Event {
  payload: { steps: any[], clientIds: string[] };
  constructor(payload: { steps: any[], clientIds: string[] }) {
    super("DocConflict");
    this.payload = payload;
  }
}



export class DocOpenedOtherEvent extends Event {
  constructor() {
    super("DocOpenedOther");
  }
}

export class ClosedEvent extends Event {
  constructor() {
    super("closed");
  }
}




export class MyWebsocket extends EventTarget {
  host : string;
  protocol : string;
  ws : WebSocket;
  openedDoc : any;

  constructor() {
    super();
    this.ws = null;
    this.host = location.host;
    this.openedDoc = null;
    this.protocol = window.isLocal == 1 ? "ws://" : "wss://";
  }

  connect () {
    if(this.ws != null) {
      if(this.ws.readyState == 1) {
        return;
      }
      try {
        this.ws.close();
      } catch (err) {
        console.error(err)
      }
    }
    this.ws = new WebSocket(this.protocol + this.host + "/api/document/realtime");
    this.ws.onopen = (e) => {
      console.log("ws open", e);
      let event = new WsOpenEvent();
      this.dispatchEvent(event);
    };

    this.ws.onclose = (e) => {
      let event = new ClosedEvent();
      this.dispatchEvent(event);
      if(!e.wasClean){
        setTimeout( () => {
          this.ws = null;
          this.connect();
        }, 1000); // wait 1 second before trying to connect again.
      }
    }

    this.ws.onmessage = (msg : MessageEvent<string>) => {
      const rec = JSON.parse(msg.data) as RecMsg;
      switch (rec.tag) {
        case "DocListenStart": {
          let event = new DocListenStartEvent(rec.contents);
          this.dispatchEvent(event);
          break;
        }
        case "DocSaved": {
          let event = new DocSavedEvent(rec.contents.time);
          this.dispatchEvent(event);
          break;
        }
        case "DocUpdated": {
          let event = new DocUpdatedEvent(rec.contents);
          this.dispatchEvent(event);
          break;
        }
        case "DocConfirmed": {
          let event = new DocConfirmedEvent(rec.contents);
          this.dispatchEvent(event);
          break;
        }
        case "DocConflict": {
          let event = new DocConflictEvent(rec.contents);
          this.dispatchEvent(event);
          break;
        }
        case "DocOpened": {
          let event = new DocOpenedEvent(rec.contents);
          this.dispatchEvent(event);
          break;
        }
        case "DocOpenedOther": {
          let event = new DocOpenedOtherEvent();
          this.dispatchEvent(event);
          break;
        }
        case "DocNameUpdated": {
          let event = new DocNameUpdated();
          this.dispatchEvent(event);
          break;
        }
      }
    };
  }

  close () {
    if(this.ws != null) {
      if(this.ws.readyState == 1) {
        console.log("closing websocket");
        this.ws.close();
      }
    }
  }

  openDoc(docId : T.DocId) {
    if(this.openedDoc != null) {
      console.log("needs to be built");
    }
    this.openedDoc = docId;
    this.send({ tag: "OpenDoc", contents: docId});
  }

  sendStrRaw (msg:string) {
    if(this.ws.readyState == 1) {
      this.ws.send(msg);
    }
  }

  send (msg : SendMsg) {
    if(this.ws.readyState == 1) {
      this.ws.send(JSON.stringify(msg));
    }
  };
}
