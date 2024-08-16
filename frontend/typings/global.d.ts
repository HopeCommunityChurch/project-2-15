declare module "*.css";
declare module "*.scss";
declare module "*.jpg";
declare module "*.json";
declare module "*.svg" {
  const content: any;
  export default content;
}
declare module "*.png" {
  const value: any;
  export default value;
}
declare var process : {
  env : {
    [key: string] : string;
  }
}

declare interface Window {
  editor: any;
  editorActions: any;
  base: string;
  previewScripture: any;
  isLocal: number;
}
