import {body} from "./jonathanMeme/index.tsx";
import ReactDOM from "react-dom";
console.log("hello world");

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(<h1 className={body} >Hello, world!</h1>);

console.log(body);
