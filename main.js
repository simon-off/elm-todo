import "./scss/main.scss";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });
