
// document.getElementById("createPeople").addEventListener("input", (ev : InputEvent) => {
//   const value : String = ev.target.value;
//   if(value.length > 3) {
//     const lastThing = value.at(value.length-1);
//     if(lastThing == " " || lastThing == ",") {
//       const email = value.substring(0, value.length-1)

//       const person = document.createElement("div");
//       person.className = "person";
//       person.innerHTML = email;

//       const img = document.createElement("img");
//       img.src = "/static/img/x.svg";
//       img.addEventListener("click", () => {
//         person.parentElement.removeChild(person);
//       });
//       person.appendChild(img);

//       const input = document.createElement("input");
//       input.type = "hidden";
//       input.value = email;
//       input.name = "email[]";
//       person.appendChild(input);

//       document.getElementById("createPeoples").appendChild(person);
//       ev.target.value = "";
//     }
//   }
// });
