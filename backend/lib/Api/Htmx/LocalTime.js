const monthsFull = [
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December",
];

document.addEventListener('alpine:init', () => {
  Alpine.directive('localtime', (el) => {
    let str = el.textContent;
    str = str.replace(" UTC", "");
    const dt = new Date(str);
    console.log(dt);
    const m = monthsFull[dt.getMonth()];
    el.textContent = m + " " + dt.getDay() + ", " + dt.getFullYear();
  });
});
