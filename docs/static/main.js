

window.addEventListener('load', function () {
  const titles = document.getElementsByClassName("gdoc-brand__title");

  const title = titles[0]

  const subtitle = document.createElement('div');

  subtitle.classList.add("_subtitle");

  title.appendChild(subtitle);

  subtitle.innerHTML = "Learn once, run anywhere";
});
