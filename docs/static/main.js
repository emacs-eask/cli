
const quote = "Learn once, run anywhere";

window.addEventListener('load', function () {
  var subtitle = document.getElementsByClassName("_subtitle");

  // In case adding more than once!
  if (subtitle.length != 0)
    return;

  const titles = document.getElementsByClassName("gdoc-brand__title");

  const title = titles[0]

  title.innerHTML += '<div class="_subtitle">' + quote + '</div>';
});
