window.addEventListener('load', function () {
  var subtitle = document.getElementsByClassName("_subtitle");

  // In case adding more than once!
  if (subtitle.length != 0)
    return;

  // Place it here to avoid error: `Identifier 'xxx' has already been declared`.
  const quote = "Learn once, run anywhere";

  const titles = document.getElementsByClassName("gdoc-brand__title");

  const title = titles[0]

  title.innerHTML += '<div class="_subtitle">' + quote + '</div>';
});
