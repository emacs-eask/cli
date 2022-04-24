---
title: ✒️ Documentation
weight: 30
---

{{< toc >}}

Eask includes a comprehensive user guide. Please try to extend it accordingly while
you implement new features.

The documentation is written in [Markdown](https://gohugo.io/), using [Hugo]() and GitHub Pages.
The former is the static site generator, and the latter is the static web pages hosting service
from GitHub.

{{< hint info >}}
💡 *You can find all our documentation under the `docs/content/{language}/` folder.*
{{< /hint >}}

### 🚩 Prerequisites

To make changes to documentation, you should have:

* [hugo](https://gohugo.io/getting-started/quick-start/#step-1-install-hugo)
executable; the static site generator.

### 📐 Setup

To setup the website locally, you need to first navigate to the `docs/` folder.

```sh
$ cd path/to/eask/docs/
```

Then run the `hugo` command:

```sh
$ hugo server
```

And that's it! Now you can open the browser with the url `localhost:1313`.
