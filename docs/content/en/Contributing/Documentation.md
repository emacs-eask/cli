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
💡 You can find all our documentation under the **docs/content/** folder.
{{< /hint >}}

### 🚩 Prerequisites

To make changes to documentation, you should have:

* [hugo](https://gohugo.io/getting-started/quick-start/#step-1-install-hugo)
executable; the static site generator.

### 📐 Setup

To set up the website locally, you need to first install the theme:

```sh
# Clone the repository with submodules...
git clone https://github.com/emacs-eask/cli --recurse-submodules

# Navgiate to `docs/theme/geekdoc` folder
cd path/to/cli/docs/theme/geekdoc/

# Build the themes
npm install && npm run build
```

Then run the `hugo` command:

```sh
# Navigate back to `docs` folder
cd path/to/cli/docs/

# Run hugo server locally
hugo server
```

And that's it! Now you can open the browser with the URL `localhost:1313`. 🎉

{{< hint info >}}
💡 You can specify **-D** option if you consider writing a draft.
{{< /hint >}}
