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

## 🚩 Prerequisites

To make changes to documentation, you should have:

* [hugo](https://gohugo.io/getting-started/quick-start/#step-1-install-hugo)
executable; the static site generator.

## 📐 Setup

To set up the website locally, you need to first install the theme:

```sh
# Clone the repository with submodules...
git clone https://github.com/emacs-eask/cli --recurse-submodules

# Navgiate to `docs/theme/geekdoc` folder
cd ./docs/theme/geekdoc/

# Build the themes
npm install && npm run build
```

Then run the `hugo` command:

```sh
# Navigate back to `docs` folder
cd ./docs/

# Run hugo server locally
hugo server
```

You should see something similar to the following screen:

```console
Start building sites …
hugo v0.148.1-98ba786f2f5dca0866f47ab79f394370bcb77d2f windows/amd64 BuildDate=2025-07-11T12:56:21Z VendorInfo=gohugoio


                  │ EN  │ ZH - TW
──────────────────┼─────┼─────────
 Pages            │  36 │      34
 Paginator pages  │   0 │       0
 Non-page files   │   2 │       0
 Static files     │ 144 │     144
 Processed images │   0 │       0
 Aliases          │   2 │       1
 Cleaned          │   0 │       0

Built in 3987 ms
Environment: "development"
Serving pages from disk
Running in Fast Render Mode. For full rebuilds on change: hugo server --disableFastRender
Web Server is available at http://localhost:1313/ (bind address 127.0.0.1)
Press Ctrl+C to stop
```

And that's it! Now you can open the browser with the URL `localhost:1313`. 🎉

{{< hint info >}}
💡 You can specify **-D** option if you consider writing a draft.
{{< /hint >}}
