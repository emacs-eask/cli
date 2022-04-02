---
title: About CI/CD
permalink: about-cicd
---

# About CI/CD

```yml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [27.2, snapshot]

    steps:
    - uses: actions/checkout@v2

    # Install Emacs on Linux/macOS
    - uses: purcell/setup-emacs@master
      if: matrix.os == 'ubuntu-latest' || matrix.os == 'macos-latest'
      with:
        version: ${{ matrix.emacs-version }}

    # Install Emacs on Windows
    - uses: jcs090218/setup-emacs-windows@master
      if: matrix.os == 'windows-latest'
      with:
        version: ${{ matrix.emacs-version }}

    # You need node for eask
    - uses: actions/setup-node@v2
      with:
        node-version: '14'

    # Install eask
    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask install
        eask compile
        eask lint
```

