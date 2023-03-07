---
title: 💠 CircleCI
weight: 400
---

{{< toc >}}

Example to use [Circle CI](https://circleci.com/).

```yml
# Default actions to perform on each Emacs version
default: &default-steps
  steps:
    - checkout
    - run: apt-get update && apt-get install -y git
    - run: |
        eask package
        eask install
        eask compile

# Enumerated list of Emacs versions
jobs:
  test-emacs-26:
    docker:
      - image: silex/emacs:26.2
        entrypoint: bash
    <<: *default-steps

  test-emacs-master:
    docker:
      - image: silex/emacs:master
        entrypoint: bash
    <<: *default-steps

# Executing in parallel
workflows:
  version: 2
  ci-test-matrix:
    jobs:
      - test-emacs-26
      - test-emacs-master
```

This example is testing your Emacs Lisp package in the below environment;

* Emacs: `26.2` and `snapshot`
* Eask: `snapshot` (latest)
