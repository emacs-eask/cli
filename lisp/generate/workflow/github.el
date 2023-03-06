;;; generate/workflow/github.el --- Generate GitHub Actions workflow `test.yml` file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate GitHub Actions test yaml file,
;;
;;   $ eask generate workflow github
;;

;;; Code:

(load (expand-file-name
       "../../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let* ((dir ".github/workflows/")
         (basename (or (car (eask-args)) "test.yml"))
         (filename (expand-file-name (concat dir basename))))
    (ignore-errors (make-directory  t))
    (if (file-exists-p filename)
        (eask-info "The yaml file already exists `%s`" filename)
      ;; TODO: Move it to the internet, and download it from there!
      (with-current-buffer (find-file filename)
        (insert "name: CI

on:
  push:
    branches:
      - master
  pull_request:
  workflow_dispatch:

concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version:
          - 26.3
          - 27.2
          - 28.2
          - snapshot

    steps:
    - uses: actions/checkout@v3

    - uses: jcs090218/setup-emacs@master
      with:
        version: ${{ matrix.emacs-version }}

    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask clean all
        eask package
        eask install
        eask compile")
        (save-buffer))
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/github.el ends here
