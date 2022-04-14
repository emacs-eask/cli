---
title: Development API
permalink: api
---

# Development API

This document provides a reference to the public Eask API, which you may use in
your projects and extensions to Eask.

## Core

```elisp
(eask-start
  (message "Hello World!~"))
```

## Logging

#### 🔎 Variable: eask-verbosity

Verbosity level represent as an integer.

```elisp
(setq eask-verbosity 4)  ; you could set from 0 to 4
```

#### 🔎 Variable: eask-timestamps

Log messages with timestamps.

```elisp
(setq eask-timestamps t)
```

```
2022-04-14 13:44:46 This is message with timestamps
```

#### 🔎 Variable: eask-log-level

Log messages with level.

```elisp
(setq eask-log-level t)
```

```
[DEBUG] This is DEBUG message with log level
```

#### 🔎 Macro: eask-with-verbosity (`symbol` `&rest body`)

Define executions with the verbosity level.

```elisp
(eask-with-verbosity 'debug
  (message "Hello World!~"))
```
