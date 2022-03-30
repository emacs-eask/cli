# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 0.4.x (Unreleased)
> Released N/A

* N/A

## 0.3.x
> Released Mar 30, 2022

* Ensure use `throw` operation to trigger CI (0253edff816a85868daf708289872542d49af729)
* Fix extra concatenation args (458d551d2e1981407929ada1ad8b4b4b430cf526)
* Use inherit options for `spawn` process (aaf4ace0603905574ccbe1c900c057e31988bdee)
* Add command `eask package [dest]` (a304cc2342c4cd85a59d38fb61709543d3d39fca)
* Add `shmelpa` source (578ea2cc0b11c145c33833b87a01e938aca48e0e)
* Improve output message (9aa9ba5977779fe837f084ff54666c6104361e07)
* Disable backup file by default (3be0010d3fe19ea7cd1d5fd64931532c98f6267f)
* Fix line endings on Windows system (b7b25da75f794e1bd1c1f6b74a9d4cd6a83e3435)
* Fix option that accept arguments (979d87ef6953f0f4a9403db4bb244c87987ea478)
* Add verbosity level and timestamps (9de0bb4155911c5399d9d2ddb0cbff9dfc25f88f)
* Add new command `eask concat` (2c85287e4a24d667d99185c05de6f714bcc669a0)
* Add new options `--timestamps` and `--no-timestamps` (3f167d64e3115eea2580708f7eff2becce246537)
* Add new option `--strict` (6d59d98999f2bd46e66e2a6d03064ee56678a591)
* Add new options `--log-level` and `--no-log-level` (e5e03679db496ce77d1ba4adde1fc6b42d931ba9)
* Improve output messages, more!

## 0.2.x
> Released Mar 26, 2022

* Fix install command (286fa96475358bfee29645c35192646ac1762724)
* Fix load path for `_prepare.el` script (87946d5d0b792dd6727f2afa8e4bdcd8e395d67a)
* Add command `eask compile` (f2422276748cdba2dd74b33fcf741f491dbc65df)
* Add Eask file options `source-priority`
* Add Eask file options `load-path`
* Add Eask file options `load-paths`
* Add command `eask list`
* Add command `eask info`
* Add command `eask files`
* Add command `eask clean-elc`
* Add command `eask load-path`
* Show help on command fails (cf17c081bfd339a09be8b9e43723a739e4bb3f58)
* Add command `eask lint` (4157f4310d639612fe5cab1d5b2d75747dde2311)
* Add hooks for all commands, including the master one.
* Command `compile`, and `lint` accept multiple arguments, `files` (b06f1139fbcc404f4148948e3f3429d23773a6c0)
* Add command `eask init` (846f8972c597cc1427bc9990fa7e452a9a963bb9)
* Add command `eask exec` (83cc11d5b9aa4f818a11f53cb29a6a076f05739b)
* Add command `eask package-directory` (e3098c8b0816a9f1a1fa953fa46f140e60c29bb2)
* Add command `eask path` (3b37957c53d5e662258014da82e81a3550bcb085)
* Add command `eask eval` (a3d8cd5a3d28aabee2aec3950f8e74fc299b75dd)
* Add command `eask list-all` (5817606f85dbb88253b6eb30cf072e2a8cd4f5c2)
* Add command `eask autoloads` (3f7c98954aa7ffb5dce4c424d380d7ef4af66a7c)
* Add command `eask pkg-file` (e267b5a26987f1e1361864280cf8781ee58200c2)
* Add command `eask outdated` (eb03a2b1cecd20620746edc4fe6719ed7a4c0b59)
* Command `load-path` and `exec-path` can now accept `-g` option. (e73bff70b21290c2b79a7c13091701f712ce6d0b)
* Add proxy global options (8e3a9130805341bd4c1ce9ccb8e4bf0a42e16ee8)
* Accept `Eask` file from default `~/.emacs.d/` directory (044ce9327a60cad8adb4bc34141b2f8e8ecd2b45)
* Use `spawn` instead of `exec`, so new the terminal will be updated immediately (e1790a833ba2422d3d171523a2670f66f0485173)
* Done basic error handling with exit code at the end of executions (e5afb70f6bbdf0424681949dd10ebd02b9fc7a25)

## 0.1.x
> Released Mar 15, 2022

* Very early release.
