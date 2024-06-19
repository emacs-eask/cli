# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 0.11.x (Unreleased)
> Released N/A

* N/A

## 0.10.x
> Released Jun 13, 2024

* fix: Unsilent in Eask-file by default (40d82becaf20f0851e5fc37aa17c5f6871c163a3)
* Make better use for `special` commands (#206)
* feat: Accept rest arguments after command line separator (#207)
* feat(_prepare.el): Add program temp buffer (6262821596edb48d4f3b9bc61405129e084350b1)
* fix(indent.el): Further improve indent-lint (c0bf56ccc0c47846cd314067fde6ee4eeedee5aa)
* fix: Compatible to Emacs 26.x (#209)
* feat(repl): Add the Elisp REPL command (#210)
* fix(generate/workflow): Require emacs version (f643c5da8992bf8b93287578dd8f3b553398ad85)
* chore: Alias `pack` to `package` command (e209d7c8152a17de81613f09b380a2f5ad05697a)
* fix(build.yml): Release with `.tar.gz` instead of `.zip` on Unix-like system (#214 and #216)
* feat: Add `loc` command (#227)
* fix: Allow initialize in other scopes (#229)
* feat: Rename command `check-eask` to `analyze` (#230)
* feat(fmt): Add formatters (#232)
* feat(test): Add built-in `ecukes` test command (#236)
* fix: Don't pollute outer programs (#239)
* feat(generate/test): Add commands to setup test files (#243)
* feat: Add `docs` command (#245)
* feat(cmds): Add `recompile` command (#254)
* feat(eask): Use `strict` option (#256)

## 0.9.x
> Released Nov 17, 2023

* Add command to generate LICENSE file (#155)
* Add new DSLs `author` and `license` (#156)
* Add license linter (#157)
* Allow few commands' execution without any Eask-file (#159)
* Add command `cat` (#160)
* Improve messages while processing package transaction (#164)
* Improve messages while task cleaning all (#165)
* Add `clean` option for clean compile environment (#166)
* Split up `config` and `global` space into two different environments (#167)
* Add use of `--show-hidden` option (#168)
* Add command to generate `ignore` file (#169)
* Use standard output (#170)
* Fix non-displayable character, use ascii instead (#172)
* Use environment PATH to specify Emacs version to use (#173)
* Fix showing positional arguments in subcommand's help menu (#174)
* Add aliases for `pkg-file` command (0d35d762a12bd399657c2fdcb60541dcc0c8b5e0)
* Add option to init from `Keg`-file (#182)
* Add option to init from elisp source file (#183)
* Print archives progress (#184)
* Respect package file path in `autoload` command (44c042445bba0dd071d9112e58549437b7ebd58d)
* fix(vcpkg): Use workaround for `MODULE_NOT_FOUND` error (#187)
* Add docker command (#188)
* Enter docker directly when no arguments (#191)
* Add command to generate recipe (#192)
* Add option to init from `Eldev`-file (#193)
* Add command `commnad` for custom commands (#195)
* Merge the two commands `run` and `command` (#196)
* Add `melpazoid` command (#202)
* Add `source` command and its subcommands (#203)
* Add `bump` command (#204)
* fix: Print with stdout (#205)

## 0.8.x
> Released Mar 08, 2023

* Handle Easkfile existence for command `eask init` (2f5edb3d9437d4ba55d2f014ca1ba1b65075eed8)
* Generate log files in user directory (974a17e3b573520a775f813c72ecbd30a32012bf)
* Make sure there is no eval error in the root layer (fbd8eb4b91dfbb614b2b59a1a2a7fea9f594313d)
* Recognize `bsdtar` for windows (#64)
* Add `-q`/`--quick` option for quickstart (#67)
* Pick Eask-file by Emacs version, allow multiple Eask-files (#68)
* Add command and directive for `run-script` (#69)
* Remove alias for `build` subcommand (#70)
* Merge `clean` commands to one subcommand (#71)
* Fix void function `eask-source` to `eask-f-source` (#75)
* Fix upcoming breaking changes from `package-build` (#65)
* Add support for `elisp-lint` (#79)
* Adapt `-a`/`--all` option for archives command (#83)
* Merge command `list-all` to list with `-a`/`--all` option (#84)
* Support JSON format with Eask-file linter (#85)
* Handle multiple Eask-files for `init` command (#86)
* Remove dependency `s.el` (962dd5f8d0da1443368ac2d79b0a013c153a804e)
* Acknowledge cleaning state for `clean all` command (#87)
* Fix keyword lint undetected (#88)
* Improve Eask-file loader to search file upward (#89)
* Move command `activate` under test subcommand (#92)
* Add command to create ELPA project (#94)
* Add command to clean log files (#97)
* Add command to clean autoloads file (#98)
* Add command to clean pkg-file (#99)
* Add option `-o`/`--output` to output log for Eask checker (#100)
* Fix line-endings for unix system (#106)
* Add `link` command (#107)
* Fix package `xxx` is unavailable issue from Emacs snapshots (#111)
* Fix install `CRLF` EOL (#112)
* fix: Also expose `bin` folder for symlink package (#115)
* Resolve infinite recursion in `exec-path` setup (#118)
* feat: Add capability to search through path (#119)
* Support DSL `package-descriptor` (#124)
* Resolve the potential symlink to the bash script (#24, #125, and #126)
* Workaround for arguments that contain whitespaces (#128 and #129)
* Silent unnecessary Node's stacktrace (#134 and #136)
* Allow linter list through all errors (#134 and #137)
* Omit `nil` value for conditions in `development` scope (#143 and #144)
* Move `pkg-file` and `autoloads` commands under `generate` subcommand (#142)
* Add option to convert Cask to Eask (#141 and #145)
* Add command to generate GHA workflow (#141 and #146)
* Add commands to support all kind of CI platforms (#150)

## 0.7.x
> Released Sep 08, 2022

* Avoid loading package info unless it's needed (#13, #14, and #19)
* Read `-pkg.el` file prior to package-file while exists (#21)
* Simplify (rewrite) command exec (#22)
* Move `exec` command to node layer (#27)
* Move `test` and `lint` commands to it's subcommand (#31)
* Avoid printing asni sequence in DUMB terminal (#34)
* Add implementation for `package-build--get-timestamp` (#36)
* Fix `time-convert` missing in Emacs 26.x; improve CI test on `package` command (#38)
* Fix `eask create`, honour `version` and `emacs_version` (#41)
* Add new command `eask emacs` (#46)
* Remove two options contradict to default settings, `--no-timestamps` and `--no-log-level` (#48)
* Add new option `--elapsed-time`, `--et` (#48)
* Add two new DSL; `website-url` and `keywords` (#49)
* Replace `time-convert` to compatible with older Emacs version (#50)
* Fix test buttercup with adding the current path to `load-path` (#53)
* Return correct packaged file depends on multi-files flag (23cd251abd4e65c62549feb280e785d1ee9634a7)
* Add keywords command and it's linter (#54)
* Handle many-to-one condition for version string (0ac654face0557ff74c4a6048fc8bef59c915ec8)
* Add new option `--log-file`, `--lf` to generate log files (#57)
* Rename package to `@emacs-eask/cli` (#60)

## 0.6.17
> Released May 05, 2022

* Reset `command-line-args-left` variable by default (fbda232e962e3566efb94d8061a4035709b7c6e2)
* Clean up `.git` directory after creating a new elisp project (d845fd281707f0ed47fa955139e1ebddd56b8f58)
* Add new commnad `eask activate` (dab4321803315eac530afa04161bd315687c271a)
* Make `default-directory` respect to `-g` flag (ffecfda6c982f145ccbd176fd02cc8bfb44352d7)
* Add new command `eask recipe` (3073fe89140a93fa91b8e81c2535240fb9e3ada1)
* Add new command `eask elsa` (69b5f1519fb5a2493fb26f96ee3ef022526aa6a1)
* Add `pkg` configuration, we can now pack to executables (79d2c454a14babc85fe3d8ae861be26e635ee12b)
* Don't inhibit message for `print`, `princ`, or `prin1`; this is dangerous (1297f786fcebcae3ba743792fc6ff9706e8dfa6d)
* Fix path for `pkg` build (b97bfa8cc46cedd967243a55d8fae83f106b081c)

## 0.6.x
> Released Apr 29, 2022

* Clean up `ansi-apply`, now accept object for the first argument (67277ffb70ff8966d452da76fd50ad3cfb41ef08)
* Ensure Eask-file exists before executing the command (d40e29a042c1886d5748480d789a8d7b36735a09)
* Add checker rules for package metadata (f69efac5e1f66ee73424e17d733cd732d3a28eda)
* Add checker rules for unmatch dependencies (ab42f7f39dd0db6cbbbdfbfec5e7fae397936b99)
* Print more information for command `eask info` (6d74139fe16c5bdddd0942b66bad21c3b34c6f5a)
* Print archive name while listing packages (b1cacc71c48821142d41e5a740e7f09ba5b37bb1)
* Set main file for package lint (cfd47289bea936163c133bcd4b68d9e6b758a356)
* Print total files with command `eask info` (fe8dbe28fb7692aa6abdaa1d871cc986ebef8f14)
* Print unpacked size with command `eask info` (82cd36c34a4b978fe76b7f9b2099ea7929d31a60)
* Add new command `eask buttercup` (4ef7b3a054971aafc57d0060f62cf70b4126c97c)
* Add new command `eask indent` (99ec942878452ae1c27f027f4a577eefb70a0d92)
* Add new command `eask create` (94b1075f919d53c0741c61ebea42a0fbed3d1ccf)
* Add new command `eask declare` (04b3b51a6a07d3a84611b6082ca46f2aafece7bd)
* Add new command `eask regexps` (9121e1c37b2640a21cdd7abd1d0a2f51ac1768dd)

## 0.5.x
> Released Apr 18, 2022

* Unified dependencies list with data structure of list, and no longer have string (9f09f93ec866e32d14dd2f6c53ee8fde6cd4b986)
* Add new command `eask refresh` (5423d84a59c88d52c2279f27a189573eb525a82b)
* Move help manual to it's folder (4036319d3d40ea37270103adbfb102dd0d6b24d5)
* Move command files by category (2b405c5cc65eb9a3099b2bc4b6b3b41395d7ba0f)
* Add new command `eask search` (840646c1b91fcbed689d33cb6890bf482b9b1913)
* Restrict `package` definition in `Eask`-file (ba6ed6853c98d19d88971af745ad9a8b2d794ae5)
* Add new command `eask ert-runner` (8647d37c80c5349d8b3b3b17b8570a91bb91339c)
* Add new command `eask ert` (29c5722fa5b8fea8add30d6de0169166cfd7c17f)
* Handle error `Failed to download ‘xxx’ archive` (29887c80c33b9f909151b465cb99160160cc96c3)
* Handle Emacs version while installing packages (8cafd4f2d34829da5832241cf105753658019abf)
* Make error/warning log check compatible to older Emacs version (397d3740f1eee5f8e078116d323eeca9479f5114)
* DSL `files` support multiple definition (3948a1d2366b82eb0a7e11bf1131867881c83ad7)
* Colorized ERT tests result (1b55b51eb4fbe5e602bd7aed8d8e05e372a0cd95)
* Simulate batch-mode (d67098c10a683cd05893d8fda46105229f287394)
* Add new command `eask check-eask` (621fb8eefe41f513c9fe090daaec24c1ee9083e6)
* Use built-in `pkg-file` generate function for command `eask pkg-file` (4aa327bf43b84dfed128f8898b1c0885afc1edf9)

## 0.4.x
> Released Apr 05, 2022

* Add Eask file keyword `exec-paths` (5334bc5e16e8d52a9e6a7f23781f2b42d333d11c)
* Remove Eask file keyword `load-path` (a72fdacece54aa7acebfea3265bf9a0eace41778)
* Add new command `eask checkdoc` (c10ccb908f8af5bc45094efff17364eecf0b6fbc)
* Fix install dependencies when calling package init (e9be0bc42fea6620572c70aeaf0fc684e7a5ce5c)
* Add color to the output (afa74da680150fb8f9d50187adf1922b21f83fc9)
* Add new option `--allow-error` (c9c4cf24a6d42633f8c725385c7e4910774075ff)
* Add new command `eask install-deps` (955a36231fa968e1fdedd29ae9c818385d21f93e)
* Add new option `--insecure` (9c41e5ce65b9e92f1e807ed43ba735bfa7cac7e2)
* Add new command `eask upgrade-eask` (e1f21fec84a5461c0c027a4560bfdb84428e6f99)
* Add new command `eask locate` (cef319d0820932cf619bd8836a7b0f7b36742746)
* Colorized compile log for `eask compile` command (cfc31054d1ce4e077b4b501bc38adc41660c4cab)
* No longer require package after installation! (9175c8b5cbdf97b6410cd35b438db406f4f841be)
* Hide script name in commands list (7ec2967152e11f1bbbd9a48d94835a7a8cd5aa1f)
* Integrate SPEC for `depends-on` DSL (4250ea837e41d1357b2ce98e4902c38e73adb59e)
* Move help manual to individual files (4036319d3d40ea37270103adbfb102dd0d6b24d5)

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
