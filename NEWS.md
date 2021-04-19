# didehpc 0.3.0

* Complete overhaul. Key breaking changes:
  - handling of nonstandard dependency locations has changed as we now use conan
  - some details of workers have changed as rrq has significantly changed
  - "out of place" running currently not supported
  - hpctools support has been removed (believed unused)
  - linux cluster formally removed

# didehpc 0.2.11

* Refresh available job templates, and core limits, particularly adding 32Core.

# didehpc 0.2.10

* Support for R 4.0.x with RTools 4.0

# didehpc 0.2.9

* Support for R 3.6.1 and future versions reported by HPC portal. See [Issue #67](https://github.com/mrc-ide/didehpc/issues/71), [mrc-481](https://vimc.myjetbrains.com/youtrack/issue/mrc-481), [PR #72](https://github.com/mrc-ide/didehpc/pull/72), [PR #73](https://github.com/mrc-ide/didehpc/pull/73)

# didehpc 0.2.7

* Support for R 3.5.3. See [Issue #mrc-269](https://vimc.myjetbrains.com/youtrack/issue/mrc-269) and [PR #70](https://github.com/mrc-ide/didehpc/pull/70)) by `@weshinsley`

# didehpc 0.2.6

* Support for R 3.6.0. See [Issue #67](https://github.com/mrc-ide/didehpc/issues/67) and [PR #68](https://github.com/mrc-ide/didehpc/pull/68)) by `@weshinsley`

# didehpc 0.2.5

* Unmap network drives at end of job- see (See [Issue #65](https://github.com/mrc-ide/didehpc/issues/65) and [PR #66](https://github.com/mrc-ide/didehpc/pull/66)) by `@weshinsley`

# didehpc 0.2.4

* Fixes for different slashes for FQDN fix([#62](https://github.com/mrc-ide/didehpc/pull/62)) by `@weshinsley`

# didehpc 0.2.3

* Workaround a server share issue by using fully-qualified domain address (.dide.ic.ac.uk) for the home and temp shares. ([#61](https://github.com/mrc-ide/didehpc/pull/61)) by `@weshinsley`

# didehpc 0.2.2

* Support adding a Java Runtime to the path. ([#58](https://github.com/mrc-ide/didehpc/pull/59)) by `@weshinsley`

# didehpc 0.2.1

* Fixed rrq support, which had broken with changes I made to rrq

# didehpc 0.2.0

* Support for R 3.4.x and 3.5.x ([#54](https://github.com/mrc-ide/didehpc/issues/54))
* Better detection of windows mounts ([#52](https://github.com/mrc-ide/didehpc/pull/52)) by `@weshinsley`
* Documentation improvements ([#47](https://github.com/mrc-ide/didehpc/pull/47)) by `@weshinsley`

# didehpc 0.1.6

* Fix mount-point detection failure with wmic occurring on some Windows machines.

# didehpc 0.1.5

* Check what resources a user has on login ([#50](https://github.com/mrc-ide/didehpc/issues/50))

# didehpc 0.1.4

* Support for machines set up to use `qdrive` ([#48](https://github.com/mrc-ide/didehpc/issues/48))
