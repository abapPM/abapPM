<div align="center">
  <picture>
    <img width="300" height="120" alt="apm logo banner" src="https://github.com/abapPM/abapPM/blob/main/img/apm_banner.png?raw=true&ver=1.0.0">
  </picture>
  <p>&nbsp;</p>
</div>

![Version](https://img.shields.io/endpoint?url=https://shield.abappm.com/github/abapPM/abapPM/src/main/%2523apmg%2523if_apm_version.intf.abap/c_version&label=Version&color=blue)
[![Download](https://img.shields.io/badge/Download-Click_Here-blue)](https://raw.githubusercontent.com/abapPM/abapPM-Standalone/refs/heads/main/src/zabappm_standalone.prog.abap)

[![License](https://img.shields.io/github/license/abapPM/abapPM?label=License&color=success)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/abapPM)](https://api.reuse.software/info/github.com/abapPM/abapPM)

# apm

apm is a *package manager* 📦 for ABAP applications and modules, a *website* 🌐, and a *registry* 📑.

This repository contains the **source code** of the *package manager*, i.e., the developer version of apm. 

You can find the *website* at https://abappm.com and the *registry* at https://registry.abappm.com.

NO WARRANTIES, [MIT License](LICENSE)

## Prerequisites

SAP Basis 7.50 or higher

(A downport to lower releases is on the roadmap)

## Limitations

- Only "prefix" folder logic
- Only local packages (no transports)
- No parallel processing (for installing or publishing packages)
- Always all languages (no "main language only")
- Always complete package hierarchy (no "ignore sub-packages")

These limitations are planned to be removed in future releases.

## Installation

1. Download the standalone version of apm from [zabappm_standalone.prog.abap](https://raw.githubusercontent.com/abapPM/abapPM-Standalone/refs/heads/main/src/zabappm_standalone.prog.abap)
1. Create SAP package `$ABAPPM` in your system
1. Create the program `ZABAPPM_STANDALONE` in package `$ABAPPM`, upload the code, and activate
1. Create transaction `ZAPM` in package `$ABAPPM` for the program `ZABAPPM_STANDALONE`
 
## Usage

Start apm using transaction `ZAPM`. The start screen shows a list of installed packages, which on your first run will be empty. On the top right, it shows the current registry which by default is `playground.abappm.com`. You can install from or publish any package to this playground registry (see [Playground Rules (t.b.d.)](https://docs.abappm.com/user-guide/playground.html)).

To use the productive apm Registry, sign up at https://abappm.com/sign-up. Switch the registry in your apm settings from `playground.abappm.com` to `registry.abappm.com`, and you will be able to plublish your own packages or download and install any other package you find in the registry. 

Now build awesome things with ABAP packages and share them with your fellow ABAPers!

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/abapPM/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

Install the developer version of *apm* using [abapGit](https://github.com/abapGit/abapGit) by creating a new online repository for `https://github.com/abapPM/abapPM` abd SAP Package `/APMG/APM`.

> [!IMPORTANT]
> Do not change dependencies found in `/src/modules`. To change a dependency, find the corresponding module on [apm GitHub](https://github.com/abapPM) or the [apm Website](https://abappm.com) and contribute to the corresponding repository.

> [!NOTE]
> Unlike other projects, a build process is required to create the standalone version. This process is not automated or documented, yet!

## Attribution

This project includes the code for the following open-source projects. Please support them if you can!

- [abapGit](https://github.com/abapGit/abapGit), abapGit Community, [MIT](https://github.com/abapGit/abapGit/blob/main/LICENSE)
- [AJSON](https://github.com/sbcgua/ajson), Alexander Tsybulsky, [MIT](https://github.com/sbcgua/ajson/blob/main/LICENSE)
- [ABAP String Map](https://github.com/sbcgua/abap-string-map), Alexander Tsybulsky, [MIT](https://github.com/sbcgua/abap-string-map/blob/main/LICENSE)
- [ABAP Logger](https://github.com/ABAP-Logger/ABAP-Logger), Eric Peterson, [MIT](https://github.com/ABAP-Logger/ABAP-Logger/blob/main/LICENSE)
- [node semver](https://github.com/npm/node-semver), Isaac Z. Schlueter and Contributors, [ISC](https://github.com/npm/node-semver/blob/main/LICENSE)

## About

Made with ❤ in Canada

Copyright 2025 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Bluesky and [@marcfbe](https://linkedin.com/in/marcfbe) or LinkedIn
