<div align="center">
  <picture>
    <img width="300" height="120" alt="apm logo banner" src="https://github.com/abapPM/abapPM/blob/main/img/apm_banner.png?raw=true&ver=1.0.0">
  </picture>
  <p>&nbsp;</p>
</div>

![Version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/abapPM/abapPM/src/core/zif_abappm_version.intf.abap/c_version&label=Version&color=blue)
[![Download](https://img.shields.io/badge/Download-Click_Here-blue)](https://github.com/abapPM/abapPM/build/zabappm_standalone.abap)

[![License](https://img.shields.io/github/license/abapPM/abapPM?label=License&color=success)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/abapPM)](https://api.reuse.software/info/github.com/abapPM/abapPM)

# apm

apm is a *package manager* 📦 for ABAP applications and modules, a *website* 🌐, and a *registry* 📑.

This repository contains the source code of the *package manager* i.e. the developer version of apm. 

You can find the *website* at https://abappm.com and the *registry* at https://registry.abappm.com.

NO WARRANTIES, [MIT License](LICENSE)

## Prerequisites

SAP Basis 7.50 or higher

(A downport to lower releases is on the roadmap)

## Installation

1. Download the standalone version of apm from [zabappm_standalone](https://github.com/abapPM/abapPM/build/zabappm_standalone.abap)
1. Create SAP package `$ZAPM` in your system
1. Create the program `ZABAPPM_STANDALONE` in package `$ZAPM`, upload the code, and activate
1. Create transaction `ZAPM` in package `$ZAPM` for program `ZABAPPM_STANDALONE`

## Usage

Start apm using transaction `ZAPM`.

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/abapPM/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

Install the developer version of *apm* using [abapGit](https://github.com/abapGit/abapGit) by creating a new online repository for https://github.com/abapPM/abapPM.

Recommended SAP Package: `$ABAPPM`

> [!NOTE]
> Unlike other projects, a build process is required to create the standalone version.
> Currently, this process is not automated or documented!

## About

Made with ❤️ in Canada

Copyright 2024 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Bluesky and [@marcfbe](https://linkedin.com/in/marcfbe) on LinkedIn
