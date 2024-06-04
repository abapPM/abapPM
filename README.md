<div align="center">
  <img src="https://github.com/abapPM/abapPM/blob/main/img/apm_logo.svg?raw=true&ver=1.0.0"
       alt="apm - A Package Manager for ABAP" 
       height="200"
       width="200" />
</div>

![Version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/abapPM/abapPM/src/zif_abappm_const.intf.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/abapPM?label=License&color=green)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=green)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/abapPM)](https://api.reuse.software/info/github.com/abapPM/abapPM)

# apm

apm is a *package manager* 📦 for ABAP applications and modules, a *website* 🌐, and a *registry* 📑.

This repository contains the source code of the *package manager* i.e. the developer version of apm. 

You can find the *website* at https://abappm.com and the *registry* at https://registry.abappm.com.

NO WARRANTIES, [MIT License](LICENSE)

## Prerequisites

SAP Basis 7.40 SP 8 or higher 

(A downport to 7.02 is on the roadmap)

## Installation

1. Download the standalone version of apm from [zapm_standalone](/build/zapm_standalone.abap)
1. Create a new SAP package in your system. Recommended: `$APM`.
1. Create the program `ZAPM_STANDALONE` in the package, upload the code, and activate
1. Create transaction `ZAPM` for the program `ZAPM_STANDALONE`

## Usage

Start apm using transaction `ZAPM`.

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](CONTRIBUTING.md), fork this repo, and create a pull request.

Install the developer version of *apm* using [abapGit](https://github.com/abapGit/abapGit) either by creating a new online repository for https://github.com/abapPM/abapPM.

*Note:*

Different from other projects, a build process is required to create the standalone version which is not documented yet! 

## About

Made with :heart: in Canada

Copyright 2024 apm.to Inc. <https://apm.to>

Follow [@marcfbe](https://twitter.com/marcfbe) on X/Twitter
