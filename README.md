# Most used TeX macros

This is an analysis of the most popular macros in (La)TeX files.

It's intended to help LaTeX editor developers in choosing the macros for which to provide shortcuts. This results in several choices which would be odd in a full LaTeX parser:

* Macros that consist of a backslash and single symbol (e.g. `\[`) are skipped as they're easy to type anyway.
* `\left(`, `\right\}`, etc. are considered to be a single macro.
* `\begin{name}` is special-cased but `\end{name}` is not so that we can see both the popularity of particular environments as well as environment blocks in general.

## How to use

### Dependencies

The Python program requires the `requests` and `github` modules. On Debian-based systems, you can install them with:

```sh
sudo apt install python3-requests python3-github
```

In addition, the Haskell program requires the `stack` binary, available at [haskellstack.org](https://haskellstack.org/).

### Running

First, download the top N most starred TeX GitHub repositories to the `repos` directory:

```sh
./download.py --n-repos=500
```

Then, analyze the code:

```sh
./analyze.hs > results.txt
```
