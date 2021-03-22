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
./download.py --n-repos=500 --token=<your GitHub token>
```

(`--token` is optional but recommended because authenticated users have higher rate limits.)

Then, analyze the code:

```sh
./analyze.hs > results.txt
```

### Results

As of 2021-03-15, the top 200 results are:

```
\end
\item
\mathcal
\to
\ref
\label
\texttt
\in
\frac
\mathbf
\node
\def
\draw
\tcode
\emph
\text
\times
\subsection
\textbf
\pdfglyphtounicode
\alpha
\usepackage
\section
\meta
\index
\path
\mathfrak
\begin{itemize}
\ldots
\ar
\includegraphics
\fi
\noindent
\code
\sum
\otimes
\newcommand
\expandafter
\begin{proof}
\pgfpoint
\textwidth
\pi
\subsubsection
\lambda
\subset
\caption
\tt
\verb
\bullet
\cite
\NormalTok
\pgf@y
\centering
\pgf@x
\varphi
\theta
\begin{enumerate}
\hline
\mu
\partial
\begin{codeexample}
\circ
\ctikzvalof
\sigma
\textit
\l
\leq
\gls
\begin{lstlisting}
\cs
\begin{frame}
\begin{lemma}
\cdot
\phi
\else
\int
\color
\begin
\begin{figure}
\beta
\let
\pnum
\right)
\left(
\lstinline
\endcsname
\csname
\quad
\begin{tikzpicture}
\delta
\infty
\sh
\Gamma
\PYG
\gamma
\Spec
\geq
\medskip
\vspace
\relax
\ctikzset
\anchor
\hat
\d
\psi
\marg
\small
\begin{center}
\overline
\paragraph
\exp
\pgf@circ@res@step
\bf
\x
\li
\omega
\Omega
\input
\cap
\log
\begin{verbatim}
\pgfpathlineto
\kappa
\Delta
\sqrt
\it
\pgf@circ@res@up
\cdots
\multicolumn
\rho
\begin{equation}
\footnote
\frametitle
\tau
\xi
\begin{tabular}
\documentclass
\epsilon
\advance
\begin{document}
\definecolor
\textcolor
\pgf@circ@res@left
\begin{key}
\wedge
\par
\em
\the
\dots
\tikz
\usetikzlibrary
\url
\pgfmathresult
\pgfpathmoveto
\rule
\bar
\tilde
\textsubscript
\ell
\cref
\mathrm
\pgf@circ@res@right
\LaTeX
\href
\TeX
\footnotesize
\edef
\ifx
\mat
\State
\vec
\nabla
\protect
\longrightarrow
\varepsilon
\Lambda
\R
\oplus
\mid
\mapsto
\pgfusepath
\hspace
\linewidth
\nu
\foreach
\setlength
\cup
\eta
\begin{minipage}
\dim
\mathbb
\renewcommand
\eqref
\co
\subseteq
\cmd
\hypertarget
\xymatrix
\Hom
\scriptsize
```
