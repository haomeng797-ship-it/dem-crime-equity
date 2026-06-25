#!/usr/bin/env python3
"""Render dem_crime_equity_paper.qmd to the 'pretty APA' PDF without Quarto:
Times New Roman, 1.5 line spacing, a centered title page with affiliation, an
ABSTRACT block, and a running head, via pandoc + xelatex.

Why this exists: Quarto's default PDF title block does not produce the title-page
+ affiliation + abstract-rule layout we want. This script reuses the qmd's text
and the figures_pdf/ images and lays them out with a small raw-LaTeX preamble.

Usage:  python3 paper/build_pretty_pdf.py
Output: paper/dem_crime_equity_paper.pdf
Requires: pandoc (>=2.9) and a xelatex (TinyTeX or TeX Live). Both are looked up
on PATH first, then at the common Quarto/TinyTeX locations.
"""
import os, re, shutil, subprocess, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PAPER = os.path.join(ROOT, "paper")
QMD = os.path.join(PAPER, "dem_crime_equity_paper.qmd")
MD = os.path.join(ROOT, "_pretty_render.md")
HEADER = os.path.join(ROOT, "_pretty_header.tex")
PDF = os.path.join(PAPER, "dem_crime_equity_paper.pdf")
DATE = "June 22, 2026"


def find_tool(name, candidates):
    p = shutil.which(name)
    if p:
        return p
    for c in candidates:
        c = os.path.expanduser(c)
        if os.path.exists(c):
            return c
    sys.exit(f"could not find {name} on PATH or known locations")


PANDOC = find_tool("pandoc", ["~/quarto/bin/tools/aarch64/pandoc",
                              "~/quarto/bin/tools/x86_64/pandoc",
                              "~/quarto/bin/tools/pandoc"])
XELATEX = find_tool("xelatex", ["~/Library/TinyTeX/bin/universal-darwin/xelatex",
                                "/Library/TeX/texbin/xelatex"])

qmd = open(QMD).read()
m = re.match(r"^---\n(.*?)\n---\n(.*)$", qmd, re.S)
front, body = m.group(1), m.group(2)


def yaml_scalar(key):
    mm = re.search(rf'^{key}:\s*"(.*?)"\s*$', front, re.M)
    return mm.group(1) if mm else ""


title = yaml_scalar("title")
subtitle = yaml_scalar("subtitle")
am = re.search(r'^abstract:\s*\|\s*\n(.*?)\n(?=\S)', front, re.S | re.M)
abstract = re.sub(r'\s+', ' ', am.group(1)).strip() if am else ""

# keep the PDF figure branch, drop the HTML branch
body = re.sub(r':::\s*\{\.content-visible when-format="html"\}.*?\n:::[ \t]*\n', '', body, flags=re.S)
body = re.sub(r':::\s*\{\.content-visible when-format="pdf"\}[ \t]*\n(.*?)\n:::[ \t]*\n', r'\1\n\n', body, flags=re.S)
# the md sits at repo root, so figures_pdf/ resolves without the ../
body = body.replace("../figures_pdf/", "figures_pdf/")


def esc(s):
    for a, b in [('\\', r'\textbackslash{}'), ('&', r'\&'), ('%', r'\%'), ('#', r'\#'),
                 ('_', r'\_'), ('$', r'\$'), ('~', r'\textasciitilde{}'), ('^', r'\textasciicircum{}')]:
        s = s.replace(a, b)
    return s


title_block = r"""```{=latex}
\thispagestyle{plain}
\begin{center}
{\LARGE\bfseries @@TITLE@@\par}
\vskip 0.8em
{\itshape\large @@SUBTITLE@@\par}
\vskip 0.7em
\rule{1.1in}{0.4pt}\par
\vskip 0.7em
{\large Miura Meng\par}
\vskip 0.6em
{\small M.S.Ed. in Statistics, Measurement, Assessment, and Research Technology (SMART)\par}
{\small University of Pennsylvania, Graduate School of Education\par}
\vskip 0.6em
{\small @@DATE@@\par}
\end{center}
\vskip 0.7em
\noindent\rule{\textwidth}{0.4pt}
\vskip 0.5em
\begin{center}{\bfseries\footnotesize A\,B\,S\,T\,R\,A\,C\,T}\end{center}
\vskip 0.3em
\begingroup\setlength{\parindent}{0.5in}
@@ABSTRACT@@
\par\endgroup
```
"""
# placeholders are collision-free (unlike replacing TITLE before SUBTITLE)
for k, v in [("@@TITLE@@", esc(title)), ("@@SUBTITLE@@", esc(subtitle)),
             ("@@DATE@@", DATE), ("@@ABSTRACT@@", esc(abstract))]:
    title_block = title_block.replace(k, v)

# hanging indent for the reference list (APA), overriding the global 0.5in indent
body = body.replace(
    "# References {.unnumbered}",
    "# References {.unnumbered}\n\n```{=latex}\n\\setlength{\\leftskip}{0.5in}\\setlength{\\parindent}{-0.5in}\n```\n",
)

open(MD, "w").write(title_block + "\n" + body)

header = r"""\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\small THE MEASURE MAKES THE FINDING}
\fancyhead[R]{\small\thepage}
\renewcommand{\headrulewidth}{0.4pt}
\fancypagestyle{plain}{\fancyhf{}\cfoot{\small\thepage}\renewcommand{\headrulewidth}{0.4pt}}
\usepackage{titlesec}
\titleformat{\section}{\large\bfseries}{}{0em}{}
\titleformat{\subsection}{\normalsize\bfseries}{}{0em}{}
\titlespacing*{\section}{0pt}{14pt}{4pt}
\titlespacing*{\subsection}{0pt}{12pt}{2pt}
\usepackage{indentfirst}
\usepackage{float}
\floatplacement{figure}{H}
\AtBeginDocument{\setlength{\parindent}{0.5in}\setlength{\parskip}{0pt}}
"""
open(HEADER, "w").write(header)

cmd = [PANDOC, MD, "-o", PDF, "--pdf-engine=" + XELATEX, "-H", HEADER,
       "-V", "mainfont=Times New Roman", "-V", "fontsize=11pt",
       "-V", "geometry:margin=1in", "-V", "linestretch=1.5"]
print("pandoc:", PANDOC)
print("xelatex:", XELATEX)
subprocess.run(cmd, cwd=ROOT, check=True)
print("wrote", PDF)
