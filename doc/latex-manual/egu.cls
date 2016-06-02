%% File: egu.cls
%% =============================================
%% IMPORTANT NOTICE:
%% See the copyright conditions below.
%% =============================================
%% EGU LaTeX2e Class file
%% ---------------------------------
%% The official LaTeX2e class file for journals of the
%% EUROPEAN GEOSCIENCES UNION
%% ---------------------------------
%% Author:
%% Patrick W. Daly
%% c/o European Geosciences Union
%% Max-Planck-Str. 13
%% D-37191 Katlenburg-Lindau
%% Germany
%%
%% Major modifications by Dieter Schmitt and Rolf Sander
%%
%% Email:      egu@copernicus.org
%% Internet:   http://www.copernicus.org/EGU/EGU.html
%%
%% This consists of several modules, each with its separate copywrite
%% The modules that are specific to EGU publications are
%%        Copyright (C) 1994-2005 Copernicus Society
%%
%% The entire package is available from
%%     http://www.copernicus.org/guidelines/LaTeX_submit.htm
%%
\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\def\MyProvidesClass#1 [#2 #3 #4]
  {\ProvidesClass{#1}[#2 #3 #4]%
   \def\clsname{#1}\def\clsdate{#2}\def\clsversion{#3}\def\clsinfo{#4}}
\MyProvidesClass{egu}
          [2005/10/08 7.7 (PWD/DS/RS)]
 %  For producing camera-ready copy and manuscripts for journals of the
 %      European Geosciences Union
 %
 %  This is a LaTeX2e class file, to be invoked as
 %     \documentclass{egu}           (for journal camera-ready copy) or as
 %     \documentclass[inbook]{egu}   (for book camera-ready copy) or as
 %     \documentclass[ms]{egu}       (for manuscript)
 %  It requires the local files
 %     article.cls   size10.clo    fleqn.clo
\def\ACPD{Atmospheric Chemistry and Physics Discussions}
\def\ACP{Atmospheric Chemistry and Physics}
\def\BGD{Biogeosciences Discussions}
\def\BG{Biogeosciences}
\def\CPD{Climate of the Past Discussions}
\def\CP{Climate of the Past}
\def\HESSD{Hydrology and Earth System Sciences Discussions}
\def\HESS{Hydrology and Earth System Sciences}
\def\OSD{Ocean Science Discussions}
\def\OS{Ocean Science}
\def\ADGEO{Advances in Geosciences}
\def\AG{Annales Geophysicae}
\def\NHESS{Natural Hazards and Earth System Sciences}
\def\NPG{Nonlinear Processes in Geophysics}
\def\ARS{Advances in Radio Science}
\def\ASTRA{Astrophysics and Space Sciences Transactions}
\def\SG{Social Geography}
\newif\iftwostagejnl % true if either \acptrue or \acpdtrue
\newif\ifacp
\newif\ifacpd
\newif\ifbg
\newif\ifbgd
\newif\ifcp
\newif\ifcpd
\newif\ifhess
\newif\ifhessd
\newif\ifos
\newif\ifosd

\newif\ifonline
\newif\if@hvmath
\newif\if@aglet
\newif\if@draft
\newif\if@egsbook
\newif\if@khb
\newif\if@headers \@headerstrue
\newif\if@nohyperref
\newif\if@noline
\newif\if@nolastpage
\newif\if@noauthor
\newif\if@nosref
\newif\if@mueller
\newif\if@sil

%% draft options:
\DeclareOption{draft}{\@drafttrue\@headersfalse}
\DeclareOption{ms}{\@drafttrue\@headersfalse}
%% options for two-stage Journals:
\DeclareOption{acp}{\acptrue\twostagejnltrue\@draftfalse}
\DeclareOption{acpd}{\acpdtrue\twostagejnltrue\@draftfalse\@headerstrue}
\DeclareOption{bg}{\bgtrue\acptrue\twostagejnltrue\@draftfalse}
\DeclareOption{bgd}{\bgdtrue\acpdtrue\twostagejnltrue\@draftfalse\@headerstrue}
\DeclareOption{os}{\ostrue\acptrue\twostagejnltrue\@draftfalse}
\DeclareOption{osd}{\osdtrue\acpdtrue\twostagejnltrue\@draftfalse\@headerstrue}
\DeclareOption{hess}{\hesstrue\acptrue\twostagejnltrue\@draftfalse}
\DeclareOption{hessd}{\hessdtrue\acpdtrue\twostagejnltrue\@draftfalse\@headerstrue}
\DeclareOption{cp}{\cptrue\acptrue\twostagejnltrue\@draftfalse}
\DeclareOption{cpd}{\cpdtrue\acpdtrue\twostagejnltrue\@draftfalse\@headerstrue}
%% options for other Journals:
\DeclareOption{aglet}{\@aglettrue}
\DeclareOption{khb}{\@khbtrue\@egsbooktrue\@headersfalse}
%% other options:
\DeclareOption{online}{\onlinetrue}
\DeclareOption{hvmath}{\@hvmathtrue}
\DeclareOption{inbook}{\@egsbooktrue\@headersfalse}
\DeclareOption{fleqn}{\relax}
\DeclareOption{twoside}{\relax}
\DeclareOption{twocolumn}{\relax}
\DeclareOption{titlepage}{\relax}
\DeclareOption{headers}{\@headerstrue}
\DeclareOption{noheaders}{\@headersfalse}
\DeclareOption{nohyperref}{\@nohyperreftrue}
\DeclareOption{noline}{\@nolinetrue}
\DeclareOption{nolastpage}{\@nolastpagetrue}
\DeclareOption{noauthor}{\@noauthortrue}
\DeclareOption{nosref}{\@nosreftrue}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[fleqn,a4paper]{article}
\paperheight=277mm\paperwidth=210mm
\if@aglet\paperheight=283mm\paperwidth=213mm\fi
\ifacpd\paperheight=159mm\paperwidth=166mm\fi
\newif\if@ps
\ifx\pdfoutput\undefined%
  \IfFileExists{graphicx.sty}{\RequirePackage[dvips]{graphicx}% LaTeX
  \DeclareGraphicsExtensions{.eps,.ps}}{%
  \ClassWarningNoLine{egu}{Cannot find graphicx.sty; proceeding without it}}
  \if@aglet\IfFileExists{color.sty}{\RequirePackage{color}}{%
  \ClassWarningNoLine{egu}{Cannot find color.sty; proceeding without it}}\fi
  \@pstrue
\else%
  \IfFileExists{graphicx.sty}{\RequirePackage[pdftex]{graphicx}% pdfLaTeX
  \DeclareGraphicsExtensions{.pdf,.png,.jpg}}{%
  \ClassWarningNoLine{egu}{Cannot find graphicx.sty; proceeding without it}}
  \if@aglet\IfFileExists{color.sty}{\RequirePackage[pdftex]{color}}{%
  \ClassWarningNoLine{egu}{Cannot find color.sty; proceeding without it}}\fi
  \@psfalse
  \pdfinfo{/Creator (egu.cls version \clsversion)}
\fi
\IfFileExists{url.sty}%
  {\RequirePackage{url}\urlstyle{same}}%
  {\ClassWarningNoLine{egu}{Cannot find url.sty; proceeding without it}%
    \newcommand{\url}[1]{\ClassError{egu}%
      {To use \string\url, you must have url.sty}{Install url.sty}}}
\IfFileExists{textcomp.sty}%
  {\RequirePackage{textcomp}\let\permil\textperthousand}%
  {\ClassWarningNoLine{egu}{Cannot find textcomp.sty; proceeding without it}%
    \newcommand{\permil}{\ensuremath{\kern-.05em%
        \raise.5ex\hbox{\the\scriptfont0 0}%
        \kern-.15em/\kern-.15em\lower.25ex\hbox{\the\scriptfont0 00}}}%
    \def\textmu{\ensuremath{\mu}}
  }
\IfFileExists{times.sty}{\RequirePackage{times}}{%
  \ClassWarningNoLine{egu}{Cannot find times.sty; proceeding without it}}
\IfFileExists{fix2col.sty}{\RequirePackage{fix2col}}{%
  \ClassWarningNoLine{egu}{Cannot find fix2col.sty; proceeding without it}}
\IfFileExists{amssymb.sty}{\RequirePackage{amssymb}}{%
  \ClassWarningNoLine{egu}{Cannot find amssymb.sty; proceeding without it}}
\IfFileExists{ifthen.sty}{\RequirePackage{ifthen}}{%%K.S. 21.9.2001
  \ClassWarningNoLine{egu}{Cannot find ifthen.sty; proceeding without it}}
\IfFileExists{array.sty}{\RequirePackage{array}}{%%K.S. 21.9.2001
  \ClassWarningNoLine{egu}{Cannot find array.sty; proceeding without it}}
\IfFileExists{multicol.sty}{\RequirePackage{multicol}}{%%K.S. 27.11.2001
  \ClassWarningNoLine{egu}{Cannot find multicol.sty; proceeding without it}}
\newif\if@egs@home
\IfFileExists{pergamon.eps}{\@egs@hometrue}{\@egs@homefalse}
\IfFileExists{egs3.pdf}{\@egs@hometrue}{}
\IfFileExists{egu.pdf}{\@egs@hometrue}{}
\if@egs@home\ifacpd\if@hvmath\RequirePackage{hvmath}\RequirePackage{hvams}\fi\else%
\RequirePackage[mtbold]{mathtime}% qqq
\fi\fi
\@twosidefalse
\if@draft\@twocolumnfalse\else\@twocolumntrue\fi
\IfFileExists{lineno.sty}{\ifacpd\RequirePackage[pagewise,displaymath,mathlines,modulo]{lineno}
   \else\RequirePackage[displaymath,mathlines,modulo]{lineno}\fi}{%
   \ClassWarningNoLine{egu}{Cannot find lineno.sty; proceeding without it}}
\if@draft\IfFileExists{lineno.sty}{\linenumbers\renewcommand{\linenumberfont}{\normalfont\small\sffamily}}{}%
   \else\IfFileExists{lineno.sty}{\nolinenumbers}{}\fi
\if@egsbook\@twocolumnfalse\fi
\iftwostagejnl % make sure the author doesn't forget anything
  \newcommand{\UndefinedError}[1]
    {\ClassError{egu}{You must define #1}{Add #1 to your document!}}
  \newcommand{\NoSectionError}[2]
    {\ClassError{egu}{You forgot the section: #1}{Add #2 to your document!}}
  \AtEndDocument{%
    \@ifundefined{abstractexists}{\NoSectionError{Abstract}
      {\string\begin{abstract}...\string\end{abstract}}}{}
    \@ifundefined{introductionexists}{\NoSectionError{Introduction}
      {\string\introduction}}{}
    \@ifundefined{conclusionsexists}{\NoSectionError{Conclusions}
      {\string\conclusions}}{}
    \if!\@runauth!\UndefinedError{\string\runningauthor}\fi
    \if!\@runtit!\UndefinedError{\string\runningtitle}\fi
  }
  \def\acpurl{% \maketitle will define correct value for \acpurl
  \ClassError{egu}{Did you forget \string\maketitle\space ?}
  {Add \string\maketitle\space just before your abstract!}}
\fi
\ifacpd\@twocolumnfalse\fi
\setlength{\arraycolsep}{0.5mm}
\newcommand{\editorthanks}[3]{%
  \ifthenelse{\equal{#2}{}}% check if the first referee exits, if not write:
  {\\ \hspace*{4mm} Topical Editor\ #1\ thanks two unknown referees %
  for their help in evaluating this paper.}%
  {\ifthenelse{\equal{#3}{}}% check if second referee exist, if not write:
  {\\ \hspace*{4mm}Topical Editor\ #1\ thanks\ #2\ and another %
  referee for their help in evaluating this paper.}%
  {\\ \hspace*{4mm}Topical Editor\ #1\ thanks\ #2\ and\ #3\ for their %
  help in evaluating this paper.}}%
}
%% set a big mark in the text which needs to be checked by the author
\newcommand{\blackbox}{\rule{8mm}{3mm}}
\newcommand{\degree}{\ensuremath{^{\circ}}}
\newcommand{\tophline}{\hline\noalign{\vspace{1mm}}}
\newcommand{\middlehline}{\noalign{\vspace{1mm}}\hline\noalign{\vspace{1mm}}}
\newcommand{\bottomhline}{\noalign{\vspace{1mm}}\hline}
%% \hhline is obsolete and only kept for compatibility:
\newcommand{\hhline}{\noalign{\vspace{1mm}}\hline\noalign{\vspace{1mm}}}
\DeclareRobustCommand*{\vec}[1]{\ensuremath{%
\mathchoice{\mbox{\boldmath$\displaystyle#1$}}
           {\mbox{\boldmath$\textstyle#1$}}
           {\mbox{\boldmath$\scriptstyle#1$}}
           {\mbox{\boldmath$\scriptscriptstyle#1$}}}}
\def\testbx{bx}
\ifacpd
\if@hvmath
\DeclareRobustCommand*{\chem}[1]{\ensuremath{%
\mathcode`\-="0200\mathcode`\=="003D% no space around "-" and "="
\ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}
\DeclareRobustCommand*{\unit}[1]{\ensuremath{\def\mu{\mbox{\textmu}}\def~{\,}%
\ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}
\else
\let\mathrm\mathsf \let\rm\sf
\DeclareRobustCommand*{\chem}[1]{\ensuremath{%
\mathcode`\-="0200\mathcode`\=="003D% no space around "-" and "="
\ifx\testbx\f@series\mbox{\boldmath$\mathsf{#1}$}\else\mathsf{#1}\fi}}
\DeclareRobustCommand*{\unit}[1]{\ensuremath{\def\mu{\mbox{\textmu}}\def~{\,}%
\ifx\testbx\f@series\mbox{\boldmath$\mathsf{#1}$}\else\mathsf{#1}\fi}}
\fi
\else
\DeclareRobustCommand*{\chem}[1]{\ensuremath{%
\mathcode`-="0200\mathcode`\=="003D% no space around "-" and "="
\ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}
\DeclareRobustCommand*{\unit}[1]{\ensuremath{\def\mu{\mbox{\textmu}}\def~{\,}%
\ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}
\fi

\if@egs@home\if@nohyperref\else\ifacpd\else
   \usepackage[pdftex]{color}
   \usepackage[pdftex,bookmarks=false,colorlinks]{hyperref}
   \hypersetup{anchorcolor=black,citecolor=black,filecolor=black,linkcolor=black,%
   menucolor=black,pagecolor=black,urlcolor=black}
\fi\fi\fi

\ifacpd
\if@hvmath\else
  \let\textrm\textsf
  \let\rm\sf
\fi
\if@egs@home
\usepackage[pdftex]{color}
\usepackage[pdfpagelabels,pdftex,pagebackref,bookmarks=false,bookmarksopen,colorlinks]%
  {hyperref}
\ifonline%
  \usepackage[screen,rightpanel]{pdfscreen}
  \setlength{\panelwidth}{5cm}%
  \newlength{\bigbutton}%
  \setlength{\bigbutton}{0.75\panelwidth}%
  \newlength{\buttongap}%
  \setlength{\buttongap}{0.06\panelwidth}%
  \newlength{\smallbutton}%
  \setlength{\smallbutton}{0.5\bigbutton}%
  \addtolength{\smallbutton}{-0.5\buttongap}%
  \def\panelfont{\if@hvmath\rmfamily\fi\scriptsize}
\else%
  \usepackage[print]{pdfscreen}%
\fi
\def\PDFSCR@Warning#1{} % no more "No overlay specified" warnings
\definecolor{bgrot}{rgb}{0.62,0.04,0.05}
\definecolor{osblau}{cmyk}{1.00,0.60,0.00,0.40}
\definecolor{hessblau}{cmyk}{0.99,0.87,0.00,0.00}
\definecolor{cpgelb}{cmyk}{0.00,0.20,1.00,0.00}
\ifbgd
   \hypersetup{pdffitwindow=true,pdfmenubar=true,pdftoolbar=true,pdfwindowui=true,%
     anchorcolor=blue,citecolor=blue,filecolor=blue,linkcolor=blue,menucolor=blue,%
     pagecolor=blue,urlcolor=bgrot}
\else\ifosd
   \hypersetup{pdffitwindow=true,pdfmenubar=true,pdftoolbar=true,pdfwindowui=true,%
     anchorcolor=blue,citecolor=blue,filecolor=blue,linkcolor=blue,menucolor=blue,%
     pagecolor=blue,urlcolor=osblau}
\else\ifhessd
   \hypersetup{pdffitwindow=true,pdfmenubar=true,pdftoolbar=true,pdfwindowui=true,%
     anchorcolor=blue,citecolor=blue,filecolor=blue,linkcolor=blue,menucolor=blue,%
     pagecolor=blue,urlcolor=hessblau}
\else\ifcpd
   \hypersetup{pdffitwindow=true,pdfmenubar=true,pdftoolbar=true,pdfwindowui=true,%
     anchorcolor=blue,citecolor=blue,filecolor=blue,linkcolor=blue,menucolor=blue,%
     pagecolor=blue,urlcolor=cpgelb}
\else
   \hypersetup{pdffitwindow=true,pdfmenubar=true,pdftoolbar=true,pdfwindowui=true,%
     anchorcolor=blue,citecolor=blue,filecolor=blue,linkcolor=blue,menucolor=blue,%
     pagecolor=blue,urlcolor=blue}
\fi\fi\fi\fi
\setlength\footskip{5mm}
\pagestyle{plain}
\margins{10mm}{10mm}{7mm}{7mm}
\screensize{15.9cm}{21.6cm}
\definecolor{backgroundcolor}{rgb}{1.,1.,1.} % white
\ifbgd
   \definecolor{panelbackground}{rgb}{.89,.93,.76} % also used for inactive buttons!
   \definecolor{buttonbackground}{rgb}{.61,.70,.15}
   \definecolor{buttonshadow}{rgb}{.61,.70,.15}
\else\ifosd
   \definecolor{panelbackground}{cmyk}{0.18,0.08,0.05,0.00} % also used for inactive buttons!
   \definecolor{buttonbackground}{cmyk}{0.50,0.30,0.00,0.10}
   \definecolor{buttonshadow}{cmyk}{0.50,0.30,0.00,0.10}
\else\ifhessd
   \definecolor{panelbackground}{cmyk}{0.19,0.00,0.27,0.00} % also used for inactive buttons!
   \definecolor{buttonbackground}{cmyk}{0.65,0.00,1.00,0.00}
   \definecolor{buttonshadow}{cmyk}{0.65,0.00,1.00,0.00}
\else\ifcpd
   \definecolor{panelbackground}{cmyk}{0.20,0.20,0.00,0.06} % also used for inactive buttons!
   \definecolor{buttonbackground}{cmyk}{0.55,0.55,0.00,0.00}
   \definecolor{buttonshadow}{cmyk}{0.55,0.55,0.00,0.00}
\else % acpd
   \definecolor{panelbackground}{rgb}{.8,.95,1.} % also used for inactive buttons!
   \definecolor{buttonbackground}{rgb}{0.5,.85,1.}
\fi\fi\fi\fi
\ifonline\backgroundcolor{backgroundcolor}\fi
\definecolor{section0}{rgb}{0.,0.,0.} % black
\definecolor{section1}{rgb}{0.,0.,0.} % black
\definecolor{section2}{rgb}{0.,0.,0.} % black
\definecolor{section3}{rgb}{0.,0.,0.} % black
\definecolor{section4}{rgb}{0.,0.,0.} % black
\def\panel{\colorbox{panelbackground}
{\begin{minipage}[t][\paperheight][t]{\panelwidth}
%% set default font for panel:
\if@hvmath\else\fontfamily{\sfdefault}\fi\fontseries{m}\fontshape{n}\fontsize{11}{13.7pt}\selectfont
%% journal name, volume, pages, year, runningtitle, runningauthor:
\null\vspace*{7mm}\centering\parbox[t]{0.8\panelwidth}%
{\centering %
  {\Large\bfseries{\ifbgd\href{http://www.biogeosciences.net/bgd.htm}{BGD}
                   \else\ifosd\href{http://www.ocean-science.net/osd.htm}{OSD}
                   \else\ifhessd\href{http://www.copernicus.org/EGU/hess/hessd.htm}{HESSD}
                   \else\ifcpd\href{http://www.climate-of-the-past.net/cpd.htm}{CPD}
                   \else\href{http://www.atmos-chem-phys.org/acpd.htm}{ACPD}\fi\fi\fi\fi}}\\[2mm]
  {\small\@pvol, \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear}\\[1mm]
  \rule{0.8\panelwidth}{1.1pt}\\[1mm]
  {\bfseries\mbox{}\@runtit}\\[3mm]
  {\small\mbox{}\@runauth}\\[0mm]
  \rule{0.8\panelwidth}{1.1pt}\\[5mm]
} % end of centering
%% the buttons:
\addButton{\bigbutton}{\hyperlink{title}{\hfill\color{black}Title Page\hfill}}\\[1.5mm]
\addButton{\smallbutton}{\hyperlink{abstr}{\hfill\color{black}Abstract\hfill}}%
\hspace{\buttongap}%
\addButton{\smallbutton}{\hyperlink{intro}{\hfill\color{black}Introduction\hfill}}\\[1.5mm]
\addButton{\smallbutton}{\hyperlink{conclusions}{\hfill\color{black}Conclusions\hfill}}%
\hspace{\buttongap}%
\addButton{\smallbutton}{\hyperlink{references}{\hfill\color{black}References\hfill}}\\[1.5mm]
\@ifundefined{@numtable}% automatically defined via \@num\@captype
  {{\ifbgd\definecolor{buttonbackground}{rgb}{.89,.93,.76}
    \else\ifosd\definecolor{buttonbackground}{rgb}{.81,.86,.90}
    \else\ifhessd\definecolor{buttonbackground}{rgb}{.82,.91,.78}
    \else\ifcpd\definecolor{buttonbackground}{rgb}{.73,.66,.79}
    \else\definecolor{buttonbackground}{rgb}{.8,.95,1.}\fi\fi\fi\fi%
  \addButton{\smallbutton}{{\hfill\color{black}Tables\hfill}}}}%
  {\addButton{\smallbutton}{\hyperlink{table}%
      {\hfill\color{black}Tables\hfill}}}%
\hspace{\buttongap}%
\@ifundefined{@numfigure}% automatically defined via \@num\@captype
  {{\ifbgd\definecolor{buttonbackground}{rgb}{.89,.93,.76}
    \else\ifosd\definecolor{buttonbackground}{rgb}{.81,.86,.90}
    \else\ifhessd\definecolor{buttonbackground}{rgb}{.82,.91,.78}
    \else\ifcpd\definecolor{buttonbackground}{rgb}{.73,.66,.79}
    \else\definecolor{buttonbackground}{rgb}{.8,.95,1.}\fi\fi\fi\fi%
  \addButton{\smallbutton}{{\hfill\color{black}Figures\hfill}}\\[4mm]}}
  {\addButton{\smallbutton}{\hyperlink{figure}%
      {\hfill\color{black}Figures\hfill}}\\[4mm]}%
\Acrobatmenu{FirstPage}{\addButton{\smallbutton}%
  {$\rule[-0.144ex]{0.35ex}{1.244ex}\!\blacktriangleleft$}}%
\hspace{\buttongap}%
\Acrobatmenu{LastPage}{\addButton{\smallbutton}%
  {$\blacktriangleright\!\rule[-0.144ex]{0.35ex}{1.244ex}$}}\\[1.5mm]
\Acrobatmenu{PrevPage}{\addButton{\smallbutton}{$\blacktriangleleft$}}%
\hspace{\buttongap}%
\Acrobatmenu{NextPage}{\addButton{\smallbutton}{$\blacktriangleright$}}\\[1.5mm]
\Acrobatmenu{GoBack}{\addButton{\smallbutton}{Back}}%
\hspace{\buttongap}%
\Acrobatmenu{Close}{\addButton{\smallbutton}{Close}}\\[1.5mm]
\Acrobatmenu{FullScreen}{\addButton{\bigbutton}{Full Screen / Esc}}\\[4mm]
\ifbgd
  \addButton{\bigbutton}{\href{http://\bgdurl/bgd-\@pvol-\@fpage_p.pdf}%
    {\hfill Print Version\hfill}}\\[1.5mm]
  \addButton{\bigbutton}{\href{http://\bgdurl/comments.php}%
    {\hfill Interactive Discussion\hfill}}\\[4mm]
\else\ifosd
  \addButton{\bigbutton}{\href{http://\osdurl/osd-\@pvol-\@fpage_p.pdf}%
    {\hfill Print Version\hfill}}\\[1.5mm]
  \addButton{\bigbutton}{\href{http://\osdurl/comments.php}%
    {\hfill Interactive Discussion\hfill}}\\[4mm]
\else\ifhessd
  \addButton{\bigbutton}{\href{http://\hessdurl/hessd-\@pvol-\@fpage_p.pdf}%
    {\hfill Print Version\hfill}}\\[1.5mm]
  \addButton{\bigbutton}{\href{http://\hessdurl/comments.php}%
    {\hfill Interactive Discussion\hfill}}\\[4mm]
\else\ifcpd
  \addButton{\bigbutton}{\href{http://\cpdurl/cpd-\@pvol-\@fpage_p.pdf}%
    {\hfill Print Version\hfill}}\\[1.5mm]
  \addButton{\bigbutton}{\href{http://\cpdurl/comments.php}%
    {\hfill Interactive Discussion\hfill}}\\[4mm]
\else
  \addButton{\bigbutton}{\href{http://\acpdurl/acpd-\@pvol-\@fpage_p.pdf}%
    {\hfill Print Version\hfill}}\\[1.5mm]
  \addButton{\bigbutton}{\href{http://\acpdurl/comments.php}%
    {\hfill Interactive Discussion\hfill}}\\[4mm]
\fi\fi\fi\fi
\href{http://www.copernicus.org/EGU/EGU.html}{{\if@egs@home\fontfamily{ma1}\selectfont\fi EGU}}
\null\vspace*{10mm}
\end{minipage}}}
\linenumbers
\def\equationname{Eq.}  % different name in new hyperref version?
\def\sectionname{Sect.} % different name in new hyperref version?
\fi\fi
\def\refname{References}
\def\figurename{Fig.}
\def\tablename{Table}
\def\fnum@figure{\ifacpd\if@hvmath\rm\else\sf\fi\bfseries\fi\figurename~\thefigure}
\def\fnum@table{\ifacpd\if@hvmath\rm\else\sf\fi\bfseries\fi\tablename~\thetable}
\if@egsbook
 \def\abstractname{Resume}
\else
 \def\abstractname{Abstract}
\fi
\def\acknowname{Acknowledgement}
\def\acknowsname{Acknowledgements}
\if@khb
\IfFileExists{german.sty}{\RequirePackage{german}}{%
  \ClassWarningNoLine{egu}{Cannot find german.sty; proceeding without it}
  \@ifundefined{language}{\newcount\language \language=0}{}%
  \providecommand{\selectlanguage}[1]{\csname captions##1\endcsname
   \language=\csname l@##1\endcsname\relax}%
  \providecommand{\iflanguage}[3]{%
    \expandafter\ifx\csname l@##1\endcsname\relax ##3%
    \else \ifnum\csname l@##1\endcsname=\language ##2%
    \else ##3\fi\fi}%
  \@ifundefined{l@english}{\chardef\l@english=255}{}%
  \@ifundefined{l@german}{\chardef\l@german=254}{}%
}
\def\captionsgerman{%
  \def\acknowname{Danksagung}%
  \def\acknowsname{Danksagungen}%
  \def\refname{Schrifttum}%
  \def\abstractname{Zusammenfassung}%
  \def\appendixname{Anhang}%
  \def\figurename{Bild}%
  \def\tablename{Tabelle}}
\def\captionsenglish{%
  \def\acknowname{Acknowledgement}%
  \def\acknowsname{Acknowledgements}%
  \def\refname{References}%
  \def\abstractname{Abstract}%
  \def\appendixname{Appendix}%
  \def\figurename{Figure}%
  \def\tablename{Table}}
\selectlanguage{german}
\fi
\if@draft
\oddsidemargin  0cm
\evensidemargin 0cm
\textheight 22cm
\textwidth 14cm
\topmargin -1cm
\raggedbottom
\let\subsecsize=\normalsize
\else
\ifacpd
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xipt{13.6}%
   \abovedisplayskip 11\p@ \@plus3\p@ \@minus6\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6.5\p@ \@plus3.5\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\renewcommand\small{%
   \@setfontsize\small\@xpt\@xiipt
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 6\p@ \@plus2\p@ \@minus2\p@
               \parsep 3\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip}
\let\footnotesize=\small
\renewcommand\scriptsize{\@setfontsize\scriptsize\@viiipt{9.5}}
\renewcommand\tiny{\@setfontsize\tiny\@vipt\@viipt}
\renewcommand\large{\@setfontsize\large\@xiipt{14}}
\renewcommand\Large{\@setfontsize\Large\@xivpt{18}}
\renewcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\renewcommand\huge{\@setfontsize\huge\@xxpt{25}}
\renewcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}
\else
\def\@normalsize{\@setsize\normalsize{12pt}\xpt\@xpt
\abovedisplayskip 10\p@ plus2\p@ minus5\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip  \z@ plus3\p@
\belowdisplayshortskip  6\p@ plus3\p@ minus3\p@
\let\@listi\@listI}
\def\small{%
\if@egsbook
  \@setsize\small{9pt}\viiipt\@viiipt
\else
  % \@setsize\small{10pt}\viiipt\@viiipt
  \@setsize\small{11pt}\ixpt\@ixpt
\fi
\abovedisplayskip 8.5\p@ plus3\p@ minus4\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus2\p@
\belowdisplayshortskip 4\p@ plus2\p@ minus2\p@
\def\@listi{\leftmargin\leftmargini
\topsep 4\p@ plus2\p@ minus2\p@\parsep 2\p@ plus\p@ minus\p@
\itemsep \parsep}}
\let\footnotesize=\small
\def\scriptsize{\@setsize\scriptsize{8pt}\viipt\@viipt}
\def\tiny{\@setsize\tiny{6pt}\vpt\@vpt}
\def\large{\@setsize\large{14pt}\xiipt\@xiipt}
\def\Large{\@setsize\Large{18pt}\xivpt\@xivpt}
\def\LARGE{\@setsize\LARGE{22pt}\xviipt\@xviipt}
\def\huge{\@setsize\huge{25pt}\pt\@xxpt}
\def\Huge{\@setsize\Huge{30pt}\xxvpt\@xxvpt}
\if@egsbook
  \def\subsecsize{\@setsize\subsecsize{12pt}\xipt\@xipt}
\fi
\fi

\normalsize
\oddsidemargin  -0.9cm % 0cm
\evensidemargin -0.9cm % 0cm
\topmargin -3.0cm % -2.1cm
\if@aglet \evensidemargin -0.6cm \topmargin -2.7cm \fi
\if@ps\topmargin -1.0cm\fi
\headheight 2cm
\headsep 0.5cm
\topskip = 12pt
\footskip 30pt
\ifacpd
  \topmargin -1.84cm \headheight 0pt \headsep 0pt \footskip 5mm
  \oddsidemargin -1.54cm \evensidemargin -1.54cm \topskip 0pt
\fi
\if@egsbook
  \textwidth 11.7cm
  \textheight = 45\baselineskip
\else
  \textwidth 17.7cm
  \textheight = 56\baselineskip
\fi
\ifacp \textheight = 54\baselineskip \fi
\ifacpd \textheight 140mm \textwidth 146mm \fi
\if@khb
  \textwidth 16.9cm
  \textheight = 56\baselineskip
  \oddsidemargin -0.54cm
  \evensidemargin -0.54cm
\fi
\advance\textheight by \topskip
\columnsep 0.7cm
\columnseprule 0pt
\fi
\parindent 1em
\parskip 0pt plus 1pt
\if@egsbook
\def\section{\@startsection {section}{1}{\z@}{-2em plus-1ex minus
    -.2ex}{1em plus.2ex}{\reset@font\large
             \bfseries
}}
\def\subsection{\@startsection{subsection}{2}{\z@}{-1em plus-0.25ex
    minus-.2ex}{1em plus.2ex}{\reset@font\subsecsize
             \bfseries
}}
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{-1em plus
    -0.25ex minus-.2ex}{1em plus.2ex}{\reset@font\normalsize
             \bfseries
}}
\def\paragraph{\@startsection{paragraph}{4}{\z@}{1em plus
    0.25ex minus.2ex}{-1ex plus-.2ex}{\reset@font\normalsize
             \bfseries
}}
\else
\def\section{\@startsection {section}{1}{\z@}{-2em plus-1ex minus
    -.2ex}{1em plus.2ex}{\reset@font\normalsize
             \bfseries
}}
\def\subsection{\@startsection{subsection}{2}{\z@}{-1em plus-0.25ex
    minus-.2ex}{1em plus.2ex}{\reset@font\normalsize
             \mdseries
}}
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{-1em plus
    -0.25ex minus-.2ex}{1em plus.2ex}{\reset@font\normalsize
             \mdseries
}}
\fi
\if@khb\else\let\paragraph=\subsubsection\fi
\let\subparagraph=\paragraph
\ifacpd\if@hvmath\let\reset@font=\rm\else\let\reset@font=\sf\fi\fi
\@ifundefined{reset@font}{\let\reset@font=\relax}{}
\setcounter{secnumdepth}{3}
%% code copied from latex.ltx and renamed: equation->reaction, eqn->rxn
\def\rxnlabel{R}
\@definecounter{reaction}
\def\reaction{$$\refstepcounter{reaction}}
\def\endreaction{\eqno \hbox{\@rxnnum}$$\@ignoretrue}
\def\@rxnnum{{\normalfont \normalcolor (\rxnlabel\thereaction)}}
\def\stackrel#1#2{\mathrel{\mathop{#2}\limits^{#1}}}
\def\frac#1#2{{\begingroup#1\endgroup\over#2}}
\DeclareRobustCommand\sqrt{\@ifnextchar[\@sqrt\sqrtsign}
\def\@sqrt[#1]{\root #1\of}
\newcount\@rxcnt
\newcount\@rxpen
\newskip\@centering
\@centering = 0pt plus 1000pt
\def\rxnarray{%
   \stepcounter{reaction}%
   \def\@currentlabel{\p@reaction\rxnlabel\thereaction}%
   \global\@eqnswtrue
   \m@th
   \global\@rxcnt\z@
   \tabskip\@centering
   \let\\\@rxncr
   $$\everycr{}\halign to\displaywidth\bgroup
       \hskip\@centering$\displaystyle\tabskip\z@skip{##}$\@rxnsel
      &\global\@rxcnt\@ne\hskip \tw@\arraycolsep \hfil${##}$\hfil
      &\global\@rxcnt\tw@ \hskip \tw@\arraycolsep
         $\displaystyle{##}$\hfil\tabskip\@centering
      &\global\@rxcnt\thr@@ \hb@xt@\z@\bgroup\hss##\egroup
         \tabskip\z@skip
      \cr
}
\def\endrxnarray{%
      \@@rxncr
      \egroup
      \global\advance\c@reaction\m@ne
   $$\@ignoretrue
}
\let\@rxnsel=\relax
\def\@rxncr{%
   {\ifnum0=`}\fi
   \@ifstar{%
      \global\@rxpen\@M\@yrxncr
   }{%
      \global\@rxpen\interdisplaylinepenalty \@yrxncr
   }%
}
\def\@yrxncr{\@testopt\@xrxncr\z@skip}
\def\@xrxncr[#1]{%
   \ifnum0=`{\fi}%
   \@@rxncr
   \noalign{\penalty\@rxpen\vskip\jot\vskip #1\relax}%
}
\def\@@rxncr{\let\reserved@a\relax
    \ifcase\@rxcnt \def\reserved@a{& & &}\or \def\reserved@a{& &}%
     \or \def\reserved@a{&}\else
       \let\reserved@a\@empty
       \@latex@error{Too many columns in rxnarray environment}\@ehc\fi
     \reserved@a \if@eqnsw\@rxnnum\stepcounter{reaction}\fi
     \global\@eqnswtrue\global\@rxcnt\z@\cr}
\let\@srxncr=\@rxncr
\@namedef{rxnarray*}{\def\@rxncr{\nonumber\@srxncr}\rxnarray}
\@namedef{endrxnarray*}{\nonumber\endrxnarray}
\def\leftrxn#1{\rlap{$\displaystyle #1$}}
%% code copied from from fleqn.clo and renamed: equation->reaction, eqn->rxn
%% this probably only works if [fleqn] is already loaded
\renewenvironment{reaction}%
    {\@beginparpenalty\predisplaypenalty
     \@endparpenalty\postdisplaypenalty
     \refstepcounter{reaction}%
     \def\@currentlabel{\p@reaction\rxnlabel\thereaction}%
     \trivlist \item[]\leavevmode
       \hb@xt@\linewidth\bgroup $\m@th% $
         \displaystyle
         \hskip\mathindent}%
        {$\hfil % $
         \displaywidth\linewidth\hbox{\@rxnnum}%
       \egroup
     \endtrivlist}
\renewenvironment{rxnarray}{%
    \stepcounter{reaction}%
    \def\@currentlabel{\p@reaction\rxnlabel\thereaction}%
    \global\@eqnswtrue\m@th
    \global\@rxcnt\z@
    \tabskip\mathindent
    \let\\=\@rxncr
    \setlength\abovedisplayskip{\topsep}%
    \ifvmode
      \addtolength\abovedisplayskip{\partopsep}%
    \fi
    \addtolength\abovedisplayskip{\parskip}%
    \setlength\belowdisplayskip{\abovedisplayskip}%
    \setlength\belowdisplayshortskip{\abovedisplayskip}%
    \setlength\abovedisplayshortskip{\abovedisplayskip}%
    $$\everycr{}\halign to\linewidth% $$
    \bgroup
      \hskip\@centering
      $\displaystyle\tabskip\z@skip{##}$\@rxnsel&%
      \global\@rxcnt\@ne \hskip \tw@\arraycolsep \hfil${##}$\hfil&%
      \global\@rxcnt\tw@ \hskip \tw@\arraycolsep
        $\displaystyle{##}$\hfil \tabskip\@centering&%
      \global\@rxcnt\thr@@
        \hb@xt@\z@\bgroup\hss##\egroup\tabskip\z@skip\cr}%
      {\@@rxncr
    \egroup
    \global\advance\c@reaction\m@ne$$% $$
    \@ignoretrue
    } % end of eqn/rxn code
\if@khb \def\desfont{\bfseries} \else \def\desfont{\itshape}\fi
\def\descriptionlabel#1{\hspace\labelsep \desfont #1.}
\def\description{\list{}{\labelwidth\z@ \leftmargin\z@ \itemindent-\leftmargin
       \let\makelabel\descriptionlabel\reset@font\small}}

\let\enddescription\endlist
\setcounter{topnumber}{2}
\def\topfraction{1.}
\setcounter{bottomnumber}{1}
\def\bottomfraction{1.}
\setcounter{totalnumber}{3}
\def\textfraction{0}
\def\floatpagefraction{.7}
\setcounter{dbltopnumber}{2}
\def\dbltopfraction{1.}
\def\dblfloatpagefraction{.7}
\long\def\@makecaption#1#2{%
  \vskip 10\p@
  {\reset@font\small{\bf #1.} #2\par}
  \ifonline\ifnum\csname the\@captype\endcsname=1 % for 1st fig or tab only
    \immediate\write\@auxout{\string\gdef\string\@num\@captype{}}%
    \hypertarget{\@captype}{}%
  \fi\fi
}

\long\def\@makenarrowcap#1#2{\hfill\vbox to \z@{\hsize=5cm \vss
     \reset@font\small{\bf #1.} #2\par\null}}
\def\figbox{\@ifstar{\let\egsbox\makebox\@figbox}
  {\let\egsbox\framebox\@figbox}}
\def\@figbox#1#2#3{\setbox\@tempboxa=\hbox{%
  \if!#1!\egsbox{#3}\else
  \egsbox[#1][c]{\if!#2!#3\else\@tempdima#2\relax
  \divide\@tempdima by2\relax
  \advance\@tempdima by-\fboxsep \advance\@tempdima by-\fboxrule
  \vrule\@height\@tempdima\@depth\@tempdima\@width\z@
  \vbox to \z@{\vss\hbox{#3}\vss}\fi}\fi}%
  \@tempdima=\hsize \advance\@tempdima by-\wd\@tempboxa
  \box\@tempboxa
  \if@egsbook\else
  \ifdim\@tempdima>5.5cm \let\@makecaption=\@makenarrowcap\fi
  \fi}
\let\@oxfloat=\@xfloat
\def\@xfloat#1[#2]{\@oxfloat#1[#2]\small}
\if@twocolumn
\sloppy\clubpenalty4000\widowpenalty4000
\leftmargini 2em
\leftmarginii 1.75em
\leftmarginiii 1.5em
\leftmarginiv 1em
\leftmarginv .5em
\leftmarginvi .5em
\fi
\def\labelitemi{\bf --}
\def\labelitemii{\bf --}
\def\labelitemiii{$\m@th\bullet$}
\def\labelitemiv{$\m@th\cdot$}
\def\maketitle{%
%%\setcounter{Maxaffil}{1} % deleted because it may prevent footnotes
 \if@khb \renewcommand\Authand{ und }%
         \renewcommand\Authands{ und }\fi
 \ifacp
   \ifbg
     \def\bgref{Biogeosciences\if@nosref\else, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi} % bg reference
     \def\bgurl{www.biogeosciences.net\if@nosref\else/bg/\@pvol/\@fpage\fi} % bg url
     \def\supplement##1{{%
       \def\-{\discretionary{}{}{}}% line break ok but no hyphen
       http://www.\-biogeosciences.\-net/\-bg/\-\@pvol/\-\@fpage%
       /\-bg-\@pvol-\@fpage-sp.##1}}
     \def\bgdoi{10.00000/bg-\@pvol-\@fpage} % bg doi
     \def\@journal{\BG}
   \else\ifos
     \def\osref{Ocean Science\if@nosref\else, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi} % os reference
     \def\osurl{www.ocean-science.net\if@nosref\else/os/\@pvol/\@fpage\fi} % os url
     \def\supplement##1{{%
       \def\-{\discretionary{}{}{}}% line break ok but no hyphen
       http://www.\-ocean-science.\-net/\-os/\-\@pvol/\-\@fpage%
       /\-os-\@pvol-\@fpage-sp.##1}}
     \def\osdoi{10.00000/os-\@pvol-\@fpage} % os doi
     \def\@journal{\OS}
   \else\ifhess
     \def\hessref{Hydrology and Earth System Sciences\if@nosref\else, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi} % hess reference
     \def\hessurl{www.copernicus.org/EGU/hess\if@nosref\else/hess/\@pvol/\@fpage\fi} % hess url
     \def\supplement##1{{%
       \def\-{\discretionary{}{}{}}% line break ok but no hyphen
       http://www.\-copernicus.\-org/\-EGU/\-hess/\-hess/\-\@pvol/\-\@fpage%
       /\-hess-\@pvol-\@fpage-sp.##1}}
     \def\hessdoi{10.00000/hess-\@pvol-\@fpage} % hess doi
     \def\@journal{\HESS}
   \else\ifcp
     \def\cpref{Climate of the Past\if@nosref\else, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi} % cp reference
     \def\cpurl{www.climate-of-the-past.net\if@nosref\else/cp/\@pvol/\@fpage\fi} % cp url
     \def\supplement##1{{%
       \def\-{\discretionary{}{}{}}% line break ok but no hyphen
       http://www.\-climate-of-the-past.\-net/\-cp/\-\@pvol/\-\@fpage%
       /\-cp-\@pvol-\@fpage-sp.##1}}
     \def\cpdoi{10.00000/cp-\@pvol-\@fpage} % cp doi
     \def\@journal{\CP}
   \else
     \def\acpref{Atmos.~Chem.~Phys.\if@nosref\else, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi} % acp reference
     \def\acpurl{www.atmos-chem-phys.org\if@nosref\else/acp/\@pvol/\@fpage\fi} % acp url
     \def\supplement##1{{%
       \def\-{\discretionary{}{}{}}% line break ok but no hyphen
       http://www.\-atmos-chem-phys.\-org/\-acp/\-\@pvol/\-\@fpage%
       /\-acp-\@pvol-\@fpage-sp.##1}}
     \def\acpdoi{10.00000/acp-\@pvol-\@fpage} % acp doi
     \def\@journal{\ACP}
   \fi\fi\fi\fi
   \ifodd\@fpage\else
     \ClassError{egu}
       {Starting page must be odd!}{change the firstpage command}
   \fi
 \fi
 \ifacpd
   \ifonline\hypertarget{title}{}\fi
   \ifbgd
     \def\bgdref{Biogeosciences~Discussions, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear} % bgd reference
     \def\bgdurl{www.biogeosciences.net/bgd/\@pvol/\@fpage} % bgd url
     \def\supplement##1{\expandafter
       {\csname url\endcsname{http://\bgdurl/bgd-\@pvol-\@fpage-sp.##1}}}
     \def\bgddoi{10.00000/bgd-\@pvol-\@fpage} % bgd doi
     \def\@journal{\BGD}
   \else\ifosd
     \def\osdref{Ocean~Science~Discussions, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear} % osd reference
     \def\osdurl{www.ocean-science.net/osd/\@pvol/\@fpage} % osd url
     \def\supplement##1{\expandafter
       {\csname url\endcsname{http://\osdurl/osd-\@pvol-\@fpage-sp.##1}}}
     \def\osddoi{10.00000/osd-\@pvol-\@fpage} % osd doi
     \def\@journal{\OSD}
   \else\ifhessd
     \def\hessdref{Hydrol.~Earth~Sys.~Sci.~Discuss., \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear} % hessd reference
     \def\hessdurl{www.copernicus.org/EGU/hess/hessd/\@pvol/\@fpage} % hessd url
     \def\supplement##1{\expandafter
       {\csname url\endcsname{http://\hessdurl/hessd-\@pvol-\@fpage-sp.##1}}}
     \def\hessddoi{10.00000/hessd-\@pvol-\@fpage} % hessd doi
     \def\@journal{\HESSD}
   \else\ifcpd
     \def\cpdref{Climate~of~the~Past~Discussions, \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear} % cpd reference
     \def\cpdurl{www.climate-of-the-past.net/cpd/\@pvol/\@fpage} % cpd url
     \def\supplement##1{\expandafter
       {\csname url\endcsname{http://\cpdurl/cpd-\@pvol-\@fpage-sp.##1}}}
     \def\cpddoi{10.00000/cpd-\@pvol-\@fpage} % cpd doi
     \def\@journal{\CPD}
   \else
     \def\acpdref{Atmos.~Chem.~Phys.~Discuss., \@pvol,
       \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear} % acpd reference
     \def\acpdurl{www.atmos-chem-phys.org/acpd/\@pvol/\@fpage} % acpd url
     \def\supplement##1{\expandafter
       {\csname url\endcsname{http://\acpdurl/acpd-\@pvol-\@fpage-sp.##1}}}
     \def\acpddoi{10.00000/acpd-\@pvol-\@fpage} % acpd doi
     \def\@journal{\ACPD}
   \fi\fi\fi\fi
 \fi
\if@egs@home\else
\iftwostagejnl\else
 \begin{titlepage}%
 \IfFileExists{lineno.sty}{\begin{nolinenumbers}}{}
 \if@draft\else\hsize=16cm\fi
 \let\footnotesize\small
 \let\footnoterule\relax
 \def\thefootnote{\fnsymbol{footnote}}%
 \setcounter{page}{0}%
 \null
 \vfil
 \vskip 60\p@
 \begin{center}%
   {\LARGE\bf \@title \par}%
   \vskip 3em
   {\large
     \lineskip .75em
     \@author \par}%
    \vfil
   {\large
    \if@draft Manuscript submitted to \else Manuscript submitted to \fi\\[1ex]
          {\bf \@journal}\\[1ex]
    \if@draft \@date\else Manuscript-No. \@msnumber\fi
    \if!\@offsets!\else\\[2em]{\bf Offset requests to:}\\ \@offsets\fi
    \if@draft\if!\@proofs!\else\\[2em]{\bf Send proofs to:}\\ \@proofs\fi\fi
    \par}%
\end{center}\par
\vfil
\@thanks
\vfil
\null
\IfFileExists{lineno.sty}{\end{nolinenumbers}}{}
\end{titlepage}%
\fi\fi
\if@headers\setcounter{page}{\@fpage}
\thispagestyle{default}
\xdef\@temp{\@journal}
\ifacp\ifbg\thispagestyle{bg}\else\ifos\thispagestyle{os}\else\ifhess\thispagestyle{hess}\else\ifcp\thispagestyle{cp}\else\thispagestyle{acp}\fi\fi\fi\fi\fi
\ifx\@temp\NPG\thispagestyle{npg}\fi
\ifx\@temp\AG\thispagestyle{ag}\fi
\ifx\@temp\NHESS\thispagestyle{nhess}\fi
\ifx\@temp\ARS\thispagestyle{ars}\fi
\ifx\@temp\SG\thispagestyle{sg}\fi
\ifx\@temp\ADGEO\thispagestyle{adgeo}\fi
\if@mueller\thispagestyle{mueller}\fi
\fi
\setcounter{footnote}{0}%
\begingroup
\def\@thanks{}\parindent=0pt
\if@twocolumn % generic layout for first page in twocolumn mode
   \twocolumn[%
    {\begin{minipage}[b][\@tithei][s]{\hsize}
   \null\vfil
    \begin{minipage}{\hsize}
    \raggedright
 \let\footnotesize\normalsize
 \let\footnoterule\relax
 \def\thefootnote{\fnsymbol{footnote}}%
   \def\@mpfn{footnote}%
   \let\thempfn=\thefootnote
   \renewcommand\@makefntext[1]{%
      \parindent 1em \noindent
      \@makefnmark##1}
   \if@aglet{\normalfont\Large\itshape Letter to the Editor}\\[2.5mm]%
{\LARGE\bfseries\@title\par}\else{\LARGE\bfseries\@title\par}\fi
\if@noauthor\else
   \vspace{10pt}
   {\normalsize
     \renewcommand{\Authfont}{\bfseries}%
     \renewcommand{\Affilfont}{\@affilfontsize\mdseries}%
     \setlength{\affilsep}{4pt}%
     \baselineskip 15pt
     \let\and=\sw@and
     \begin{minipage}[t]{\hsize}\flushleft\baselineskip=12pt%
        \@author
     \end{minipage}\par}%
\fi
     \@thanks
   \end{minipage}%
   \if!\@recvd!\else
     \par\vspace{10pt}
     \ifacp
        Received: \@recvd\ -- Published in
          \ifbg Biogeosciences Discussions:
          \else\ifos Ocean Science Discussions:
          \else\ifhess Hydrology and Earth System Sciences Discussions:
          \else\ifcp Climate of the Past Discussions:
          \else Atmos. Chem. Phys. Discuss.:
          \fi\fi\fi\fi\@pubdiscuss\\
        Revised: \@revsd\ -- Accepted: \@accptd\ -- Published: \@published
     \else
        Received: \@recvd\ \if!\@revsd!\else -- Revised: \@revsd\ \fi
        \if!\@accptd!\else -- Accepted: \@accptd\ -- Published: \@published \fi
     \fi
   \fi
   \if!\@speciss!\else
      \par\vspace{10pt}
      \if@sil Part of Special Issue \href{\@specissurl}{``\@speciss''} \else
      Part of Special Issue ``\@speciss'' \fi
   \fi
   \vspace{20pt}%
   \end{minipage}%
   }]\else
\global\@topnum\z@
\ifacpd % acpd/bgd title page starts here
  \if@egs@home\begin{nolinenumbers}\fi %
  \ifbgd
    \parbox[t]{9.5cm}{\bgdref\\\bgdurl/\\
      SRef-ID: 1810-6285/bgd/\@pyear-\@pvol-\@fpage\\ %
      % DOI: \bgddoi\\ % activate when doi number has been assigned
      European Geosciences Union} %
    \hfill\@bgdlogo\\[8mm]
    \scalebox{0.977}[1.0]{{\slshape Biogeosciences Discussions} is the access
      reviewed discussion forum of {\slshape Biogeosciences}}
    \vfill\raggedright %
  \else\ifosd
    \parbox[t]{9.5cm}{\osdref\\\osdurl/\\
      SRef-ID: 1812-0822/osd/\@pyear-\@pvol-\@fpage\\ %
      % DOI: \osddoi\\ % activate when doi number has been assigned
      European Geosciences Union} %
    \hfill\@osdlogo\\[8mm]
    \scalebox{1.0}[1.0]{Papers published in {\slshape Ocean Science Discussions}
      are under}\\
    \scalebox{1.0}[1.0]{open-access review for the journal {\slshape Ocean Science}}
    \vfill\raggedright %
  \else\ifhessd
    \parbox[t]{9.5cm}{\hessdref\\\hessdurl/\\
      SRef-ID: 1812-2116/hessd/\@pyear-\@pvol-\@fpage\\ %
      % DOI: \hessddoi\\ % activate when doi number has been assigned
      European Geosciences Union} %
    \hfill\@hessdlogo\\[8mm]
    \scalebox{1.0}[1.0]{Papers published in {\slshape Hydrology and Earth System Sciences Discussions} are under}\\
    \scalebox{1.0}[1.0]{open-access review for the journal {\slshape Hydrology and Earth System Sciences}}
    \vfill\raggedright %
  \else\ifcpd
    \parbox[t]{9.5cm}{\cpdref\\\cpdurl/\\
      SRef-ID: 1814-9359/cpd/\@pyear-\@pvol-\@fpage\\ %
      % DOI: \osddoi\\ % activate when doi number has been assigned
      European Geosciences Union} %
    \hfill\@cpdlogo\\[8mm]
    \scalebox{0.905}[1.0]{{\slshape Climate of the Past Discussions} is the access
      reviewed discussion forum of {\slshape Climate of the Past}}
    \vfill\raggedright %
  \else
    \parbox[t]{10cm}{\acpdref\\\acpdurl/\\
      SRef-ID: 1680-7375/acpd/\@pyear-\@pvol-\@fpage\\ %
      % DOI: \acpddoi\\ % activate when doi number has been assigned
      European Geosciences Union} %
    \hfill\@acpdlogo\vfill\raggedright %
  \fi\fi\fi\fi
  {\huge\bfseries\@title\\[5mm]}%
  \renewcommand{\Authfont}{\if@hvmath\rm\else\sf\fi\bfseries}%
  \renewcommand{\Affilfont}{\@affilfontsize\if@hvmath\rm\else\sf\fi\mdseries}%
  \setlength{\affilsep}{3mm}%
  \@author\\[3mm] %
  {\small Received: \@recvd\ -- Accepted: \@accptd\ -- Published: \@published\\[2mm]
  Correspondence to: \@corresp}\\[2mm]
  \copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\\\vfill %
  \if@egs@home\end{nolinenumbers}\fi %
\fi % acpd/bgd title page ends here
\if@egsbook\if@draft\else
    \begin{minipage}[b][5cm][s]{\hsize}
    \null\vfil
    \begin{minipage}{\hsize}
    \raggedright
 \let\footnotesize\normalsize
 \let\footnoterule\relax
 \def\thefootnote{\fnsymbol{footnote}}%
   \def\@mpfn{footnote}%
   \let\thempfn=\thefootnote
   \renewcommand\@makefntext[1]{%
      \parindent 1em \noindent
      \@makefnmark##1}
   {\LARGE\bfseries \@title\par}
   \vspace{1cm}
   {\large
     \renewcommand{\Authfont}{\normalfont}%
     \renewcommand{\Affilfont}{\normalsize\@affilfontsize\normalfont}%
     \setlength{\affilsep}{4pt}%
     \baselineskip 15pt
     \let\and=\sw@and
     \begin{minipage}[t]{\hsize}\flushleft\baselineskip=12pt%
        \@author
     \end{minipage}\par}%
     \@thanks
   \end{minipage}%
   \end{minipage}%
\fi\fi
\fi
   \renewcommand\@makefntext[1]{\parindent 1em \noindent ##1}%
 \let\footnotesize\normalsize
\if@egsbook\else\ifacpd\else
  \if!\@corresp!\else
\footnotetext{\textit{Correspondence to:} \@corresp}%
\fi\fi\fi
\endgroup
\setcounter{footnote}{0}%
\let\thanks\relax
\gdef\@thanks{}\gdef\@author{}\gdef\@title{}\let\maketitle\relax
\if@draft\gdef\baselinestretch{1.4}\reset@font\large\normalsize\fi
} % end of \def\maketitle
\renewcommand\and{\@centercr}
\newcommand\sw@and{\end{minipage}\\[1em]\begin{minipage}[t]{\hsize}%
  \flushleft\baselineskip12pt}
\def\journal#1{\def\@journal{#1}}
\let\booktitle=\journal
   \def\@journal{???}
\if@khb\booktitle{KHB}\fi
\def\msnumber#1{\def\@msnumber{#1}} \def\@msnumber{???}
\def\firstauthor#1{\def\@firstauthor{#1}} \def\@firstauthor{???}
\def\proofs#1{\def\@proofs{#1}} \def\@proofs{}
\def\offsets#1{\expandafter\split@off#1\\@@ \def\@offsets{#1}}
\def\@offsets{}
\def\split@off#1\\#2@@{\def\@corresp{#1}}
\def\correspondence#1{\def\@corresp{#1} \def\offsets##1{\def\@offsets{##1}}}
  \def\@corresp{}
%% some of the following commands are only active if \@egs@hometrue
\def\pubdiscuss#1{\if@egs@home\def\@pubdiscuss{#1}\fi} \def\@pubdiscuss{}
\def\received#1{\if@egs@home\def\@recvd{#1}\fi}        \def\@recvd{}
\def\revised#1{\if@egs@home\def\@revsd{#1}\fi}         \def\@revsd{}
\def\accepted#1{\if@egs@home\def\@accptd{#1}\fi}       \def\@accptd{}
\def\published#1{\if@egs@home\def\@published{#1}\fi}   \def\@published{}
\def\titleheight#1{\def\@tithei{#1}}                   \def\@tithei{7.5cm}
\def\affilfontsize#1{\def\@affilfontsize{#1}}          \def\@affilfontsize{}
\ifacpd\def\@affilfontsize{\small}\fi
\def\pubyear#1{\if@egs@home\def\@pyear{#1}\fi}         \def\@pyear{\number\year}
\def\pubvol#1{\if@egs@home\def\@pvol{#1}\fi}           \def\@pvol{0000}
\def\pubnum#1{\if@egs@home\def\@pnum{#1}\fi}           \def\@pnum{0000}
\def\firstpage#1{\if@egs@home\def\@fpage{#1}\fi}       \def\@fpage{0001}
\def\specialissue#1#2{\def\@speciss{#1}\def\@specissurl{#2}}  \def\@speciss{}\def\@specissurl{}
\if@egs@home
  \newcounter{jnlvolume} % the journal volume
  \setcounter{jnlvolume}{\number\year}
  \ifbg \addtocounter{jnlvolume}{-2003}
  \else\ifbgd \addtocounter{jnlvolume}{-2003}
  \else\ifos \addtocounter{jnlvolume}{-2003}
  \else\ifosd \addtocounter{jnlvolume}{-2003}
  \else\ifhess \addtocounter{jnlvolume}{-1996}
  \else\ifhessd \addtocounter{jnlvolume}{-2003}
  \else\ifcp \addtocounter{jnlvolume}{-2004}
  \else\ifcpd \addtocounter{jnlvolume}{-2004}
  \else \addtocounter{jnlvolume}{-2000} % for acp and acpd: volume = year - 2000
  \fi\fi\fi\fi\fi\fi\fi\fi
  \iftwostagejnl\def\@pvol{\arabic{jnlvolume}}\fi  % set volume for acp/acpd and bg/bgd
  \def\@published{\number\day~\ifcase\month\or January\or February\or%
  March\or April\or May\or June\or July\or August\or September\or%
  October\or November\or December\fi~\number\year}
\fi
\def\runningauthor#1{\def\@runauth{#1}%
\ifonline\hypersetup{pdfauthor={\@runauth}}\fi} \def\@runauth{}
\def\runningtitle#1{\def\@runtit{#1}%
\ifonline\hypersetup{pdftitle={\@runtit}}\fi}   \def\@runtit{}
\def\runninghead#1{\def\@runhd{#1}}             \def\@runhd{\@runauth: \@runtit}
\def\lastpage@putlabel{\addtocounter{page}{-1}%
   \immediate\write\@auxout{\string
   \newlabel{LastPage}{{}{\thepage}{}{}{}}}%
   \addtocounter{page}{1}}
%% rs20030722: activate next line only if figcaps is not included:
%% \AtEndDocument{\clearpage\lastpage@putlabel}
\def\@lpage{\ifacpd\pageref*{LastPage}\else\pageref{LastPage}\fi}
\def\abslang{}
\def\keywords#1{%
\iftwostagejnl\ClassWarningNoLine{egu}{Keywords are not supported in acp/bg}
\else\vspace{1.7mm}\par\noindent{\bfseries Keywords.}\enspace\ignorespaces#1
\fi}
\renewenvironment{abstract}{%
  \iftwostagejnl\gdef\abstractexists{true}\fi
  \if@khb \abslang\fi
  \if@draft
    \if@egsbook
      \if@khb
        \reset@font\normalsize
        \begin{center}%
          {\bfseries \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \begin{quotation}%
      \else
        \section{\abstractname}
      \fi
    \else
      \reset@font\normalsize
      \begin{center}%
       {\bfseries \abstractname\vspace{-.5em}\vspace{\z@}}%
      \end{center}%
      \begin{quotation}\noindent%
    \fi
  \else
    \if@egsbook
      \if@khb
        \paragraph*{\abstractname.}
      \else
        \section{\abstractname}
      \fi
    \else
      \ifacpd\newpage\section*{\abstractname}\else
      \reset@font\normalsize\par\noindent
        {\bfseries \abstractname.}
      \ignorespaces\fi
    \fi
  \fi
  \ifonline\hypertarget{abstr}{}\fi}{%
  \if@khb
    \iflanguage{german}{\gdef\abslang{\selectlanguage{english}}}
                       {\gdef\abslang{\selectlanguage{german}}}
  \fi
  \if@draft
    \if@egsbook\if@khb
      \end{quotation}\newpage\fi
    \else
      \end{quotation}\newpage
    \fi
  \else
    \if@egsbook\else\ifacpd\else\if@noline\else
    \newline\null\par\hrule
    \fi\fi\fi
  \fi}

\newcommand{\introduction}[1][Introduction]%
  {\section{#1}\ifonline\hypertarget{intro}{}\fi%
  \def\introductionexists{true}}
\newcommand{\conclusions}[1][Conclusions]%
  {\section{#1}\ifonline\hypertarget{conclusions}{}\fi%
  \def\conclusionsexists{true}}
\newenvironment{acknowledgements}{\begin{description}\item[\acknowsname]}%
    {\end{description}}
\newenvironment{acknowledgement}{\begin{description}\item[\acknowname]}%
    {\end{description}}
\def\ps@cameraready{\let\@mkboth\@gobbletwo
    \def\@oddhead{\reset@font\normalsize
    \begin{minipage}[t]{\textwidth}%
      \begin{minipage}[b]{10cm}%
        \baselineskip=6mm \lineskip=0pt \lineskiplimit=0pt
        \if@egsbook Book%
        \else Journal\fi: \@journal\\
        MS No.: \@msnumber\\
        First author: \@firstauthor
      \end{minipage}\hspace{\fill}\thepage
    \end{minipage}}
  \fi
  \let\@evenhead=\@oddhead
  \def\@oddfoot{}\def\@evenfoot{}}
\ifacp
\def\ps@runninghead{\let\@mkboth\@gobbletwo
  \def\@oddhead{\@runhd\hfil\makebox[\z@][r]{\thepage}}
  \def\@evenhead{\makebox[\z@][l]{\thepage}\hfil\@runhd}
  \def\@oddfoot{\ifbg\bgurl\if@nosref\else/\fi\hfil\bgref\else\ifos\osurl\if@nosref\else/\fi\hfil\osref\else\ifhess\hessurl\if@nosref\else/\fi\hfil\hessref\else\ifcp\cpurl\if@nosref\else/\fi\hfil\cpref\else\acpurl\if@nosref\else/\fi\hfil\acpref\fi\fi\fi\fi}
  \def\@evenfoot{\ifbg\bgref\hfil\bgurl\if@nosref\else/\fi\else\ifos\osref\hfil\osurl\if@nosref\else/\fi\else\ifhess\hessref\hfil\hessurl\if@nosref\else/\fi\else\ifcp\cpref\hfil\cpurl\if@nosref\else/\fi\else\acpref\hfil\acpurl\if@nosref\else/\fi\fi\fi\fi\fi}}
\else\ifacpd
\def\ps@runninghead{
  \def\@oddhead{}
  \def\@evenhead{}
  \def\@oddfoot{\hfil\thepage\hfil}
  \def\@evenfoot{\hfil\thepage\hfil}}
\else
\def\ps@runninghead{\let\@mkboth\@gobbletwo
  \def\@oddhead{\@runhd\hfil\makebox[\z@][r]{\thepage}}
  \def\@evenhead{\makebox[\z@][l]{\thepage}\hfil\@runhd}
  \def\@oddfoot{}\def\@evenfoot{}}
\fi\fi
\if@aglet
\def\ps@runninghead{\let\@mkboth\@gobbletwo
  \def\@oddhead{\@runhd\hfil\makebox[\z@][r]{\thepage}
  {\setlength{\unitlength}{1.0cm}
  \begin{picture}(0,0)
  \color[gray]{0.85}\put(0.8,0){\rule[-300mm]{14mm}{400mm}}
  \end{picture}
  }}
  \def\@evenhead{\makebox[\z@][l]{\thepage}\hfil\@runhd
  {\setlength{\unitlength}{1.0cm}
  \begin{picture}(0,0)
  \color[gray]{0.85}\put(-19.7,0){\rule[-300mm]{14mm}{400mm}}
  \end{picture}
  }}
  \def\@oddfoot{}\def\@evenfoot{}}
\fi
\def\ps@default{\ifacpd\def\@oddhead{}\else\let\@mkboth\@gobbletwo
  \def\@oddhead{\textbf{\large\@journal} (\@pyear)
    \@pvol:\@fpage\if@nolastpage\else{--\@lpage}\fi\hfill}\fi
  \def\@oddfoot{\hfil\thepage\hfil}}

\def\@logo{\parbox[t][\z@][t]{11em}
  {\vspace{-.7\baselineskip}
  \rule{\linewidth}{1\p@}\\\fontfamily{\sfdefault}\fontseries{\seriesdefault}\fontsize{46}{3}\selectfont
  \scalebox{0.85}[1.0]{LOGO}\par
  \rule{\linewidth}{1\p@}}}
\def\ps@acp{\let\@mkboth\@gobbletwo % only for 1st page
    \if@egs@home % real header
      \def\@oddhead{\parbox[t]{18pc}{\acpref\\\acpurl%
      \if@nosref\else/\\SRef-ID: 1680-7324/acp/\@pyear-\@pvol-\@fpage\fi\\European Geosciences Union} % for now without doi
      %\def\@oddhead{\parbox[t]{18pc}{\acpref\\\acpurl/\\DOI: \acpdoi}
      \hfil\scalebox{1.25}{\@acplogo\\}}
    \else % slugcomment
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         This manuscript was prepared for Atmos.~Chem.~Phys. (Discuss.)\\
         with version \clsversion\ of the egu \LaTeX\ class egu.cls.\\
         Date: \today}
      \hfil\parbox[t][4cm][t]{0.35\textwidth}{\vspace{-.7\baselineskip}%
        \rule{\linewidth}{1\p@}\\[3mm]
        \centering{\huge ACP-LOGO}\\ % \@logo not used since it needs graphicx
        \rule{\linewidth}{1\p@}}}
    \fi
  \def\@oddfoot{\copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\hfil}}
\def\ps@bg{\let\@mkboth\@gobbletwo % only for 1st page
    \if@egs@home % real header
      \def\@oddhead{\parbox[t]{18pc}{\bgref\\\bgurl%
      \if@nosref\else/\\SRef-ID: 1726-4189/bg/\@pyear-\@pvol-\@fpage\fi\\European Geosciences Union} % for now without doi
      %\def\@oddhead{\parbox[t]{18pc}{\bgref\\\bgurl/\\DOI: \bgdoi}
      \hfil\scalebox{1.25}{\@bglogo\\}}
    \else % slugcomment
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         This manuscript was prepared for Biogeosciences (Discussions)\\
         with version \clsversion\ of the egu \LaTeX\ class egu.cls.\\
         Date: \today}
      \hfil\parbox[t][4cm][t]{0.35\textwidth}{\vspace{-.7\baselineskip}%
        \rule{\linewidth}{1\p@}\\[3mm]
        \centering{\huge BG-LOGO}\\ % \@logo not used since it needs graphicx
        \rule{\linewidth}{1\p@}}}
    \fi
  \def\@oddfoot{\copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\hfil}}
\def\ps@os{\let\@mkboth\@gobbletwo % only for 1st page
    \if@egs@home % real header
      \def\@oddhead{\parbox[t]{18pc}{\osref\\\osurl%
      \if@nosref\else/\\SRef-ID: 1812-0792/os/\@pyear-\@pvol-\@fpage\fi\\European Geosciences Union} % for now without doi
      %\def\@oddhead{\parbox[t]{18pc}{\osref\\\osurl/\\DOI: \osdoi}
      \hfil\scalebox{1.25}{\@oslogo\\}}
    \else % slugcomment
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         This manuscript was prepared for Ocean Science (Discussions)\\
         with version \clsversion\ of the egu \LaTeX\ class egu.cls.\\
         Date: \today}
      \hfil\parbox[t][4cm][t]{0.35\textwidth}{\vspace{-.7\baselineskip}%
        \rule{\linewidth}{1\p@}\\[3mm]
        \centering{\huge OS-LOGO}\\ % \@logo not used since it needs graphicx
        \rule{\linewidth}{1\p@}}}
    \fi
  \def\@oddfoot{\copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\hfil}}
\def\ps@hess{\let\@mkboth\@gobbletwo % only for 1st page
    \if@egs@home % real header
      \def\@oddhead{\parbox[t]{18pc}{\hessref\\\hessurl%
      \if@nosref\else/\\SRef-ID: 1607-7938/hess/\@pyear-\@pvol-\@fpage\fi\\European Geosciences Union} % for now without doi
      %\def\@oddhead{\parbox[t]{18pc}{\hessref\\\hessurl/\\DOI: \hessdoi}
      \hfil\scalebox{1.25}{\@hesslogo\\}}
    \else % slugcomment
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         This manuscript was prepared for Hydrology and Earth System Sciences (Discussions)\\
         with version \clsversion\ of the egu \LaTeX\ class egu.cls.\\
         Date: \today}
      \hfil\parbox[t][4cm][t]{0.35\textwidth}{\vspace{-.7\baselineskip}%
        \rule{\linewidth}{1\p@}\\[3mm]
        \centering{\huge HESS-LOGO}\\ % \@logo not used since it needs graphicx
        \rule{\linewidth}{1\p@}}}
    \fi
  \def\@oddfoot{\copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\hfil}}
\def\ps@cp{\let\@mkboth\@gobbletwo % only for 1st page
    \if@egs@home % real header
      \def\@oddhead{\parbox[t]{18pc}{\cpref\\\cpurl%
      \if@nosref\else/\\SRef-ID: 1814-9332/cp/\@pyear-\@pvol-\@fpage\fi\\European Geosciences Union} % for now without doi
      %\def\@oddhead{\parbox[t]{18pc}{\cpref\\\cpurl/\\DOI: \cpdoi}
      \hfil\scalebox{1.25}{\@cplogo\\}}
    \else % slugcomment
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         This manuscript was prepared for Climate of the Past (Discussions)\\
         with version \clsversion\ of the egu \LaTeX\ class egu.cls.\\
         Date: \today}
      \hfil\parbox[t][4cm][t]{0.35\textwidth}{\vspace{-.7\baselineskip}%
        \rule{\linewidth}{1\p@}\\[3mm]
        \centering{\huge CP-LOGO}\\ % \@logo not used since it needs graphicx
        \rule{\linewidth}{1\p@}}}
    \fi
  \def\@oddfoot{\copyright\ \@pyear\ Author(s). This work is licensed under a Creative Commons License.\hfil}}
\if@egs@home
\def\@acplogo{%
   \parbox[t][\z@][t]{42.0mm}{\vskip-3.5mm\includegraphics[width=15.5mm]{egu.pdf}\\[-19.3mm]
   \null\hspace{0.5mm}\rule{3.5mm}{1pt}\hspace{8.3mm}\rule{29.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Atmospheric}\\
   \null\hfill \scalebox{1.}[1.0]{Chemistry}\\
   \null\hfill \scalebox{1.}[1.0]{and Physics}}\\[-2.7mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{32.0mm}{1pt}}}
\def\@acpdlogo{%
   \parbox[t][0pt][t]{43.0mm}{\vskip-3.5mm\includegraphics[width=17.8mm]{egu.pdf}\\[-22.7mm]
   \null\hspace{0.5mm}\rule{5.5mm}{1pt}\hspace{8.3mm}\rule{28.7mm}{1pt}\\[-1.2mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[.9]{Atmospheric}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{Chemistry}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{and Physics}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{Discussions}}\\[-3.3mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{33.0mm}{1pt}}}
\def\@bglogo{%
   \parbox[t][\z@][t]{44.0mm}{\vskip-3.5mm\includegraphics[width=10.5mm]{egu.pdf}\\[-14.8mm]
   \null\hspace{0.5mm}\rule{3.5mm}{1pt}\hspace{6.3mm}\rule{33.7mm}{1pt}\\[1.9mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Biogeosciences}}\\[-0.4mm]
   \null\hspace{0.5mm}\rule{1.0mm}{1pt}\hspace{6.5mm}\rule{36.0mm}{1pt}}}
\def\@bgdlogo{%
   \parbox[t][\z@][t]{46.0mm}{\vskip-3.5mm\includegraphics[width=12.2mm]{egu.pdf}\\[-16.7mm]
   \null\hspace{0.5mm}\rule{2.5mm}{1pt}\hspace{7.3mm}\rule{35.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Biogeosciences}\\
   \null\hfill \scalebox{1.}[1.0]{Discussions}}\\[-2.9mm]
   \null\hspace{0.5mm}\rule{1.5mm}{1pt}\hspace{6.0mm}\rule{38.0mm}{1pt}}}
\def\@oslogo{%
   \parbox[t][\z@][t]{42.0mm}{\vskip-3.5mm\includegraphics[width=10.5mm]{egu.pdf}\\[-14.8mm]
   \null\hspace{0.5mm}\rule{3.5mm}{1pt}\hspace{6.3mm}\rule{31.7mm}{1pt}\\[1.9mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Ocean Science}}\\[-0.4mm]
   \null\hspace{0.5mm}\rule{1.0mm}{1pt}\hspace{6.5mm}\rule{34.0mm}{1pt}}}
\def\@osdlogo{%
   \parbox[t][\z@][t]{44.0mm}{\vskip-3.5mm\includegraphics[width=12.2mm]{egu.pdf}\\[-16.7mm]
   \null\hspace{0.5mm}\rule{2.5mm}{1pt}\hspace{7.3mm}\rule{33.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Ocean Science}\\
   \null\hfill \scalebox{1.}[1.0]{Discussions}}\\[-2.9mm]
   \null\hspace{0.5mm}\rule{1.5mm}{1pt}\hspace{6.0mm}\rule{36.0mm}{1pt}}}
\def\@hesslogo{%
   \parbox[t][\z@][t]{45.0mm}{\vskip-3.5mm\includegraphics[width=15.5mm]{egu.pdf}\\[-19.3mm]
   \null\hspace{0.5mm}\rule{3.5mm}{1pt}\hspace{8.3mm}\rule{32.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Hydrology and}\\
   \null\hfill \scalebox{1.}[1.0]{Earth System}\\
   \null\hfill \scalebox{1.}[1.0]{Sciences}}\\[-2.7mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{35.0mm}{1pt}}}
\def\@hessdlogo{%
   \parbox[t][0pt][t]{46.0mm}{\vskip-3.5mm\includegraphics[width=17.8mm]{egu.pdf}\\[-22.7mm]
   \null\hspace{0.5mm}\rule{5.5mm}{1pt}\hspace{8.3mm}\rule{31.7mm}{1pt}\\[-1.2mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[.9]{Hydrology and}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{Earth System}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{Sciences}\\[-0.8mm]
   \null\hfill \scalebox{1.}[.9]{Discussions}}\\[-3.3mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{36.0mm}{1pt}}}
\def\@cplogo{%
   \parbox[t][\z@][t]{34.0mm}{\vskip-3.5mm\includegraphics[width=12.2mm]{egu.pdf}\\[-15.7mm]
   \null\hspace{0.5mm}\rule{2.5mm}{1pt}\hspace{7.3mm}\rule{23.7mm}{1pt}\\[0.1mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Climate}\\
   \null\hfill \scalebox{1.}[1.0]{of the Past}}\\[-2.4mm]
   \null\hspace{0.5mm}\rule{1.5mm}{1pt}\hspace{6.0mm}\rule{26.0mm}{1pt}}}
\def\@cpdlogo{%
   \parbox[t][\z@][t]{42.0mm}{\vskip-3.5mm\includegraphics[width=17.5mm]{egu.pdf}\\[-21.6mm]
   \null\hspace{0.5mm}\rule{4.0mm}{1pt}\hspace{9.8mm}\rule{27.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Climate}\\[-0.5mm]
   \null\hfill \scalebox{1.}[1.0]{of the Past}\\[-0.5mm]
   \null\hfill \scalebox{1.}[1.0]{Discussions}}\\[-2.9mm]
   \null\hspace{0.5mm}\rule{3.0mm}{1pt}\hspace{8.5mm}\rule{30.0mm}{1pt}}}
\else
\let\@acplogo\@logo
\let\@acpdlogo\@logo
\let\@bglogo\@logo
\let\@bgdlogo\@logo
\let\@oslogo\@logo
\let\@osdlogo\@logo
\let\@hesslogo\@logo
\let\@hessdlogo\@logo
\let\@cplogo\@logo
\let\@cpdlogo\@logo
\fi
\if@egs@home
\def\ps@npg{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Nonlinear Processes in Geophysics%
    \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
    SRef-ID: 1607-7946/npg/\@pyear-\@pvol-\@fpage\fi
    \\European Geosciences Union\\
    \copyright\ \@pyear\ Author(s). This work is licensed\\under a Creative Commons License.}
    \hfil\scalebox{1.25}{\@npglogo}}
  \def\@oddfoot{}}
\def\@npglogo{%
   \parbox[t][\z@][t]{54.5mm}{\vskip-3.5mm\includegraphics[width=13.0mm]{egu.pdf}\\[-16.8mm]
   \null\hspace{0.5mm}\rule{3.0mm}{1pt}\hspace{8.3mm}\rule{42.7mm}{1pt}\\[0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{13}{13}\selectfont
   \null\hfill \scalebox{0.9}[1.0]{Nonlinear Processes}\\[0.7mm]
   \null\hfill \scalebox{1.}[1.0]{in Geophysics}}\\[-2.3mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{44.5mm}{1pt}}}
\else
\def\ps@npg{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Nonlinear Processes in Geophysics%
    \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
    SRef-ID: 1607-7946/npg/\@pyear-\@pvol-\@fpage\fi
    \\European Geosciences Union\\
    \copyright\ \@pyear\ Author(s). This work is licensed\\under a Creative Commons License.}
    \hfil\scalebox{1.25}{\@npglogo}}
  \def\@oddfoot{}}
  \let\@npglogo\@logo
\fi
\def\ps@ars{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Advances in Radio Science%
    \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
    SRef-ID: 1684-9973/ars/\@pyear-\@pvol-\@fpage\fi
    \\\copyright\ Copernicus GmbH \@pyear}
    \hfil\scalebox{1.0}[1.2]{\@arslogo}}
  \def\@oddfoot{}}
\if@egs@home
\def\@arslogo{\parbox[t][\z@][t]{9.2pc}
  {\vspace{-.7\baselineskip}%
  \rule{\linewidth}{1\p@}\\
  \fontencoding{T1}\fontfamily{ma1}\fontseries{m}\fontshape{n}\fontsize{17.28}{19}\selectfont
  Advances in\\Radio Science\\
  \vspace{-10.5mm}
  \rule{\linewidth}{1\p@}}}
\else
\let\@arslogo\@logo
\fi
\def\ps@sg{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Social Geography\if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\fi\\
    www.social-geography.net\if@nosref\else/\@pvol/\@fpage\\
    SRef-ID: 1729-4312/sg/\@pyear-\@pvol-\@fpage\fi
    \\\copyright\ Copernicus GmbH \@pyear}
    \hfil\scalebox{1.0}[1.2]{\@sglogo}}
  \def\@oddfoot{}}
\if@egs@home
\def\@sglogo{\parbox[t][\z@][t]{11.5pc}
  {\vspace{-.7\baselineskip}%
  \rule{\linewidth}{1\p@}\\
  \fontencoding{T1}\fontfamily{ma1}\fontseries{m}\fontshape{n}\fontsize{17.28}{19}\selectfont
  Social \hfil Geography\\[-3.5mm]
  \rule{\linewidth}{1\p@}}}
\else
\let\@sglogo\@logo
\fi
\def\ps@adgeo{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Advances in Geosciences%
  \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
  SRef-ID: 1680-7359/adgeo/\@pyear-\@pvol-\@fpage\fi
  \\European Geosciences Union\\
  \copyright\ \@pyear\ Author(s). This work is licensed\\under a Creative Commons License.}
  \hfil\scalebox{1.25}{\@adgeologo}}
  \def\@oddfoot{}}
\if@egs@home
\def\@adgeologo{%
   \parbox[t][\z@][t]{42.0mm}{\vskip-3.5mm\includegraphics[width=13.0mm]{egu.pdf}\\[-16.8mm]
   \null\hspace{0.5mm}\rule{3.0mm}{1pt}\hspace{8.3mm}\rule{30.2mm}{1pt}\\[0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{13}{13}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Advances in}\\[0.7mm]
   \null\hfill \scalebox{1.}[1.0]{Geosciences}}\\[-2.3mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{32.0mm}{1pt}}}
\else
\let\@adgeologo\@logo
\fi
\def\ps@ag{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Annales Geophysicae%
  \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
  SRef-ID: 1432-0576/ag/\@pyear-\@pvol-\@fpage\fi
  \\\copyright\ European Geosciences Union\ \@pyear}
  \hfill\scalebox{1.25}{\@aglogo}}
  \if@aglet
    \def\@oddhead{\parbox[t]{25pc}{Annales Geophysicae%
    \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
    SRef-ID: 1432-0576/ag/\@pyear-\@pvol-\@fpage\fi
    \\\copyright\ European Geosciences Union\ \@pyear}
    \hfill\scalebox{1.25}{\@aglogo}
    {\setlength{\unitlength}{1.0cm}
    \begin{picture}(0,0)
    \color[gray]{0.85}\put(0.7,0){\rule[-300mm]{14mm}{400mm}}
    \end{picture}}}
  \fi
  \def\@oddfoot{}}
\if@egs@home
\def\@aglogo{
   \parbox[t][\z@][t]{30.0mm}{\vskip-3.5mm\includegraphics[width=13mm]{egu.pdf}\\[-16.5mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.8mm}\rule{19.7mm}{1pt}\\[0.2mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{13}{13}\selectfont
   \null\hfill \scalebox{0.6}[1.0]{Annales} \\ \null\hfill \scalebox{0.6}[1.0]{Geophysicae}}\\[-2.3mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{20.0mm}{1pt}}}
\else
\let\@aglogo\@logo
\fi
\def\ps@nhess{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{Natural Hazards and Earth System Sciences%
  \if@nosref\else,\ \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
  SRef-ID: 1684-9981/nhess/\@pyear-\@pvol-\@fpage\fi
  \\European Geosciences Union\\
  \copyright\ \@pyear\ Author(s). This work is licensed\\under a Creative Commons License.} \hfill
  \scalebox{1.25}{\@nhesslogo}}
  \def\@oddfoot{}}
\if@egs@home
\def\@nhesslogo{%
   \parbox[t][\z@][t]{50.0mm}{\vskip-3.5mm\includegraphics[width=15.5mm]{egu.pdf}\\[-19.3mm]
   \null\hspace{0.5mm}\rule{3.5mm}{1pt}\hspace{8.3mm}\rule{37.7mm}{1pt}\\[-0.4mm]
   {\fontencoding{T1}\fontfamily{ma1}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
   \null\hfill \scalebox{1.}[1.0]{Natural Hazards}\\
   \null\hfill \scalebox{1.}[1.0]{and Earth}\\
   \null\hfill \scalebox{1.}[1.0]{System Sciences}}\\[-2.7mm]
   \null\hspace{0.5mm}\rule{2.0mm}{1pt}\hspace{7.5mm}\rule{40.0mm}{1pt}}}
\else
\let\@nhesslogo\@logo
\fi
\def\ps@mueller{\let\@mkboth\@gobbletwo
  \def\@oddhead{\parbox[t]{25pc}{EGU Stephan Mueller Special Publication
     Series, \@pvol,\ \@fpage\if@nolastpage\else{--\@lpage}\fi,\ \@pyear\\
  \copyright\ European Geosciences Union\ \@pyear} \hfill \@muellerlogo}
  \def\@oddfoot{}}
\if@egs@home
\def\@muellerlogo{%
   \parbox[t][\z@][t]{18.5mm}{\vskip-3.5mm\includegraphics[width=18.5mm]{egu.pdf}}}
\else
\let\@muellerlogo\@logo
\fi
\if@twocolumn
  \flushbottom \else \raggedbottom
\fi
\def\iftwocol#1#2{\if@twocolumn#1\else #2\fi}

\pagenumbering{arabic}
\if@twocolumn \flushbottom \else \raggedbottom \fi
\if@twoside \flushbottom \fi

\if@draft\ps@plain
  \else\if@headers\@twosidetrue\ps@runninghead
  \else\ps@cameraready\fi
\fi
\newcommand\fncomma{\textsuperscript{,}}
\AtBeginDocument{\mathindent\z@}
\AtBeginDocument{\ifacpd\if@hvmath\else\fontfamily{\sfdefault}\fi\selectfont\fi}
\AtBeginDocument{\if@egs@home\else\def\balance{%
\ClassWarningNoLine{egu}{Please do not use \string\balance}}\fi}
%% Copyright 1997-1999 Patrick W Daly
%% Max-Planck-Institut f\"ur Aeronomie
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: daly@linmpi.mpg.de
\def\ModuleVersion#1[#2]{}
    \ModuleVersion{authblk}
          [1999/02/23 1.2 (PWD)]
%% authblk coding copyright (C) 1997 Patrick W. Daly
\newcommand\Authfont{\normalfont}
\newcommand\Affilfont{\normalfont}
\newcommand\Authsep{, }
\newcommand\Authands{, and }
\newcommand\Authand{ and }
\newlength{\affilsep}\setlength{\affilsep}{1em}
\newlength{\@affilsep}
\newcounter{Maxaffil}
\setcounter{Maxaffil}{2}
\newcounter{authors}
\newcounter{affil}
\newif\ifnewaffil \newaffiltrue
\newcommand\AB@authnote[1]{\textsuperscript{\normalfont#1}}
\newcommand\AB@affilnote[1]{\textsuperscript{\normalfont#1}}
\providecommand\textsuperscript[1]{$^{#1}$}
\newcommand\AB@blk@and{\protect\Authfont\protect\AB@setsep}
\newcommand\AB@pand{\protect\and \protect\Authfont \protect\AB@setsep}
\@namedef{@sep1}{}
\@namedef{@sep2}{\Authand}
\newcommand\AB@affilsep{\protect\Affilfont}
\newcommand\AB@affilsepx{\protect\\\protect\Affilfont}
\newcommand\AB@setsep{\setlength{\@affilsep}{\affilsep}}
\newcommand\AB@resetsep{\setlength{\@affilsep}{\z@}}
\newcommand\AB@authlist{}
\newcommand\AB@affillist{}
\newcommand\AB@authors{}
\newcommand\AB@empty{}
\xdef\AB@author{\noexpand\AB@blk@and\@author}
\renewcommand\author[2][]%
      {\ifnewaffil\addtocounter{affil}{1}%
       \edef\AB@thenote{\arabic{affil}}\fi
      \if\relax#1\relax\def\AB@note{\AB@thenote}\else\def\AB@note{#1}%
        \setcounter{Maxaffil}{0}\fi
      \ifnum\value{authors}>1\relax
      \@namedef{@sep\number\c@authors}{\Authsep}\fi
      \addtocounter{authors}{1}%
      \begingroup
          \let\protect\@unexpandable@protect \let\and\AB@pand
          \def\thanks{\protect\thanks}\def\footnote{\protect\footnote}%
         \@temptokena=\expandafter{\AB@authors}%
         {\def\\{\protect\\[\@affilsep]\protect\Affilfont
              \protect\AB@resetsep}%
              \xdef\AB@author{\AB@blk@and#2}%
       \ifnewaffil\gdef\AB@las{}\gdef\AB@lasx{\protect\Authand}\gdef\AB@as{}%
           \xdef\AB@authors{\the\@temptokena\AB@blk@and}%
       \else
          \xdef\AB@authors{\the\@temptokena\AB@as\AB@au@str}%
          \global\let\AB@las\AB@lasx\gdef\AB@lasx{\protect\Authands}%
          \gdef\AB@as{\Authsep}%
       \fi
       \gdef\AB@au@str{#2}}%
         \@temptokena=\expandafter{\AB@authlist}%
         \let\\=\authorcr
         \xdef\AB@authlist{\the\@temptokena
           \protect\@nameuse{@sep\number\c@authors}%
           \protect\Authfont#2\AB@authnote{\AB@note}}%
      \endgroup
      \ifnum\value{authors}>2\relax
      \@namedef{@sep\number\c@authors}{\Authands}\fi
      \newaffilfalse
}
\newcommand\authorcr{\protect\\ \protect\Authfont \protect\AB@setsep}%
\newcommand\affil[2][]%
   {\newaffiltrue\let\AB@blk@and\AB@pand
      \if\relax#1\relax\def\AB@note{\AB@thenote}\else\def\AB@note{#1}%
        \setcounter{Maxaffil}{0}\fi
      \begingroup
        \let\protect\@unexpandable@protect
        \def\thanks{\protect\thanks}\def\footnote{\protect\footnote}%
        \@temptokena=\expandafter{\AB@authors}%
        {\def\\{\protect\\\protect\Affilfont}\xdef\AB@temp{#2}}%
         \xdef\AB@authors{\the\@temptokena\AB@las\AB@au@str
         \protect\\[\affilsep]\protect\Affilfont\AB@temp}%
         \gdef\AB@las{}\gdef\AB@au@str{}%
        {\def\\{, \ignorespaces}\xdef\AB@temp{#2}}%
        \@temptokena=\expandafter{\AB@affillist}%
        \xdef\AB@affillist{\the\@temptokena \AB@affilsep
          \AB@affilnote{\AB@note}\protect\Affilfont\AB@temp}%
      \endgroup
       \let\AB@affilsep\AB@affilsepx
}
\def\@author{}
\renewcommand\@author{\ifx\AB@affillist\AB@empty\AB@author\else
      \ifnum\value{affil}>\value{Maxaffil}\def\rlap##1{##1}%
    \AB@authlist\\[\affilsep]\AB@affillist
    \else  \AB@authors\fi\fi}
%% Copyright 1993-1999 Patrick W Daly
%% Max-Planck-Institut f\"ur Aeronomie
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: daly@linmpi.mpg.de
\def\ModuleVersion#1[#2]{}
    \ModuleVersion{appendix}
           [1999/02/23 2.1 (PWD)]
\@ifundefined{iffigcaps}{}
  {\PackageWarningNoLine{appendix}
     {***********************************\MessageBreak
      This package read in after `figcaps'\MessageBreak
      There could be problems with sublabelled\MessageBreak
      figure and table numbers.\MessageBreak
      ***********************************}}
\providecommand{\appendixname}{Appendix}
\let\orisection=\section
\let\theolds=\thesection
\let\theoldss=\thesubsection
\def\appendix{\par
  \setcounter{section}{0}
  \setcounter{subsection}{0}
  \def\thesection{\Alph{section}}
  \def\thesubsection{\thesection\arabic{subsection}}
  \setcounter{secnumdepth}{4}
  \@addtoreset{equation}{section}
  \setcounter{equation}{0}
  \def\theequation{\thesection\arabic{equation}}
  \@addtoreset{figure}{section}
  \setcounter{figure}{0}
  \def\thefigure{\thesection\arabic{figure}}
  \@addtoreset{table}{section}
  \def\thetable{\thesection\arabic{table}}
  \setcounter{table}{0}
  \def\@Alph##1{\ifcase##1A\or A\or B\or C\or D\else\@Ialph{##1}\fi}
  \def\section{\@startsection {section}{1}{\z@}{-2em plus
    -1ex minus-.2ex}{1em plus.2ex}{\reset@font
       \if@egsbook\large\else\normalsize\fi
    \bfseries\noindent \appendixname~}}
}
\def\noappendix{\setcounter{secnumdepth}{0}
  \global\let\section=\orisection
  \global\let\thesection=\theolds
  \global\let\thesubsection=\theoldss
}
\iftwostagejnl\def\appendix{\ClassWarningNoLine{egs}%
  {\string\appendix\space is not supported in acp\MessageBreak
  see egs_man.pdf for details}}\fi
%% Copyright 1993-1999 Patrick W Daly
%% Max-Planck-Institut f\"ur Aeronomie
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: daly@linmpi.mpg.de
\def\ModuleVersion#1[#2]{}
    \ModuleVersion{figcaps}
        [1999/02/23 4.7 (PWD)]
\@ifundefined{chapter}{}{%
    \PackageError{figcaps}
     {`figcaps' may only be used with article-like classes}
     {There is no help. You just cannot use `figcaps'}}
\newif\iffigcaps \figcapstrue
\newif\iffigmark \figmarkfalse
\newif\if@figpage \@figpagefalse
\@ifundefined{figurecapname}{\def\figurecapname{Figure Captions}}{}
\@ifundefined{tablepagename}{\def\tablepagename{Tables}}{}
\@ifundefined{figurepagename}{\def\figurepagename{Figures}}{}
\begingroup
  \catcode `|=0 \catcode `[= 1
  \catcode`]=2 \catcode `\{=12 \catcode `\}=12
  \catcode`\^^M=13 \catcode`\\=12 %
|long|gdef|@vfig#1\end{figure}[|def|@term[\end{figure}]%
          |expandafter|writelof#1^^M\end{figure}^^M|end[figure]]%
|long|gdef|@vsfig#1\end{figure*}[|def|@term[\end{figure*}]%
          |expandafter|writelof#1^^M\end{figure*}^^M|end[figure*]]%
|long|gdef|@vtab#1\end{table}[|def|@term[\end{table}]%
          |expandafter|writelot#1^^M\end{table}^^M|end[table]]%
|long|gdef|@vstab#1\end{table*}[|def|@term[\end{table*}]%
          |expandafter|writelot#1^^M\end{table*}^^M|end[table*]]%
|long|gdef|@fcaponly#1\caption[|endgroup|@fcaption]%
|long|gdef|@feject#1\end{figure}[|end[figure]]%
  |endgroup
\long\def\FC@writefile#1#2{\@ifundefined{tf@#1}{}{%
  \immediate\write\csname tf@#1\endcsname{#2}}}
\def\lof{lof} \def\lot{lot}

{\catcode`\^^M=13%
  \long\gdef\writelof#1^^M{\def\@tempb{#1}%
      \ifx\@tempb\@term\let\@tempa\relax\else
      \if@filesw\FC@writefile{pof}{#1}\fi\let\@tempa\writelof\fi \@tempa}
  \long\gdef\writelot#1^^M{\def\@tempb{#1}%
      \ifx\@tempb\@term\let\@tempa\relax\else
      \if@filesw\FC@writefile{pot}{#1}\fi\let\@tempa\writelot\fi \@tempa}
}
\def\@fcaption{\refstepcounter\@captype \@ifnextchar[{\@@fcapopt}{\@@fcap}}
\def\@@fcapopt[#1]#2{\@caption\@captype[]{#2}\begingroup\set@literal\@feject}
\def\@@fcap#1{\@caption\@captype[]{#1}\begingroup\set@literal\@feject}
\def\figcapson{\figcapstrue}
\def\figcapsoff{\figcapsfalse}
\def\figmarkon{\figmarktrue}
\def\figmarkoff{\figmarkfalse}
\def\figure{\iffigcaps \let\@tempb=\@zfigure \else \let\@tempb=\@float\fi
  \@tempb{figure}}
\def\@zfigure#1{\begingroup\set@literal\@ifnextchar[{\@figure}{\@figure[]}}
\def\@figure[#1]{\@ifundefined{tf@pof}{\newwrite\tf@pof
     \if@filesw\immediate\openout\tf@pof\jobname.\lof\relax
     \ifFC@appendix\FC@writefile{pof}{\string\doappendix}\fi\fi}{}%
  \refstepcounter{figure}\iffigmark\marginpar{\fbox{\fnum@figure}}\fi
  \def\@captype{figure}\if@filesw\FC@writefile{pof}{\string\begin{figure}}\fi
  \@vfig}

\def\endfigure{\iffigcaps
    \endgroup\if@filesw\FC@writefile{pof}{\string\end{figure}}\fi\else
    \end@float\fi}

\@namedef{figure*}{\iffigcaps \let\@tempb=\@zsfigure \else
  \let\@tempb=\@dblfloat\fi \@tempb{figure}}
\def\@zsfigure#1{\begingroup\set@literal\@ifnextchar[{\@sfigure}{\@sfigure[]}}
\def\@sfigure[#1]{\@ifundefined{tf@pof}{\newwrite\tf@pof
     \if@filesw\immediate\openout\tf@pof\jobname.\lof\relax
     \ifFC@appendix\FC@writefile{pof}{\string\doappendix}\fi\fi}{}%
  \refstepcounter{figure}\iffigmark\marginpar{\fbox{\fnum@figure}}\fi
  \def\@captype{figure}\if@filesw\FC@writefile{pof}{\string\begin{figure}}\fi
  \@vsfig}

\@namedef{endfigure*}{\iffigcaps\endfigure\else\end@dblfloat\fi}
\def\table{\iffigcaps \let\@tempb=\@ztable \else \let\@tempb=\@float\fi
  \@tempb{table}}
\def\@ztable#1{\begingroup\set@literal\@ifnextchar[{\@table}{\@table[]}}
\def\@table[#1]{\@ifundefined{tf@pot}{\newwrite\tf@pot
     \if@filesw\immediate\openout\tf@pot\jobname.\lot\relax
     \ifFC@appendix\FC@writefile{pot}{\string\doappendix}\fi\fi}{}%
  \refstepcounter{table}\iffigmark\marginpar{\fbox{\fnum@table}}\fi
  \def\@captype{table}\if@filesw\FC@writefile{pot}{\string\begin{table}}\fi
  \@vtab}

\def\endtable{\iffigcaps
   \endgroup\if@filesw\FC@writefile{pot}{\string\end{table}}\fi\else
   \end@float\fi}

\@namedef{table*}{\iffigcaps \let\@tempb=\@zstable \else
  \let\@tempb=\@dblfloat\fi \@tempb{table}}
\def\@zstable#1{\begingroup\set@literal\@ifnextchar[{\@stable}{\@stable[]}}
\def\@stable[#1]{\@ifundefined{tf@pot}{\newwrite\tf@pot
     \if@filesw\immediate\openout\tf@pot\jobname.\lot\relax
     \ifFC@appendix\FC@writefile{pot}{\string\doappendix}\fi\fi}{}%
  \refstepcounter{table}\iffigmark\marginpar{\fbox{\fnum@table}}\fi
  \def\@captype{table}\if@filesw\FC@writefile{pot}
    {\string\begin{table}}%
    \fi\@vstab}

\@namedef{endtable*}{\iffigcaps\endtable\else
   \end@dblfloat\fi}
\def\tableofcontents{%
  \PackageWarningNoLine{figcaps}
   {Ignored: \protect\tableofcontents\space command}}
\def\listoffigures{%
  \PackageWarningNoLine{figcaps}
   {Ignored: \protect\listoffigures\space command}}
\def\listoftables{%
  \PackageWarningNoLine{figcaps}
   {Ignored: \protect\listoftables\space command}}
\def\addtocontents#1#2{\relax}
\def\addcontentsline#1#2#3{\relax}
{\catcode`\^^M=\active \gdef\cr@relax{\catcode`\^^M=\active \let^^M=\relax}}
\def\set@literal{\cr@relax \let\do\@makeother\dospecials}
\def\@figurecaps{\@ifundefined{tf@pof}{}{\if@filesw
  \immediate\closeout\tf@pof\fi
  \@ifundefined{appendixname}{}{\def\appendixname{}}
  \parindent\z@
  \reset@figtab\clearpage\section*{\figurecapname}\vspace{1cm}%
    \@mkboth{\uppercase{\figurecapname}}{\uppercase{\figurecapname}}%
    \addcontentsline{toc}{section}{\figurecapname}%
  \def\figure{\def\addcontentsline####1####2####3{}%
    \def\@captype{figure}%
    \begingroup\set@literal\@fcaponly
   }\def\endfigure{\endgroup\bigskip}%
  {\let\@makecaption=\@makefcaption
  \@input{\jobname.\lof}}%
  \clearpage}}%
\def\@tablepage{\@ifundefined{tf@pot}{}{\if@filesw
  \immediate\closeout\tf@pot\fi
  \@ifundefined{appendixname}{}{\def\appendixname{}}
  \parindent\z@
  \reset@figtab\clearpage\section*{\tablepagename}
    \@mkboth{\uppercase{\tablepagename}}{\uppercase{\tablepagename}}%
   \def\table{\def\@captype{table}\vbox\bgroup
    \small
   }\def\endtable{\egroup\bigskip}%
  \@input{\jobname.\lot}
  \clearpage}}
\def\printfigures{\@figpagetrue}
\def\@figurepage{\if@figpage\@ifundefined{tf@pof}{}{%
  \@ifundefined{appendixname}{}{\def\appendixname{}}
  \reset@figtab\clearpage\section*{\figurepagename}%
  \@mkboth{\uppercase{\figurepagename}}{\uppercase{\figurepagename}}%
  \begingroup
  \def\figure{\def\@captype{figure}\vbox\bgroup}\def\endfigure{\egroup
    \vspace{20pt}}%
  \def\label##1{}%
  \@input{\jobname.\lof}\newpage\endgroup}\fi}
\def\reset@figtab{%
  \setcounter{figure}{0}\setcounter{table}{0}
}
\AtEndDocument{\iffigcaps\IfFileExists{lineno.sty}{\nolinenumbers}{}\@tablepage\@figurecaps\@figurepage\fi}
\AtEndDocument{\clearpage\lastpage@putlabel}
\let\@appendix=\appendix
\newif\ifFC@appendix \FC@appendixfalse
\def\appendix{\FC@appendixtrue
  \@ifundefined{tf@pof}{}{\if@filesw\FC@writefile{pof}{\string\doappendix}\fi}%
  \@ifundefined{tf@pot}{}{\if@filesw\FC@writefile{pot}{\string\doappendix}\fi}%
  \global\let\@section=\section
  \doappendix
  \let\@@section=\section
  \def\section{%
    \@ifundefined{tf@pof}{}{\if@filesw\FC@writefile{pof}{\string
        \stepcounter{section}}\fi}%
    \@ifundefined{tf@pot}{}{\if@filesw\FC@writefile{pot}{\string
        \stepcounter{section}}\fi}%
    \@@section}
  \global\let\reset@figtab=\@unappendix}
\def\doappendix{%
  \global\let\@thefigure=\thefigure
  \global\let\@thetable=\thetable
  \global\let\@cl@section=\cl@section
  \@appendix
}
\def\@unappendix{\FC@appendixfalse
  \setcounter{figure}{0}\setcounter{table}{0}
  \let\thefigure=\@thefigure
  \let\thetable=\@thetable
  \let\cl@section=\@cl@section
  \let\section=\@section
}
\let\@makefcaption=\@makecaption
\if@draft\figcapstrue\else\figcapsfalse\fi
%% Copyright 1993-2000 Patrick W Daly
%% Max-Planck-Institut f\"ur Aeronomie
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: daly@linmpi.mpg.de
\def\ModuleVersion#1[#2]{}
    \ModuleVersion{natbib}
        [2000/07/24 7.0a (PWD)]
\def\NAT@sort{0}
\newcommand\NAT@open{(} \newcommand\NAT@close{)}
\newcommand\NAT@sep{;} \newcommand\NAT@cmt{, }
\newcommand\NAT@aysep{,} \newcommand\NAT@yrsep{,}
\def\NAT@nmfmt#1{{\NAT@up#1}}
\renewcommand\@cite%
    [3]{\ifNAT@swa\NAT@@open\if*#2*\else#2\ \fi
        #1\if*#3*\else\NAT@cmt#3\fi\NAT@@close\else#1\fi\endgroup}
\providecommand\@firstofone[1]{#1}
\let\citenumfont=\relax
\def\@citex%
  [#1][#2]#3{%
  \NAT@sort@cites{#3}%
  \let\@citea\@empty
  \@cite{\let\NAT@nm\@empty\let\NAT@year\@empty
    \@for\@citeb:=\NAT@cite@list\do
    {\edef\@citeb{\expandafter\@firstofone\@citeb}%
     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
     \@ifundefined{b@\@citeb\@extra@b@citeb}{\@citea%
       {\reset@font\bfseries ?}\NAT@citeundefined
                 \PackageWarning{natbib}%
       {Citation `\@citeb' on page \thepage \space undefined}\def\NAT@date{}}%
     {\let\NAT@last@nm=\NAT@nm\let\NAT@last@yr=\NAT@year
     \NAT@parse{\@citeb}%
     \ifNAT@full\let\NAT@nm\NAT@all@names\else
       \let\NAT@nm\NAT@name\fi
     \ifNAT@swa\ifcase\NAT@ctype
       \if\relax\NAT@date\relax
         \@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@nmfmt{\NAT@nm}\NAT@date\hyper@natlinkend
       \else
         \ifx\NAT@last@nm\NAT@nm\NAT@yrsep
            \ifx\NAT@last@yr\NAT@year
              \hyper@natlinkstart{\@citeb\@extra@b@citeb}\NAT@exlab
              \hyper@natlinkend
            \else\unskip\
              \hyper@natlinkstart{\@citeb\@extra@b@citeb}\NAT@date
              \hyper@natlinkend
            \fi
         \else\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
           \NAT@nmfmt{\NAT@nm}%
           \hyper@natlinkbreak{\NAT@aysep\ }{\@citeb\@extra@b@citeb}%
           \NAT@date\hyper@natlinkend
         \fi
       \fi
     \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@nmfmt{\NAT@nm}\hyper@natlinkend
     \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@date\hyper@natlinkend
     \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@alias\hyper@natlinkend
     \fi \def\@citea{\NAT@sep\ }%
     \else\ifcase\NAT@ctype
        \if\relax\NAT@date\relax
          \@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
          \NAT@nmfmt{\NAT@nm}\hyper@natlinkend
        \else
         \ifx\NAT@last@nm\NAT@nm\NAT@yrsep
            \ifx\NAT@last@yr\NAT@year
              \hyper@natlinkstart{\@citeb\@extra@b@citeb}\NAT@exlab
              \hyper@natlinkend
            \else\unskip\
              \hyper@natlinkstart{\@citeb\@extra@b@citeb}\NAT@date
              \hyper@natlinkend
            \fi
         \else\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
           \NAT@nmfmt{\NAT@nm}%
           \hyper@natlinkbreak{\ \NAT@@open\if*#1*\else#1\ \fi}%
              {\@citeb\@extra@b@citeb}%
           \NAT@date\hyper@natlinkend\fi
        \fi
       \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@nmfmt{\NAT@nm}\hyper@natlinkend
       \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@date\hyper@natlinkend
       \or\@citea\hyper@natlinkstart{\@citeb\@extra@b@citeb}%
         \NAT@alias\hyper@natlinkend
       \fi \if\relax\NAT@date\relax\def\@citea{\NAT@sep\ }%
           \else\def\@citea{\NAT@@close\NAT@sep\ }\fi
     \fi
     }}\ifNAT@swa\else\if*#2*\else\NAT@cmt#2\fi
     \if\relax\NAT@date\relax\else\NAT@@close\fi\fi}{#1}{#2}}
\newif\ifNAT@par \NAT@partrue
\newcommand\NAT@@open{\ifNAT@par\NAT@open\fi}
\newcommand\NAT@@close{\ifNAT@par\NAT@close\fi}
\newcommand\NAT@alias{\@ifundefined{al@\@citeb\@extra@b@citeb}{%
  {\reset@font\bfseries(alias?)}\PackageWarning{natbib}
  {Alias undefined for citation `\@citeb'
  \MessageBreak on page \thepage}}{\@nameuse{al@\@citeb\@extra@b@citeb}}}
\let\NAT@up\relax
\newcommand\NAT@Up[1]{{\let\protect\@unexpandable@protect\let~\relax
  \expandafter\NAT@deftemp#1}\expandafter\NAT@UP\NAT@temp}
\newcommand\NAT@deftemp[1]{\xdef\NAT@temp{#1}}
\newcommand\NAT@UP[1]{\let\@tempa\NAT@UP\ifcat a#1\MakeUppercase{#1}%
  \let\@tempa\relax\else#1\fi\@tempa}
\renewcommand\@biblabel[1]{\hfill}
\newlength{\bibhang}
\setlength{\bibhang}{1em}
\newlength{\bibsep}
\setlength{\bibsep}{\z@}

\newcommand\@bibsetup%
   [1]{\setlength{\leftmargin}{\bibhang}\setlength{\itemindent}{-\leftmargin}%
       \setlength{\itemsep}{\bibsep}\setlength{\parsep}{\z@}}
\AtBeginDocument{\ifx\SK@def\@undefined\else
\ifx\SK@cite\@empty\else
  \SK@def\@citex[#1][#2]#3{\SK@\SK@@ref{#3}\SK@@citex[#1][#2]{#3}}\fi
\ifx\SK@citeauthor\@undefined\def\HAR@checkdef{}\else
  \let\citeauthor\SK@citeauthor
  \let\citefullauthor\SK@citefullauthor
  \let\citeyear\SK@citeyear\fi
\fi}
\newif\ifNAT@full\NAT@fullfalse
\newif\ifNAT@swa
\DeclareRobustCommand\citet
   {\begingroup\NAT@swafalse\def\NAT@ctype{0}\NAT@partrue
     \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\newcommand\NAT@citetp{\@ifnextchar[{\NAT@@citetp}{\NAT@@citetp[]}}
\newcommand\NAT@@citetp{}
\def\NAT@@citetp[#1]{\@ifnextchar[{\@citex[#1]}{\@citex[][#1]}}
\DeclareRobustCommand\citep
   {\begingroup\NAT@swatrue\def\NAT@ctype{0}\NAT@partrue
         \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\cite
    {\begingroup\def\NAT@ctype{0}\NAT@partrue\NAT@swatrue
      \@ifstar{\NAT@fulltrue\NAT@cites}{\NAT@fullfalse\NAT@cites}}
\newcommand\NAT@cites{\@ifnextchar [{\NAT@@citetp}{%
     \NAT@swafalse
    \NAT@@citetp[]}}
\DeclareRobustCommand\citealt
   {\begingroup\NAT@swafalse\def\NAT@ctype{0}\NAT@parfalse
         \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\citealp
   {\begingroup\NAT@swatrue\def\NAT@ctype{0}\NAT@parfalse
         \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\citeauthor
   {\begingroup\NAT@swafalse\def\NAT@ctype{1}\NAT@parfalse
    \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\Citet
   {\begingroup\NAT@swafalse\def\NAT@ctype{0}\NAT@partrue
     \let\NAT@up\NAT@Up
     \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\Citep
   {\begingroup\NAT@swatrue\def\NAT@ctype{0}\NAT@partrue
     \let\NAT@up\NAT@Up
         \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\Citeauthor
   {\begingroup\NAT@swafalse\def\NAT@ctype{1}\NAT@parfalse
     \let\NAT@up\NAT@Up
    \@ifstar{\NAT@fulltrue\NAT@citetp}{\NAT@fullfalse\NAT@citetp}}
\DeclareRobustCommand\citeyear
   {\begingroup\NAT@swafalse\def\NAT@ctype{2}\NAT@parfalse\NAT@citetp}
\DeclareRobustCommand\citeyearpar
   {\begingroup\NAT@swatrue\def\NAT@ctype{2}\NAT@partrue\NAT@citetp}
\newcommand\citetext[1]{\NAT@open#1\NAT@close}
\DeclareRobustCommand\citefullauthor
   {\citeauthor*}
\newcommand\defcitealias[2]{%
   \@ifundefined{al@#1\@extra@b@citeb}{}
   {\PackageWarning{natbib}{Overwriting existing alias for citation #1}}
   \@namedef{al@#1\@extra@b@citeb}{#2}}
\DeclareRobustCommand\citetalias{\begingroup
   \NAT@swafalse\def\NAT@ctype{3}\NAT@parfalse\NAT@citetp}
\DeclareRobustCommand\citepalias{\begingroup
   \NAT@swatrue\def\NAT@ctype{3}\NAT@partrue\NAT@citetp}
\renewcommand\nocite[1]{\@bsphack
  \@for\@citeb:=#1\do{%
    \edef\@citeb{\expandafter\@firstofone\@citeb}%
    \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
    \if*\@citeb\else
    \@ifundefined{b@\@citeb\@extra@b@citeb}{%
       \NAT@citeundefined \PackageWarning{natbib}%
       {Citation `\@citeb' undefined}}{}\fi}%
  \@esphack}
\newcommand\NAT@parse[1]{{%
     \let\protect=\@unexpandable@protect\let~\relax
     \let\active@prefix=\@gobble
     \xdef\NAT@temp{\csname b@#1\@extra@b@citeb\endcsname}}%
     \expandafter\NAT@split\NAT@temp
     \expandafter\NAT@parse@date\NAT@date??????@@%
}
\newcommand\NAT@split[4]{%
  \gdef\NAT@num{#1}\gdef\NAT@name{#3}\gdef\NAT@date{#2}%
  \gdef\NAT@all@names{#4}%
  \ifx\NAT@noname\NAT@all@names \gdef\NAT@all@names{#3}\fi}
\newcommand\NAT@parse@date{}
\def\NAT@parse@date#1#2#3#4#5#6@@{%
  \ifnum\the\catcode`#1=11\def\NAT@year{}\def\NAT@exlab{#1}\else
  \ifnum\the\catcode`#2=11\def\NAT@year{#1}\def\NAT@exlab{#2}\else
  \ifnum\the\catcode`#3=11\def\NAT@year{#1#2}\def\NAT@exlab{#3}\else
  \ifnum\the\catcode`#4=11\def\NAT@year{#1#2#3}\def\NAT@exlab{#4}\else
    \def\NAT@year{#1#2#3#4}\def\NAT@exlab{{#5}}\fi\fi\fi\fi}
\newcommand\NAT@ifcmd{\futurelet\NAT@temp\NAT@ifxcmd}
\newcommand\NAT@ifxcmd{\ifx\NAT@temp\relax\else\expandafter\NAT@bare\fi}
\def\NAT@bare#1(#2)#3(@)#4\@nil#5{%
  \stepcounter{NAT@ctr}%
  \NAT@wrout{\arabic {NAT@ctr}}{#2}{#1}{#3}{#5}
}
\newcommand\NAT@wrout[5]{%
\if@filesw
      {\let\protect\noexpand\let~\relax
       \immediate
       \write\@auxout{\string\bibcite{#5}{{#1}{#2}{{#3}}{{#4}}}}}\fi
\ignorespaces}
\def\NAT@noname{{}}
\renewcommand\bibitem{%
  \@ifnextchar[{\@lbibitem}{%
    \stepcounter{NAT@ctr}\@lbibitem[\arabic{NAT@ctr}]}}
\def\@lbibitem[#1]#2{%
  \if\relax\@extra@b@citeb\relax\else
    \@ifundefined{br@#2\@extra@b@citeb}{}{%
     \@namedef{br@#2}{\@nameuse{br@#2\@extra@b@citeb}}}\fi
   \@ifundefined{b@#2\@extra@b@citeb}{\def\NAT@num{}}{\NAT@parse{#2}}%
   \item[\hfil\hyper@natanchorstart{#2\@extra@b@citeb}\@biblabel{\NAT@num}%
    \hyper@natanchorend]%
    \NAT@ifcmd#1(@)(@)\@nil{#2}}
\ifx\SK@lbibitem\@undefined\else
   \let\SK@lbibitem\@lbibitem
   \def\@lbibitem[#1]#2{%
     \SK@lbibitem[#1]{#2}\SK@\SK@@label{#2}\ignorespaces}\fi
\providecommand\bibcite{}
\renewcommand\bibcite[2]{\@ifundefined{b@#1\@extra@binfo}\relax
     {\NAT@citemultiple
      \PackageWarningNoLine{natbib}{Citation `#1' multiply defined}}%
  \global\@namedef{b@#1\@extra@binfo}{#2}}
\AtEndDocument{\NAT@swatrue\let\bibcite\NAT@testdef}
\newcommand\NAT@testdef[2]{%
  \def\NAT@temp{#2}\expandafter \ifx \csname b@#1\@extra@binfo\endcsname
    \NAT@temp \else \ifNAT@swa \NAT@swafalse
       \PackageWarningNoLine{natbib}{Citation(s) may have
          changed.\MessageBreak
          Rerun to get citations correct}\fi\fi}
\def\bibsection{\if@draft\newpage\fi
    \noappendix\section*{\refname}}
\newcounter{NAT@ctr}
\renewenvironment{thebibliography}[1]{%
 \bibsection\parindent \z@\bibpreamble\bibfont\list
   {\@biblabel{\arabic{NAT@ctr}}}{\@bibsetup{#1}%
    \setcounter{NAT@ctr}{0}}%
      \def\newblock{\hskip .11em plus.33em minus.07em}%
    \sloppy\clubpenalty4000\widowpenalty4000
    \sfcode`\.=1000\relax
    \ifonline\hypertarget{references}{}\fi
 }{\def\@noitemerr{%
  \PackageWarning{natbib}
     {Empty `thebibliography' environment}}%
  \endlist\vskip-\lastskip}
\let\bibfont\small
\let\bibpreamble\relax
\providecommand\reset@font{\relax}
\providecommand\bibname{Bibliography}
\providecommand\refname{References}
\newcommand\NAT@citeundefined{\gdef \NAT@undefined {%
    \PackageWarningNoLine{natbib}{There were undefined citations}}}
\let \NAT@undefined \relax
\newcommand\NAT@citemultiple{\gdef \NAT@multiple {%
    \PackageWarningNoLine{natbib}{There were multiply defined citations}}}
\let \NAT@multiple \relax
\AtEndDocument{\NAT@undefined\NAT@multiple}
\providecommand\@mkboth[2]{}
\providecommand\MakeUppercase{\uppercase}
\providecommand{\@extra@b@citeb}{}
\gdef\@extra@binfo{}
\providecommand\hyper@natanchorstart[1]{}
\providecommand\hyper@natanchorend{}
\providecommand\hyper@natlinkstart[1]{}
\providecommand\hyper@natlinkend{}
\providecommand\hyper@natlinkbreak[2]{#1}
\AtBeginDocument{\@ifundefined{bbl@redefine}{}{%
\let\@citex\org@@citex
\bbl@redefine\@citex[#1][#2]#3{%
  \@safe@activestrue\org@@citex[#1][#2]{#3}\@safe@activesfalse}%
\bbl@redefine\NAT@testdef#1#2{%
  \@safe@activestrue\org@NAT@testdef{#1}{#2}\@safe@activesfalse}%
\@ifundefined{org@@lbibitem}{%
\bbl@redefine\@lbibitem[#1]#2{%
  \@safe@activestrue\org@@lbibitem[#1]{#2}\@safe@activesfalse}}{}%
}}
\ifnum\NAT@sort>0
\newcommand\NAT@sort@cites[1]{%
\@tempcntb\m@ne
\let\@celt\delimiter
\def\NAT@num@list{}%
\def\NAT@cite@list{}%
\def\NAT@nonsort@list{}%
\@for \@citeb:=#1\do{\NAT@make@cite@list}%
\edef\NAT@cite@list{\NAT@cite@list\NAT@nonsort@list}%
\edef\NAT@cite@list{\expandafter\NAT@xcom\NAT@cite@list @@}}
\begingroup \catcode`\_=8
\gdef\NAT@make@cite@list{%
     \edef\@citeb{\expandafter\@firstofone\@citeb}%
    \@ifundefined{b@\@citeb\@extra@b@citeb}{\def\NAT@num{A}}%
    {\NAT@parse{\@citeb}}%
      \ifcat _\ifnum\z@<0\NAT@num _\else A\fi
       \@tempcnta\NAT@num \relax
       \ifnum \@tempcnta>\@tempcntb
          \edef\NAT@num@list{\NAT@num@list \@celt{\NAT@num}}%
          \edef\NAT@cite@list{\NAT@cite@list\@citeb,}%
          \@tempcntb\@tempcnta
       \else
          \let\NAT@@cite@list=\NAT@cite@list \def\NAT@cite@list{}%
          \edef\NAT@num@list{\expandafter\NAT@num@celt \NAT@num@list \@gobble @}%
          {\let\@celt=\NAT@celt\NAT@num@list}%
       \fi
    \else
       \edef\NAT@nonsort@list{\NAT@nonsort@list\@citeb,}%
 \fi}
\endgroup
\def\NAT@celt#1{\ifnum #1<\@tempcnta
  \xdef\NAT@cite@list{\NAT@cite@list\expandafter\NAT@nextc\NAT@@cite@list @@}%
  \xdef\NAT@@cite@list{\expandafter\NAT@restc\NAT@@cite@list}%
 \else
  \xdef\NAT@cite@list{\NAT@cite@list\@citeb,\NAT@@cite@list}\let\@celt\@gobble%
 \fi}
\def\NAT@num@celt#1#2{\ifx \@celt #1%
     \ifnum #2<\@tempcnta
        \@celt{#2}%
        \expandafter\expandafter\expandafter\NAT@num@celt
     \else
        \@celt{\number\@tempcnta}\@celt{#2}%
  \fi\fi}
\def\NAT@nextc#1,#2@@{#1,}
\def\NAT@restc#1,#2{#2}
\def\NAT@xcom#1,@@{#1}
\else
 \newcommand\NAT@sort@cites[1]{\edef\NAT@cite@list{#1}}\fi
%% Copyright 1993-1999 Patrick W Daly
%% Max-Planck-Institut f\"ur Aeronomie
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: daly@linmpi.mpg.de
\def\ModuleVersion#1[#2]{}
    \ModuleVersion{balance}
         [1999/02/23 4.3 (PWD)]
\newcommand{\@BAlancecol}{\if@twocolumn
  \setbox0=\vbox{\unvbox\@outputbox} \@tempdima=\ht0
  \advance\@tempdima by \topskip \advance\@tempdima
     by -\baselineskip \divide\@tempdima by 2
     \splittopskip=\topskip
  {\vbadness=\@M \loop \global\setbox3=\copy0
   \global\setbox1=\vsplit3 to \@tempdima
   \ifdim\ht3>\@tempdima \global\advance\@tempdima by 1pt \repeat}
   \setbox\@leftcolumn=\vbox to \@tempdima{\unvbox1\vfil}
   \setbox\@outputbox=\vbox to \@tempdima
     {\dimen2=\dp3\unvbox3\kern-\dimen2
      \vfil}
  \fi}
\newif\if@BAlanceone
\global\@BAlanceonefalse
\newdimen\oldvsize
\newcommand{\@BAdblcol}{\if@firstcolumn
       \unvbox\@outputbox \penalty\outputpenalty
       \global\oldvsize=\@colht \global\multiply \@colht by 2
       \global\@BAlanceonetrue
       \global\@firstcolumnfalse
  \else \global\@firstcolumntrue
       \if@BAlanceone
       \global\@BAlanceonefalse\@BAlancecol
       \global\@colht=\oldvsize \else
       \PackageWarningNoLine{balance}
          {You have called \protect\balance\space
             in second column\MessageBreak
           Columns might not be balanced}\fi
     \setbox\@outputbox\vbox to \@colht{\hbox to\textwidth
     {\hbox to\columnwidth {\box\@leftcolumn \hss}\hfil
      \vrule width\columnseprule\hfil \hbox to\columnwidth
      {\box\@outputbox \hss}}\vfil}\@combinedblfloats
     \@outputpage \begingroup \@dblfloatplacement
     \@startdblcolumn \@whilesw\if@fcolmade \fi
     {\@outputpage\@startdblcolumn}\endgroup
  \fi}
\newcommand{\@BAcleardblpage}{\clearpage\if@twoside \ifodd\c@page\else
  \hbox{}\newpage\fi\fi}
\newcommand{\@@cleardblpage}{}
\let\@@cleardblpage=\cleardoublepage

\newcommand{\@@utputdblcol}{}
\let\@@utputdblcol=\@outputdblcol
\newcommand{\balance}{\global\let\@outputdblcol=\@BAdblcol
  \global\let\cleardoublepage=\@BAcleardblpage}
\newcommand{\nobalance}{\global\let\@outputdblcol=\@@utputdblcol
  \global\let\cleardoublepage=\@@cleardblpage}
\endinput
%%
%% End of file `egu.cls'.
