#lang scribble/acmart @acmsmall

@title{Artifact: Corpse Reviver}
@author{Cameron Moy}
@author{Phúc C. Nguyễn}
@author{Sam Tobin-Hochstadt}
@author{David Van Horn}

@section{Installing}

@subsection{Docker (Recommended)}

Install @hyperlink["https://docker.com/"]{Docker}.

@verbatim{
$ docker pull camoy/corpse-reviver
$ docker run -it camoy/corpse-reviver
}

@subsection{Local}

Install @hyperlink["https://download.racket-lang.org"]{Racket 7.8}.

@verbatim{
$ raco pkg install --clone https://github.com/camoy/corpse-reviver
}

@section{Benchmarking}

@section{Empirical Claims}

@section{Custom Program (Optional)}
