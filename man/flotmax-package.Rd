\name{flotmax-package}
\alias{flotmax-package}
\alias{flotmax}
\docType{package}
\title{
Computing the maximum flow in a flow network, an implementation of Edmond-Karps and Floyd-Fulkerson algorithms
}
\description{
This package implements the Edmond-Karps and Floyd-Fulkerson algorithms in R and Rcpp for computing the maximum flow in a flow network. Additionally, it includes functionality to compare the computational complexity of these algorithms through simulations.
}
\details{
It implements the functions `ford_fulkerson` and `ford_fulkerson150`, which respectively implement the Ford-Fulkerson algorithm in R and Rcpp, as well as `edmonds_karp` and `edmondsKarp`, which respectively implement the Edmonds-Karp algorithm in R and Rcpp. See the documentation.
}
\author{
Assmaa Alzamadi, Viviana Gavilanes, Noemie Turmel.

Maintainer: Noemie Turmel <noemie.turmel@ensiie.eu>
}
\references{
  This optional section can contain literature or other references for
  background information.
}
\keyword{ package }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
