% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edmonds_karp.R
\name{bfs_edmonds}
\alias{bfs_edmonds}
\title{bfs_edmonds: Breadth-First Search}
\usage{
bfs_edmonds(arr, dp, source, sink)
}
\arguments{
\item{arr}{The adjacency matrix representing the graph.}

\item{dp}{The residual matrix.}

\item{source}{The index of the source node.}

\item{sink}{The index of the sink node.}
}
\value{
A vector representing the first augmenting path found by the BFS.
}
\description{
This function performs a Breadth-First Search (BFS) on a graph represented by an adjacency matrix.
}
