% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{S4Recipe-class}
\alias{S4Recipe-class}
\alias{S4ClassRecipe-class}
\alias{S4ClassRecipe}
\alias{S4MethodRecipe-class}
\alias{S4MethodRecipe}
\alias{S4GenericRecipe-class}
\alias{S4GenericRecipe}
\alias{S4RecipeList-class}
\alias{S4RecipeList}
\title{S4Recipe objects}
\usage{
S4ClassRecipe(recipe, expr, directives)

S4MethodRecipe(recipe, expr, directives)

S4GenericRecipe(recipe, expr, directives)

S4RecipeList(...)
}
\arguments{
\item{recipe}{The recipe (original S4Cofee comments)}

\item{expr}{The expression associated with the comments (currently a
srcRef but this is likely to change).}

\item{directives}{a DirectiveList containing the directives which make up
the recipe.}

\item{...}{Elements of the new S4RecipeList.}
}
\description{
S4Recipe objects represent the specifications of
classes and methods parsed from roxygen2-like S4Coffee comments
}
