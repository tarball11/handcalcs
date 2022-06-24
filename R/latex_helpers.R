
# Encloses the solution in a LaTeX aligned block
add_aligned<- function(solution) {
	paste0("\\begin{aligned} ", solution, "\\end{aligned}")
}

# Encloses the solution in a LaTeX display math block
add_math<- function(solution) {
	paste0("$$ ", solution, "$$")
}
