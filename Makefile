readme:
	pandoc -f markdown+lhs -t markdown src/interactive.lhs > README.md
