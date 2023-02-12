all:index.html

index.html:
	pandoc --standalone --mathjax="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" --wrap=none -V current_date="$(date +%Y-%m-%d%n)"  README.md -o index.html --css=style.css 

clean:
	rm index.html
