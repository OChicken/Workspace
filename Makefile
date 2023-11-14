.PHONY: resrarch projects footprint

research:
	emacs --script blog.el research

projects:
	emacs --script blog.el projects

footprint:
	emacs --script blog.el footprint

generate:
	emacs --script blog.el g

g: generate

generate-force:
	emacs --script blog.el g t

g-t: generate-force

server:
	python -m http.server

s: server

deploy:
	emacs --script blog.el d

d: deploy

clean:
	emacs --script blog.el clean
