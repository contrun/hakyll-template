.DEFAULT_GOAL:=all

builder ?= stack

all: prepare build fixup

prepare:
	npm install
	$(builder) update
	$(builder) build

# Build twice to use intermediate parsed org files
build:
	$(builder) exec site -- build
	$(builder) exec site -- build

fixup:
	mkdir -p .tmp
	$(builder) exec site -- check | sed 's#Checking file ##g' | tee .tmp/file_list.txt

serve: all
	$(builder) exec site -- server

clean:
	$(builder) exec site -- clean
	rm -rf _temp .tmp .hakyll-cache public
