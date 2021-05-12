.DEFAULT_GOAL:=all

builder ?= stack
HAKYLL_PROVIDER_DIRECTORY ?= .

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
	mkdir -p $(HAKYLL_PROVIDER_DIRECTORY)/.tmp
	$(builder) exec site -- check | sed 's#Checking file ##g' | tee $(HAKYLL_PROVIDER_DIRECTORY)/.tmp/file_list.txt

all-serve: all
	$(builder) exec site -- server

serve:
	$(builder) exec site -- server

clean:
	$(builder) exec site -- clean
	cd $(HAKYLL_PROVIDER_DIRECTORY); rm -rf _temp .tmp .hakyll-cache public
