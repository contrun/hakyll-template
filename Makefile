.DEFAULT_GOAL:=all
builder ?= cabal

ifneq (,$(wildcard ./.env))
	include .env
	export $(shell sed 's/=.*//' .env)
endif

HAKYLL_PROVIDER_DIRECTORY ?= ..
HAKYLL_DESTINATION_DIRECTORY ?= ../public
HAKYLL_BUILDER_DIRECTORY ?= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
export HAKYLL_PROVIDER_DIRECTORY HAKYLL_DESTINATION_DIRECTORY HAKYLL_BUILDER_DIRECTORY
HAKYLL_ENCRYPTION_PASSWORD ?= w6Wzr8AkL61UAdmfJvyU0CeBkyNcocX7lXo8H1MAsf611ei2GPoz1YaRaymqOBT6

all: prepare build fixup

prepare:
	npm install
	$(builder) update
	$(builder) build

# Build twice to use intermediate parsed org files
build:
	if [ "$(shell realpath '$(HAKYLL_BUILDER_DIRECTORY)')" != "$(shell realpath '$(HAKYLL_PROVIDER_DIRECTORY)/vendor/')" ]; then cp -TRv '$(HAKYLL_BUILDER_DIRECTORY)/' '$(HAKYLL_PROVIDER_DIRECTORY)/vendor/'; fi
	$(builder) exec site -- build
	$(builder) exec site -- build

.ONESHELL:
fixup:
	set -xeu
	self=$$$$
	tryOrDie() {
		eval "$$@" || (kill $$self)
	}
	files="$$(tryOrDie find '$(HAKYLL_DESTINATION_DIRECTORY)' -name '*.html')"
	grep --include '*.org' -lZRi '^#+encryption:' '$(HAKYLL_PROVIDER_DIRECTORY)' | xargs -0 -r -n1 sed -nE '/^slug:/ s/^slug:\s*//p; /#\+slug:/I s/#\+slug:\s*//pI' | sort | uniq | while IFS= read -r slug; do
		salt="$$(echo "$$slug" | tryOrDie tr -cd '[:alnum:]')BPmzB7gZEq1m4YtCk2Mr"
		file="$$(echo "$$files" | tryOrDie grep "/$$slug/index.html")"
		# staticrypt does not appear to support non-alphanum password
		password="$$(tryOrDie openssl passwd -6 -salt "$$salt" "$(HAKYLL_ENCRYPTION_PASSWORD)" | tr -cd '[:alnum:]')"
		tryOrDie npm exec -- staticrypt -o "$$file" "$$file" -f templates/password_template.html "$$password"
	done

all-serve: all
	$(builder) exec site -- server

serve:
	$(builder) exec site -- server

clean:
	$(builder) exec site -- clean
	$(builder) clean
	cd '$(HAKYLL_PROVIDER_DIRECTORY)'; rm -rf _temp .tmp .hakyll-cache
	rm -rf '$(HAKYLL_DESTINATION_DIRECTORY)'
