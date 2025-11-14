build:
	cd spookfox-addon; npm run build

lint:
	cd spookfox-addon; npm run lint

guix_pkg_file := ".guix/bitspook/packages/spookfox.scm"
guix_version := $(shell cat ${guix_pkg_file} | grep -o 'version ".*"' | tr -d ' ' | cut -d '"' -f 2)
addon_version := $(shell cat spookfox-addon/src/manifest.json | grep -o '"version": ".*"' | tr -d '[:blank:]"' | cut -d ':' -f 2)
el_pkg_version := $(shell cat lisp/spookfox.el | grep 'Version:.*' | grep -o '[0-9\.]*')
el_version := $(shell cat lisp/spookfox.el | grep 'defvar.*version' | grep -o '[0-9\.]*')

master_version := $(shell git show master:${guix_pkg_file} | grep 'version' | grep -o '"[0-9\.]*"')

version-check:
ifneq ($(filter-out $(guix_version), $(addon_version) $(el_version) $(el_pkg_version)),)
		$(error "Versions don't match. manifest.json (${addon_version}), guix.scm (${guix_version}) and spookfox.el (${el_version}, ${el_pkg_version}) must have same version.")
else ifeq ($(guix_version),$(master_version))
		$(error "Please bump the version. We will not be able to release same version again")
else ifeq ($(shell git describe --tag --abbrev=0 2> /dev/null),)
		$(error "Please set a tag to release on HEAD.")
else
		@echo "Versions look ok."
endif

version-set:
ifeq ($(VERSION),)
		$(error "Please set VERSION argument. e.g make version-set VERSION=1.0.0")
endif
		sed -i '/version.*/s/"[0-9\.]*"/"$(VERSION)"/' ${guix_pkg_file}
		sed -i '/"version".*/s/"[0-9\.]*"/"$(VERSION)"/' spookfox-addon/src/manifest.json
		sed -i '/defvar.*version.*/s/"[0-9\.]*"/"$(VERSION)"/' lisp/spookfox.el
		sed -i '/Version/s/[0-9]\.[0-9]\.[0-9]/$(VERSION)/g' lisp/spookfox.el
		git add guix.scm spookfox-addon/src/manifest.json lisp/spookfox.el
		git commit -m 'Version bump to $(VERSION)'
		git tag -a v$(VERSION) -m "Version $(VERSION)"
		@echo "Version set to: ${VERSION}"

clean:
	cd spookfox-addon && npm run clean

publish-addon:
	cd spookfox-addon; npm run publish-addon
