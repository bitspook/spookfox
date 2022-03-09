build:
	cargo build --release
	cd spookfox-addon; yarn build

lint:
	cargo check
	cd spookfox-addon; yarn lint

yarn_version := $(shell cat spookfox-addon/package.json | grep -o '"version": ".*"' | tr -d '[:blank:]"' | cut -d ':' -f 2)
addon_version := $(shell cat spookfox-addon/src/manifest.json | grep -o '"version": ".*"' | tr -d '[:blank:]"' | cut -d ':' -f 2)
native_version := $(shell cat spookfox-native/Cargo.toml | grep 'version' | cut -d '=' -f 2 | tr -d '[:blank:]"')

master_version := $(shell git show master:spookfox-addon/package.json | grep 'version' | grep -o '"[0-9\.]*"')

version-check:
ifneq ($(filter-out $(yarn_version),$(native_version) $(addon_version)),)
	$(error "Versions don't match. manifest.json, package.json and Cargo.toml must have same version.")
else ifeq ($(yarn_version),$(master_version))
	$(error "Please bump the version. We will not be able to release same version again")
else ifeq ($(shell git describe --tag --abbrev=0 2> /dev/null),)
	$(error "Please set a tag to release on HEAD.")
else
	@echo "Versions look ok."
endif

version-set:
ifeq ($(VERSION),)
		$(error "Please set VERSION argument. e.g make version-set v1.0.0")
endif
		sed -i '/"version".*/s/"[0-9\.]*"/"$(VERSION)"/' spookfox-addon/package.json
		sed -i '/"version".*/s/"[0-9\.]*"/"$(VERSION)"/' spookfox-addon/src/manifest.json
		sed -i '/version.*=/s/"[0-9\.]*"/"$(VERSION)"/' spookfox-native/Cargo.toml
		git add spookfox-addon/package.json spookfox-addon/src/manifest.json spookfox-native/Cargo.toml
		git commit -m 'Version bump to $(VERSION)'
		git tag -a v$(VERSION) -m "Version $(VERSION)"
		@echo "Version set to: ${VERSION}"

clean:
	rm -r spookfox-addon/dist
	cargo clean

publish-addon:
	cd spookfox-addon; yarn publish-addon
