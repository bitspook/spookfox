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
else
	@echo "Versions look ok."
endif

clean:
	rm -r spookfox-addon/dist
	cargo clean

publish-addon:
	cd spookfox-addon; yarn publish-addon
