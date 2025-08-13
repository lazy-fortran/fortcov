.PHONY: external clean-external

external: external/pycobertura external/gcovr external/lcov

external/pycobertura:
	git clone https://github.com/aconrad/pycobertura.git external/pycobertura

external/gcovr:
	git clone https://github.com/gcovr/gcovr.git external/gcovr

external/lcov:
	git clone https://github.com/linux-test-project/lcov.git external/lcov

clean-external:
	rm -rf external/