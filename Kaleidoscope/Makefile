run: toy.cpp
	clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core`
.PHONY: run

clean:
	rm -rf ./a.out
.PHONY: clean
