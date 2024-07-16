run: toy.cpp
	clang++ -g -O3 toy.cpp -rdynamic `llvm-config --cxxflags --ldflags --system-libs --libs all`
.PHONY: run

clean:
	rm -rf ./a.out
.PHONY: clean
