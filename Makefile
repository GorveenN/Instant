all: src/*
	stack build
	stack --local-bin-path . install

clean:
	rm -rf .stack-work .build insc_jvm insc_llvm
