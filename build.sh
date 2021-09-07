git submodule init
git submodule update
cd pyml
make
cd ..
coq_makefile -f _CoqProject -o Makefile
make clean && make && make install
