CXX = mpicxx # g++
CXXFLAGS = -Wall -g -std=c++0x  -DINTERACT -DINVISIBLE_FOOD -D'SEED_ITERATION=0'

all: test

test: population.o agent.o genome.o field.o test.o
	$(CXX) $(CXXFLAGS) $^ -o $@

%.o: %.cpp %.hpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

run:
	./test

clean:
	rm *.o test
