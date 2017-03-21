CXX = mpicxx # g++
CXXFLAGS = -Wall -g -std=c++0x  -DINTERACT -DINVISIBLE_FOOD -DFILTER_STATIC_PERCEPTIONS -D'FAMINE_ITERATION=20000' -DRESPAWN

all: test

test: population.o agent.o genome.o field.o test.o
	$(CXX) $(CXXFLAGS) $^ -o $@

%.o: %.cpp %.hpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

run:
	./test

clean:
	rm *.o test
