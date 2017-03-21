CXX = mpicxx #CXX = g++
CXXFLAGS = -Wall -g -std=c++0x  -DINTERACT -DINVISIBLE_FOOD -DIMMORTALS -DFILTER_STATIC_PERCEPTIONS -D'SEED_ITERATION=0' -DSEED_FORAGING

all: test

test: population.o agent.o genome.o field.o test.o
	$(CXX) $(CXXFLAGS) $^ -o $@

%.o: %.cpp %.hpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

run:
	./test

clean:
	rm *.o test
