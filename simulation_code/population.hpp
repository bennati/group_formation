/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

/* FLAGS:
 * DEBUG if enabled performs consistency checks and prints more output
 * INTERACT if enabled makes agents aware of other agents
 * INVISIBLE_FOOD if enabled agents can only see food in their current cell
 * BINARY_IN if enabled inputs are binary, if disabled they increase with the number of food/agents
 */

#ifndef POPULATION_HPP
#define POPULATION_HPP

#include "agent.hpp"
#include "field.hpp"
#include <list> ///operations on list most efficient for high population sizes!
#include <vector>               // but list does not support accessing elements by their index
#include <algorithm>
#include <functional>
#include <numeric> // std::iota
#include <cstdlib>
#include <cassert>
#include <utility>            // needed for swap
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

#define assert__(x) for ( ; !(x) ; assert(x) )
#define EAT_ENERGY 0
#define FOOD_ENERGY 1
#define ENTER_ENERGY 0
#define MOVE_ENERGY 0
// determines how many agents are attempted to be killed during every round. If the value is 0, the whole population is extracted.
#define FRACTION_DEATHS 0.0

#ifndef SEED_ITERATION
#define SEED_ITERATION SIZE_MAX
#endif
#ifndef FAMINE_ITERATION
#define FAMINE_ITERATION SIZE_MAX
#endif
#define SEED_VALUE MAX_WEIGHT/10.0

// choose how to generate food
#define FOOD_UNIFORM

namespace Joleste
{
    /**
     * Population of agents.
     */
    class Population
    {
    private:
        typedef Field field_type; /// containing field and number of agents in that field
        typedef std::pair<Agent, int> population_type; /// containing cell identifier and agent
        typedef std::vector<population_type>::const_iterator const_iterator; /// create iterator for the population
        Agent& get_agent_at(population_type &a) {return a.first;}
        Agent& get_agent_at(int index) {return population_[index].first;}
        Agent& get_agent_at(std::vector<population_type>::iterator it) {return (*it).first;}
        int get_field_at(population_type a) {return a.second;}
        int get_field_at(int index) {return population_[index].second;}
        int get_field_at(const_iterator it) {return (*it).second;}
        // functions
        std::vector<int> get_neighboring_cells(int,int,int, int);
        std::vector<std::vector<int> > field_of_view(int,int,int, int);
        int find_prey(int fieldID); // find an agent that is in the field fieldID
        void del_agent(size_t offset) {std::vector<population_type>::iterator it=population_.begin(); std::advance(it,offset); del_agent(it);}
        void del_agent(std::vector<population_type>::iterator &it); // delete an agent in the field fieldID
        Genome::perception_type compute_agent_perceptions(int);
        void agent_moves(population_type &a,int orientation);
        void agent_eats(population_type &a,size_t num_food);
        Agent agent_replicates(Agent &a);
        Agent agent_replicates(population_type &a) {return agent_replicates(get_agent_at(a));}
        void agent_fights(population_type &a,int preyID);
        void shuffle_agents();  // shuffles the population
        void initialize_fields();
        void initialize_population();
        void reproduction();
        void replenish_population();
        void remove_dead_agents();
        void debug_step(int fieldID, Genome::perception_type perceptions);
        void seed_population();
        void create_food_source(std::vector<field_type> &fields,int x0,int x1,int init_food);

        const_iterator begin() const {return population_.begin();} /// Get iterators for the population_ container
        const_iterator end() const {return population_.end();} /// Get iterators for the population_ container
        // variables
        std::size_t nmin_;      // min number of agents
        std::size_t n0_;        // initial population size
        std::size_t f0_;        // number of cells with food
        std::size_t max_age_;   // the maximum life expectancy
        std::vector<population_type> population_;
        std::vector<field_type> fields_; /// container for the food of each cell
        int agent_counter_;     // defines the ID of a new agent, needs to be incremented any time an agent is created
        size_t fmax_;
        size_t fov_radius_;
        bool binary_in_;
        bool direct_feedback_;
        size_t field_size_;
        double social_ratio_;
        double antisocial_ratio_;

        std::ofstream logFile;
        std::ofstream statsFile;


    public:

        /// Constructor with maximum population size and star population size
        /// also the agents shouls be initialized here! The last argument takes the
        /// field size
        Population( std::size_t nmin, std::size_t n0, std::size_t f0,std::size_t fmax, std::size_t field_size, age_type max_age, size_t fov_radius, bool binary_in, bool direct_feedback,double social_ratio,double antisocial_ratio);

        /// Classes with a vtable should have a virtual destructor.
        /// just to remember vtable contains pointers to the virtual functions! Good to remember for an job interview :-)
        /// There can only be one vtable per class, and all objects of the same class will share the same vtable. You need them!
        virtual ~Population();

        /// Simulate the population for time years.
        void simulate( std::size_t time, const std::string &fileName = std::string());
        std::size_t size() const {return population_.size();} /// Get size of population
        std::string print_pop() {return print_pop(population_);}
        std::string print_pop(int seed);
        std::string print_pop(std::vector<population_type> pop);
        std::vector<std::vector<double> > write_pop() {return write_pop(population_);}
        void write_pop(int seed);
        std::vector<std::vector<double> > write_pop(std::vector<population_type> pop);

        /// Simulate one time step (year).
        /// this contains: shuffle agents, check for neighbours (population_.second), give each agent the parameters required,
        /// execute the agents functions (make decision,...) and take the results (depending of action) to change agents position/fight, etc.
        virtual std::string step(std::size_t timeStep);
    };

} // end namespace Joleste

#endif // !defined POPULATION_HPP
