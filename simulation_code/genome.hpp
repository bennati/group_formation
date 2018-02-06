/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#ifndef GENOME_HPP
#define GENOME_HPP
#include <vector> /// we could use a list instead of vector, since for high orders the list is quicker and more efficient. If we just need up to 100 i think vector is more easy to use. If we need to switch the weights, then list is also preferable
#include <limits>
#include <random>
#include <algorithm>
#include <cassert>
#include <iostream>

#define ENERGY_MAX 100
#define MAX_WEIGHT 100

#if defined INTERACT && !defined INVISIBLE_FOOD
#define N_PERCEPTIONS 10        // exclude markers
#else
#define N_PERCEPTIONS 5
#endif
#define N_OUTPUTS 5             // cannot fight nor reproduce nor rest
#define WEIGHT_NOISE 0.03

namespace Joleste
{
    #ifndef RNG
    #define RNG
    extern std::mt19937 rng;
    #endif

    typedef unsigned age_type; // define age_type globally in the whole namespace

    class Genome
    {
    public:

        typedef std::vector<double> perception_type; /// defines the format of perceptions
        typedef std::vector<double> actions_type; /// defines the format of actions
        static void set_weight_mutation_rate( double m){weight_mutation_rate_ = m;}

        Genome();
        /// mutate the genome by changing x weights (x randomnumber, defined inside)
        void mutate();
        void set_temp(double x){TEMPERATURE=x;}
        void mutate_decay(size_t t);
        perception_type activate(perception_type inputs);
        perception_type train(perception_type input, double last_reward);
        std::string prettyprint_weights(perception_type inputs);
        double return_temp(){ return TEMPERATURE;}
        Genome::actions_type test_input(Genome::perception_type input);
        void seed(Genome::perception_type input,int action,int val);
        void fill_weights_random();
    private:

        std::vector<double> var_ranges_;
        std::vector<int> disc_numbers_;
        void mutate(double noise,size_t m);
        static double weight_mutation_rate_;
        double weights_[N_PERCEPTIONS][N_OUTPUTS];
        double TEMPERATURE;
    };

} // end namespace Joleste

#endif // !defined GENOME_HPP
