/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#include "genome.hpp"
#include <sstream>


namespace Joleste
{
    std::mt19937 rng(rand()); /// rng for the whole simulation. Available in the whole namespace!

    double Genome::weight_mutation_rate_ = 0.5;

    Genome::Genome()
        : var_ranges_(N_PERCEPTIONS,ENERGY_MAX)
        , disc_numbers_(N_PERCEPTIONS,2)
    {
        this->fill_weights_random();
    }

    void Genome::mutate(double noise,size_t m)
    {
        std::uniform_int_distribution<age_type> distribution_for_weights_1(0,N_PERCEPTIONS-1);
        std::uniform_int_distribution<age_type> distribution_for_weights_2(0,N_OUTPUTS-1);
        double range = noise*MAX_WEIGHT;
        std::uniform_real_distribution<double> distribution_for_weight_values(-range,range);

        // Mutate a random selection of M weights
        for( size_t i = 0; i < m; ++i ) {
            age_type i1 = distribution_for_weights_1(rng);
            age_type i2 = distribution_for_weights_2(rng);
            weights_[i1][i2] = std::max(std::min(weights_[i1][i2]+distribution_for_weight_values(rng),(double)MAX_WEIGHT),(double)-MAX_WEIGHT);
        }
    }

    void Genome::mutate()
    {
        mutate(WEIGHT_NOISE,weight_mutation_rate_*N_PERCEPTIONS*N_OUTPUTS);
    }

    void Genome::mutate_high()
    {
        mutate(ADAPT_WEIGHT,N_PERCEPTIONS*N_OUTPUTS);
    }

    void Genome::mutate_decay(size_t t)
    {
        // exponential decay
        //double x = std::min((double)t/TEMPERATURE,700.0); // avoid overflows
        //mutate(std::max(exp(-x),WEIGHT_NOISE),weight_mutation_rate_);
        // hyperbolic decay
        double x = (double) t/TEMPERATURE;
        mutate(1/(1+x),weight_mutation_rate_);
    }

    void Genome::fill_weights_random(){
        std::uniform_real_distribution<double> distribution_for_weight_values(0.,1.);
        for (int i = 0; i < N_PERCEPTIONS; i++)
            for (int j=0; j < N_OUTPUTS; j++)
                weights_[i][j] = distribution_for_weight_values(rng);
    }

    // compute and return the score of possible actions for the current state
    // update Q-table with reward from last round
    Genome::perception_type Genome::activate(perception_type inputs) {
        assert(inputs.size()==N_PERCEPTIONS);
        perception_type retval = perception_type(N_OUTPUTS,0);
        for (int j = 0; j < N_OUTPUTS; j++)
            for(int i=0; i<N_PERCEPTIONS;i++)
                retval[j]+=weights_[i][j]*inputs[i];
        return retval;
    }

    Genome::perception_type Genome::train(perception_type input, double last_reward){
        return activate(input);}

    Genome::actions_type Genome::test_input(Genome::perception_type input){
        return activate(input);}

    std::string Genome::prettyprint_weights(perception_type inputs) {
        std::stringstream retval;
        retval<<("| |");
        for (int i = 0; i < N_PERCEPTIONS; i++) {
            retval<<"Input"<<i<<"|";
        }
        retval<<std::endl;
        for (int j = 0; j < N_OUTPUTS; j++) {
            retval<<"|Output"<<j<<"|";
            for(int i=0; i<N_PERCEPTIONS;i++)
                retval<<weights_[i][j]<<"|";
            retval<<std::endl;
        }
        retval<<"inputs: ";
        for(auto &a:inputs)
            retval<<a<<",";
        retval<<std::endl<<"outputs: ";
        perception_type activ = activate(inputs);
        for(auto &a:activ)
            retval<<a<<",";
        retval<<std::endl;
        return retval.str();
    }

    void Genome::seed(Genome::perception_type input,int action,int val) {
        assert(input.size()==N_PERCEPTIONS);
        for(int i=0; i<N_PERCEPTIONS;i++)
            if(input[i]!=0)
                weights_[i][action]=std::max(std::min(weights_[i][action]+=val,(double)MAX_WEIGHT),(double)-MAX_WEIGHT);
    }

} // end namespace Joleste
