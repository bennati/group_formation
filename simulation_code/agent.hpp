/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#ifndef AGENT_HPP
#define AGENT_HPP

#include "genome.hpp"
#include <algorithm>
#include <iostream>
#include <math.h>

#define M_MAX 1000

#ifdef IMMORTALS
#define INIT_ENERGY_CONST 0
#define INIT_ENERGY_VAR 1
#else
// Set accordingly to MAX_ENERGY that is used for reproduction
#define INIT_ENERGY_CONST 0
#define INIT_ENERGY_VAR 50
#endif
#define DECISION_NOISE 0.001*MAX_WEIGHT

//// Joleste is the declared namespace for the implementation (to make all things accessible in the implementation also in the other codes!)
//// it should be mentioned that Joleste represents three great guys on their challenge for the nobel... you may change the name. But please in all files then :-)
/// You could do so by: sed s/Joleste/<new_name>/g *.cpp; sed s/Joleste/<new_name>/g *.hpp
namespace Joleste
{
  class Agent
  {
    public:
      //// static variables / functions ////
      static age_type maximum_age_; // max age of agents
      static void alter_max_age(age_type ag){ maximum_age_ = ag; }

      //// Constructors ////
      Agent(int ID) : Agent(Genome(),ID,0) {
      } /// Default constructor: Uses standard function

      Agent( const Genome gen, int ID,size_t t); /// Constructor using a genome as input (should either be fct or later the neural network). The genome mutates.
      int step(Genome::perception_type neighbors,bool binary_in);// ,Genome::marker_type avg_markers,Genome::marker_type prey);

      //// Setters & Getters ////
      int get_ID(){return ID_;}
      int get_seed() {return seed_;}
      void set_seed(int s) {seed_=s;}
      void set_energy( double energy ) {energy_ = energy;} // set energy of agent
      double get_energy() {return energy_;} // get energy of agent
      void vary_energy( double delta) {energy_ += delta; }//if(energy_<0) energy_=0;} // else if(energy_>ENERGY_MAX) energy_=ENERGY_MAX;}
      age_type get_age() const {return age_;} // return the age of the agent
      void birthday() {age_++;} /// let agent grow older by 1 year

      //// Actions ////
      void give_reward(double reward) {last_feedback=reward; vary_energy(reward);}
      void adapt_small(){gen_.mutate();}
      void adapt() {gen_.mutate_high();} //CHANGED FOR DEBUGGING:adapt (int, double, double, int); /// here the agent should learn (adapt), meaning taking the decision (int) the energy, health gained and if offspring (int)
      double get_temp(){return gen_.return_temp();}

      /// we need to implement the decide-action in the population, so that the agent returns it's decision and then returns the decision in integer form
      /// this we can then pick up in the population class and add the values health to the agent's health and substract it from the other agents health

      /// here we should implement the basis function which accesses the (updated perceptions and returns an decision (int type)

      bool is_dead() const; // return if agent is dead or alive
      Agent replicate(int ID,size_t t)  /// Create offspring inheriting its genome but with slightly random mutations
      {Agent ret = Agent(gen_,ID,t);
          ret.set_seed(this->get_seed());
          return ret;}
      Genome::actions_type test_agent();
      void seed_foraging(int val);
      void seed_social(int val);
      void seed_antisocial(int val);

      static std::string ntoa(int num);
      static double aton(std::string act);
  private:
      size_t test_configuration(Genome::perception_type input);
      int decide_action(Genome::perception_type inputs); // here the genome (function or nn) should come into action

      age_type age_;
      double energy_; // energy of agent
      Genome gen_; // genome of agent (fct or nn)
      int ID_;
      double last_feedback;
      int seed_;                // type of seeding

  };

} // end namespace Joleste

#endif
