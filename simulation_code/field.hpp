/*
   Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
*/

#ifndef FIELD_HPP
#define FIELD_HPP

#include "genome.hpp"
#include <algorithm>
#include <functional>
#include <cassert>
#define GRASS_SPAWN_PROB 0.005

namespace Joleste
{
  class Field
  {
  public:
      /// Default constructor: Initialize weights all random.
      Field():food_(0),num_agents_(0),initial_food_(0) {
      }
      void add_agent(){num_agents_++;};   // add an agent to the field
      void rem_agent(){
          if(num_agents_>0)
              num_agents_--;
          else
              num_agents_=0;
      };
      void step(){spawn_bundle();} // performs a simulation step
      int get_num_agents(){return num_agents_;}
      int get_food(){return food_;}
      void set_initial_food(int i){initial_food_=i;}
      int get_initial_food(){return initial_food_;}
      void inc_food(){food_++;}
      void inc_food(int n){food_+=n;}
      void consume_bundle() {
          if(food_>0){
              food_--;
              assert(food_>=0);
          }else{
              std::cout<<"Warning: trying to consume a bundle in an empty cell"<<std::endl;
              food_=0;
          }
      }
  private:
      void spawn_bundle() {
          std::uniform_real_distribution<double> food_dist(0.0,1.0);
          if(food_dist(rng)<GRASS_SPAWN_PROB)
              food_++;                // TODO is food discrete or continuous?
      };
      void update_avg(double val,double &avg,int N,bool add);
      int food_;                   // amount of food
      int num_agents_;
      int initial_food_;
  };
} // end namespace Joleste

#endif // !defined POPULATION_HPP
