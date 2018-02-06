/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#include "agent.hpp"
#include <cassert>
#ifdef DEBUG
#include <iostream>
#endif //DEBUG

namespace Joleste
{
    age_type Agent::maximum_age_=100;
    /// Default constructor: Uses standard function, should be done automatically by the compiler if we don't specify the function in the construction (or anything else)
    Agent::Agent( const Genome gen, int ID)
        : age_(0),gen_(gen), ID_(ID),last_feedback(0),seed_(0)//, energy_(ENERGY_MAX)
  {
      std::uniform_int_distribution<int> distribution_for_energy(1,INIT_ENERGY_VAR);
      energy_=INIT_ENERGY_CONST+distribution_for_energy(rng);
      assert(energy_>=0);
      assert(energy_<=INIT_ENERGY_CONST+INIT_ENERGY_VAR);
      gen_.mutate();
  } /// Constructor using a genome as input (should either be fct or later the neural network
  /// We could also pass the Genome a std::function (#include <functional>) which then can be used in the agent class; The funtion might be adjusted by genome member fcts.

  //// Actions ////
    /*  Perform a step, parameters:
     *  perceptions from environment, format is: 0-4 food, 5-9 agents
     *  avg marker in the cell
     *  marker of prey
     */
    int Agent::step(Genome::perception_type neighbors,bool binary_in)//,Genome::marker_type avg_markers,Genome::marker_type prey_markers)
    {
#ifdef DEBUG
        if(is_dead())
            std::cout<<"Agent "<<ID_<<" is dead! energy "<<energy_<<" age "<<age_<<std::endl;
#endif //DEBUG
        if(is_dead())             // in case some other agent killed it previously this turn
            return -1;            // default case in population.
#ifndef INVISIBLE_FOOD
#ifdef INTERACT
        assert(neighbors.size()==10);
#elif !defined INTERACT
        assert(neighbors.size()==5);
#endif //INTERACT
#endif //INVISIBLE_FOOD
        // ---------- increase age ----------
        birthday();
        // ---------- update perceptions ----------
        for (size_t i = 0; i < neighbors.size(); i++) { // convert food and agents quantities in binary value (0 or ENERGY_MAX)
            if(neighbors[i]!=0) {
                if(binary_in)
                    neighbors[i]=ENERGY_MAX;
                else
                    //neighbors[i]=ENERGY_MAX*neighbors[i]/(neighbors[i]+1);
                    neighbors[i]=std::min(ENERGY_MAX*1.0,neighbors[i]);
                }
        }
        // ---------- take decision ----------
        return decide_action(neighbors);
    }

    int Agent::decide_action(Genome::perception_type inputs)// here the genome (function or nn) should come into action
  {
      assert(inputs.size()==N_PERCEPTIONS);
#ifdef DEBUG
      std::cout<<std::endl<<"---------- Agent "<<get_ID()<<" plays ----------"<<std::endl;
#endif
      Genome::perception_type result = gen_.train(inputs,last_feedback);
      last_feedback=0;          // will be updated by the next feedback
      //add some random noise to break ties
      std::transform(result.begin(),result.end(),result.begin(),[](double&a){
              std::uniform_real_distribution<double> dist(-DECISION_NOISE,DECISION_NOISE);
              return a+dist(rng);});
      return std::distance(result.begin(),std::max_element(result.begin(),result.end()));
  }

  bool Agent::is_dead() const // return if agent is dead or alive
  {
      return
#ifdef IMMORTALS
          false;
#elif !defined IMMORTALS
      (
       (energy_ <= 0 ? true : false) ||
       (age_    >= maximum_age_ ? true : false) // TODO
      );
#endif
  }


    std::string Agent::ntoa(int num)
    {
        switch(num){
        case 0: return "north";
        case 1: return "west";
        case 2: return "east";
        case 3: return "south";
        case 4: return "eat";
        default: return "unknown";
        }
    }

    double Agent::aton(std::string act)
    {
        if(act=="north")
            return 0;
        else if(act=="west")
            return 1;
        else if(act=="east")
            return 2;
        else if(act=="south")
            return 3;
        else if(act=="eat")
            return 4;
        else
            return -1;
    }

    size_t Agent::test_configuration(Genome::perception_type input)
    {
        Genome::perception_type result(N_OUTPUTS);
        std::vector<size_t> idx(N_OUTPUTS);
        // initialize original index locations
        for (size_t i = 0; i != idx.size(); ++i) idx[i] = i;
        result = gen_.test_input(input);
        // sort indexes based on comparing values in v
        sort(idx.begin(), idx.end(),
             [&result](size_t i1, size_t i2) {return result[i1] > result[i2];});
#ifdef DEBUG
        for(auto &a:idx)
            std::cout<<ntoa(a)<<",";
        std::cout<<"\twith values: ";
        for(auto &r:idx)
            std::cout<<result[r]<<"|";
        std::cout<<std::endl;
#endif
        return idx[0];
    }

    // test the agent on different scenarios and return the action choosen in each of them
    Genome::actions_type Agent::test_agent()
    {
#ifdef DEBUG
        std::cout<<"--------------------------------------------------"<<std::endl;
        std::cout<<"Testing agent "<<ID_<<std::endl;
#endif
        Genome::perception_type input(N_PERCEPTIONS);
        Genome::actions_type result;
#ifndef INTERACT
#ifdef DEBUG
        std::cout<<"food here:\t";
#endif
        input={0,0,ENERGY_MAX,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food north:\t";
#endif
        input={ENERGY_MAX,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food west:\t";
#endif
        input={0,ENERGY_MAX,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food east:\t";
#endif
        input={0,0,0,ENERGY_MAX,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food south:\t";
#endif
        input={0,0,0,0,ENERGY_MAX};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#elif defined INTERACT
#ifdef INVISIBLE_FOOD
#ifdef DEBUG
        std::cout<<"food here:\t\t";
#endif
        input={0,0,ENERGY_MAX,0,0};
        auto res=test_configuration(input);
        // if (res==4)
        //     std::cout<<"Yeahhhhh!!!!!"<<std::endl;
        result.push_back(res);
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent north:\t";
#endif
        input={ENERGY_MAX,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent west:\t\t";
#endif
        input={0,ENERGY_MAX,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent east:\t";
#endif
        input={0,0,0,ENERGY_MAX,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent south:\t";
#endif
        input={0,0,0,0,ENERGY_MAX};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#elif !defined INVISIBLE_FOOD
#ifdef DEBUG
        std::cout<<"food here:\t\t";
#endif
        input={0,0,ENERGY_MAX,0,0,0,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food north:\t\t";
#endif
        input={ENERGY_MAX,0,0,0,0,0,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food west:\t\t";
#endif
        input={0,ENERGY_MAX,0,0,0,0,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food east:\t\t\t";
#endif
        input={0,0,0,ENERGY_MAX,0,0,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"food south:\t\t";
#endif
        input={0,0,0,0,ENERGY_MAX,0,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent here:\t\t";
#endif
        input={0,0,0,0,0,0,0,ENERGY_MAX,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent north:\t";
#endif
        input={0,0,0,0,0,ENERGY_MAX,0,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent west:\t\t";
#endif
        input={0,0,0,0,0,0,ENERGY_MAX,0,0,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent east:\t";
#endif
        input={0,0,0,0,0,0,0,0,ENERGY_MAX,0};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#ifdef DEBUG
        std::cout<<"agent south:\t";
#endif
        input={0,0,0,0,0,0,0,0,0,ENERGY_MAX};
        result.push_back(test_configuration(input));
        // --------------------------------------------------
#endif //INVISIBLE_FOOD
#endif //INTERACT
#ifdef DEBUG
        std::cout<<"--------------------------------------------------"<<std::endl;
#endif
        return result;
    }

    void Agent::seed_foraging(int val){
#ifdef INTERACT
#ifdef INVISIBLE_FOOD
        std::cout<<"Seeding foraging of agent "<<ID_<<std::endl;
        Genome::perception_type input(N_PERCEPTIONS);
        input={0,0,ENERGY_MAX,0,0};
        gen_.seed(input,aton("eat"),val);
#endif
#endif
    }

    void Agent::seed_social(int val){
#ifdef INTERACT
#ifdef INVISIBLE_FOOD
        seed_=1;
        std::cout<<"Seeding agent "<<ID_<<" socially"<<std::endl;
        Genome::perception_type input(N_PERCEPTIONS);
        // gen_.fill_weights_random();
        //food here
        // input={0,0,ENERGY_MAX,0,0};
        // gen_.seed(input,aton("eat"),val);
        // --------------------------------------------------
        //no food, agent north:\t";
        input={ENERGY_MAX,0,0,0,0};
        gen_.seed(input,aton("north"),val);
        // --------------------------------------------------
        //no food, agent west
        input={0,ENERGY_MAX,0,0,0};
        gen_.seed(input,aton("west"),val);
        // --------------------------------------------------
        //no food, agent east
        input={0,0,0,ENERGY_MAX,0};
        gen_.seed(input,aton("east"),val);
        // --------------------------------------------------
        //no food, agent south
        input={0,0,0,0,ENERGY_MAX};
        gen_.seed(input,aton("south"),val);
        //gen_.mutate();
#endif
#endif
    }
    void Agent::seed_antisocial(int val){
#ifdef INTERACT
#ifdef INVISIBLE_FOOD
        seed_=2;
        std::cout<<"Seeding agent "<<ID_<<" antisocially"<<std::endl;
        Genome::perception_type input(N_PERCEPTIONS);
        //no food, agent here
        // input={0,0,ENERGY_MAX,0,0};
        // gen_.seed(input,aton("eat"),val);
        // --------------------------------------------------
        //no food, agent north:\t";
        input={ENERGY_MAX,0,1,0,0};
        gen_.seed(input,aton("south"),val);
        // --------------------------------------------------
        //no food, agent west
        input={0,ENERGY_MAX,1,0,0};
        gen_.seed(input,aton("east"),val);
        // --------------------------------------------------
        //no food, agent east
        input={0,0,1,ENERGY_MAX,0};
        gen_.seed(input,aton("west"),val);
        // --------------------------------------------------
        //no food, agent south
        input={0,0,1,0,ENERGY_MAX};
        gen_.seed(input,aton("north"),val);
        //gen_.mutate();
#endif
#endif
    }

} // end namespace Joleste
