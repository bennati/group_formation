/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#include "population.hpp"

//// Template classes for vector-vector and vector-scalar operations
template <typename T>
std::vector<T> operator+(const std::vector<T>& a, const std::vector<T>& b)
{
    assert(a.size() == b.size());

    std::vector<T> result;
    result.reserve(a.size());

    std::transform(a.begin(), a.end(), b.begin(),
                   std::back_inserter(result), std::plus<T>());
    return result;
}
template <typename T>
std::vector<T>& operator+=(std::vector<T>& a, const std::vector<T>& b)
{
    a=a+b;
    return a;
}
template <typename T>
std::vector<T> operator-(const std::vector<T>& a, const std::vector<T>& b)
{
    assert(a.size() == b.size());

    std::vector<T> result;
    result.reserve(a.size());

    std::transform(a.begin(), a.end(), b.begin(),
                   std::back_inserter(result), std::minus<T>());
    return result;
}
template <typename T>
std::vector<T> operator*(const std::vector<T>& a, const std::vector<T>& b)
{
    assert(a.size() == b.size());

    std::vector<T> result;
    result.reserve(a.size());

    std::transform(a.begin(), a.end(), b.begin(),
                   std::back_inserter(result), std::multiplies<T>());
    return result;
}
template <typename T>
std::vector<T> operator/(const std::vector<T>& a, const T& b)
{

    std::vector<T> result;
    result.reserve(a.size());

    std::transform(a.begin(), a.end(), std::back_inserter(result),
                   std::bind2nd(std::divides<T>(),b));
    return result;
}

namespace Joleste
{

    Population::Population( size_t nmin, size_t n0, size_t f0, size_t fmax, size_t field_size, age_type max_age, size_t fov_radius, bool binary_in, bool direct_feedback,double social_ratio,double antisocial_ratio)
        :   nmin_( nmin )      // min number of agents, spawn random agents anytimes population is smaller than this value
        ,   n0_(n0)            // initial population size
        ,   f0_(f0)             // initial number of food sources, can change during the simulation
        ,   max_age_(max_age)
        ,   agent_counter_(0)   // counter used to give unique IDs to the agents
        ,   fmax_(fmax)         // max number of food in a cell
        ,   fov_radius_(fov_radius) // field of view radius of agents
        ,   binary_in_(binary_in)   // whether perceptions should have binary or continuous values
        ,   direct_feedback_(direct_feedback) // whether to give a small positive reward anytime an agent enters a cell with food, might be useful for learning?
        ,   field_size_(field_size) // size of grid, fields are ^2
        ,   social_ratio_(social_ratio)
        ,   antisocial_ratio_(antisocial_ratio)
    {
        Agent::alter_max_age(max_age);
        fields_.reserve(field_size_*field_size_);
        assert(f0<=field_size_*field_size_ && "cannot allocate more food cells than available cells");
        initialize_fields();         // set initial food
        initialize_population();   // generate initial population
        std::cout<<"done!"<<std::endl;
        // ---------- initialize log file ----------
    }

    Population::~Population(){
        if(statsFile.is_open())
            statsFile.close();
    }

    void Population::simulate( size_t time , const std::string &fileName)
    {
        // initialize output file
        // When changing this column names, change as well the order of output of Agent::test_agent()
#ifndef INTERACT
        std::vector<std::string> colnames = {"foodH","foodN","foodW","foodE","foodS"};
#elif defined INTERACT
#ifdef INVISIBLE_FOOD
        std::vector<std::string> colnames = {"foodH","agentN","agentW","agentE","agentS"};
#elif !defined INVISIBLE_FOOD
        std::vector<std::string> colnames = {"foodH","foodN","foodW","foodE","foodS","agentH","agentN","agentW","agentE","agentS"};
#endif
#endif

        std::ostringstream str_stat; /// for output
        std::string outstring;

        // print header in file
        if(!fileName.empty()) {
            statsFile.open(fileName);         //Opening file to print info to
            str_stat<<"timeStep,agentID,x,y,value,action,seed";
            for(auto&n : colnames )
                str_stat<<","<<n;
            str_stat<<std::endl;
            outstring = str_stat.str();
            statsFile << outstring;
            str_stat.str(std::string());
        }
        for (size_t i = 0; i < time; i++) {
#ifdef INTERACT
#ifdef INVISIBLE_FOOD
            if(i==FAMINE_ITERATION) {       // seed an agent with the social strategy
                std::cout<<"Famine"<<std::endl;
                f0_=5;
            }
            if(i==SEED_ITERATION) {       // seed an agent with the social strategy
                std::cout<<"Seeding"<<std::endl;
                seed_population();
            }
#endif
#endif
            outstring=step(i);  // run and store a simulation step

            if(statsFile.is_open()) {
                statsFile << outstring;
                statsFile.flush();    // we are going to delete the string in a moment
            }
        }

        if(statsFile.is_open())
            statsFile.close();
    }

    std::string Population::step(size_t timeStep)
    {

        size_t num_food=0;

        std::ostringstream str_stats; /// stream for ofstream
        str_stats<<"";

#ifndef IMMORTALS
        remove_dead_agents();
        reproduction(); // replicate agents
#endif

        // increase food in fields
        for(size_t i =0; i<fields_.size();i++) {
            int food = fields_[i].get_food();
            if(food>0)
                num_food++;
            // ---------- log information about fields ----------
            if(food>0) {
                int x = i%field_size_;
                int y = (int)i/field_size_;
                str_stats<<timeStep<<",-1,"<<x<<","<<y<<","<<food<<",-1"<<",-1";
                for(int i=0;i<N_OUTPUTS;i++)
                    str_stats<<",-1";
                str_stats<<std::endl;
            }
        }
#ifdef DEBUG
        assert(num_food==f0_);
#endif
        shuffle_agents();               // ensure agents play with random order
        for(auto &a : population_)
            {
                int fieldID = get_field_at(a); // where the agent is
                Genome::perception_type perceptions = compute_agent_perceptions(fieldID);
                int action = get_agent_at(a).step(perceptions,binary_in_); // let the agent choose an action
                if(action==Agent::aton("north") ||
                   action==Agent::aton("west") ||
                   action==Agent::aton("east") ||
                   action==Agent::aton("south")) {
                    // just move
                    get_agent_at(a).give_reward(-MOVE_ENERGY);
                    agent_moves(a,action);
                } else if(action==Agent::aton("eat")) {
                    // eat
                    get_agent_at(a).give_reward(-EAT_ENERGY);
                    agent_eats(a,num_food);
                } else if(action==-1) {
#ifdef DEBUG
                    std::cout<<"Agent "<<get_agent_at(a).get_ID()<<" in field "<<fieldID<<" is dead, code: "<<action<<std::endl;
#endif //DEBUG
                } else
                    std::cout<<"Warning: unknown action"<<std::endl;
                // record action
                int x = get_field_at(a)%field_size_;
                int y = (int)get_field_at(a)/field_size_;
                str_stats<<timeStep<<","<<get_agent_at(a).get_ID()<<","<<x<<","<<y<<","<<(int)get_agent_at(a).get_energy()<<","<<action<<","<<get_agent_at(a).get_seed();
                // test the agent and record its performance
                Genome::actions_type result = get_agent_at(a).test_agent();
                assert(!result.empty());
                for(unsigned int i=0;i<result.size();i++){
                    str_stats<<","<<result[i];
                }
                str_stats<<std::endl;
            }
        return str_stats.str();
    }

    void Population::agent_moves(population_type &a,int orientation) //{NORTH=0,WEST=1,EAST=2,SOUTH=3}
    {
        int fieldID = get_field_at(a);
        assert(fields_[fieldID].get_num_agents()>0);
        std::vector<int> neighbors = get_neighboring_cells(1,fieldID,field_size_,field_size_*field_size_); // indexes of neighbors
        std::vector<int> idxs = {1,3,5,7};
        int dest_idx = neighbors[idxs[orientation]]; // moves in the direction defined by orientation
        // update field reference in population
        a.second=dest_idx;
        // decrease origin counter
        fields_[fieldID].rem_agent();
        // increase destination counter
        fields_[dest_idx].add_agent();
        /// if direct_feedback enabled then add ENTER_ENERGY to the agent when he enters the cell!
        if(direct_feedback_){
          if(fields_[get_field_at(a)].get_food()>0){
            get_agent_at(a).give_reward(ENTER_ENERGY);
          }
        }
#ifdef DEBUG
        std::string ori = Agent::ntoa(orientation);
        std::cout<<"Agent "<<get_agent_at(a).get_ID()<<" moves "<<ori<<" from field "<<fieldID<<" to field "<<dest_idx<<" and neighbors are "<<neighbors[idxs[0]]<<", "<<neighbors[idxs[1]]<<", "<<neighbors[idxs[2]]<<", "<<neighbors[idxs[3]]<<std::endl;
#endif //DEBUG
    }

    void Population::agent_eats(population_type &a,size_t num_food)
    {
        bool is_food = fields_[get_field_at(a)].get_food()>0;
#ifdef DEBUG
        std::cout<<"Agent "<<get_agent_at(a).get_ID()<<" with energy "<<get_agent_at(a).get_energy()<<" eats "<<(is_food?"some food":"nothing")<<" from field "<<get_field_at(a)<<" with food "<<fields_[get_field_at(a)].get_food()<<" and agents "<<fields_[get_field_at(a)].get_num_agents();;
#endif //DEBUG
        if(is_food) {
            get_agent_at(a).give_reward(FOOD_ENERGY);
            fields_[get_field_at(a)].consume_bundle();
#ifdef DEBUG
            std::cout<<", remaining food "<<fields_[get_field_at(a)].get_food();
#endif //DEBUG
        }
        if(is_food && fields_[get_field_at(a)].get_food()<=0) { // if the food resource is empty spawn a new one
            fields_[get_field_at(a)].set_initial_food(0); // reset the initial food variable
            if(num_food<=f0_) {
                create_food_source(fields_,1,fmax_,fields_[get_field_at(a)].get_initial_food());
#ifdef DEBUG
                size_t food_sum=0;
                std::cout<<"fields with food: ";
                for(size_t i=0; i<fields_.size();i++) {
                    if(fields_[i].get_food()!=0){
                        std::cout<<i<<":"<<fields_[i].get_food()<<", ";
                        food_sum++;
                    }
                }
                std::cout<<"sum: "<<food_sum<<std::endl;
                assert(food_sum==f0_);
#endif //DEBUG
            }

        }
#ifdef DEBUG
        std::cout<<", new energy level is "<<get_agent_at(a).get_energy()<<std::endl;
#endif //DEBUG
    }

    Agent Population::agent_replicates(Agent &a)
    {
        Agent offspring(-9);
        if(a.get_energy()>0) {
#ifdef DEBUG
            std::cout<<"Agent "<<a.get_ID()<<" with energy "<<a.get_energy()<<" replicates in field "<<f;
#endif //DEBUG
            offspring = a.replicate(agent_counter_++);
            offspring.set_energy(a.get_energy()/2.0);
            a.set_energy(a.get_energy()/2.0);             // half energy of self
#ifdef DEBUG
            std::cout<<" to offspring "<<offspring.get_ID()<<", new energy of agent "<<a.get_energy()<<" and offspring "<<get_agent_at(population_.end()-1).get_energy()<<std::endl;
        } else {
            std::cout<<"Agent "<<a.get_ID()<<" with energy "<<a.get_energy()<<" has not enough energy to replicate"<<std::endl;
#endif //DEBUG
        }
        return offspring;
    }

    void Population::del_agent(std::vector<population_type>::iterator &it)
    {
#ifdef DEBUG
        std::cout<<"deleting agent "<<get_agent_at(*it).get_ID()<<std::endl;
#endif //DEBUG
        std::swap(*it , *(population_.end()-1)); // swap the element with the last element
        population_.pop_back();
        it--;                   // go back one step as the element at the iterator has not been processed yet
    }

    void Population::shuffle_agents()
    {
        std::random_shuffle(population_.begin(),population_.end());
    }

    /* returns a vector of indexes of cells in the Moore neighborhood of the cell at index, including the cell at index
     * (0,1,2
     *  3,I,5
     *  6,7,8)
     */
    std::vector<int> Population::get_neighboring_cells(int radius, int index,int grid_size, int num_cells){
        std::vector<int> neighbors;
        for (int y = -radius; y <= radius; y++) {
            for (int x = -radius; x <= +radius; x++) {
                int round = (int)(index/grid_size*grid_size); // first element in the row
                neighbors.push_back((index+x+grid_size)%grid_size+ // periodic boundaries in the row
                                    (round+y*grid_size+num_cells)%num_cells); // periodic boundaries in the column
            }
        }
        return neighbors;
    }

    /* takes the references to the cells around the agent in a range fov_radius and groups them in 5 vectors that represent the cardinal directions + current cell
     * | F | F | F | F | F |
     * | L | F | F | F | R |
     * | L | L | H | R | R |
     * | L | B | B | B | R |
     * | B | B | B | B | B |
     * the result vector contains
     * cells-ahead, cells-left, cells-here, cells-right, cells-back,
     */
    std::vector<std::vector<int>> Population::field_of_view(int fov_radius, int index, int grid_size, int num_cells)
    {
        std::vector<int> neighs = get_neighboring_cells(fov_radius,index,grid_size,num_cells);
        int matrix_dim=fov_radius*2+1;
        std::vector<std::vector<int>> result(5);
        for(int y=0;y<matrix_dim;y++)
            for(int x=0;x<matrix_dim;x++)
                {
                    if(x==y && x==(matrix_dim-1-y)){ // central cell
                        result[2].push_back(neighs[y*matrix_dim+x]);
                    } else if (x>=y && x<=(matrix_dim-1-y)) { // north
                        result[0].push_back(neighs[y*matrix_dim+x]);
                    } else if (x<=y && x>=(matrix_dim-1-y)) { // south
                        result[4].push_back(neighs[y*matrix_dim+x]);
                    } else if (x>y && x>(matrix_dim-1-y)) { // east
                        result[3].push_back(neighs[y*matrix_dim+x]);
                    } else {    // west
                        result[1].push_back(neighs[y*matrix_dim+x]);
                    }
                }
        return result;
    }

    /* returns a Genome (vector<double>) that contains for each of the visible 5 neighbors (including the current field) the amount of food and the number of agents
     * food-north, food-west, food-here, food-east, food-south
     * if INTERACT is defined the vector also contains:
     * agents-north, agents-west, agents-here, agents-east, agents-south
     * if INVISIBLE_FOOD is defined the vector looks like:
     * agents-north, agents-west, FOOD-here, agents-east, agents-south
     */
    Genome::perception_type Population::compute_agent_perceptions(int fieldID) // TODO convert the result type into a hashmap?
    {
        Genome::perception_type perceptions; // ignore input1
        std::vector<std::vector<int> > neighbors = field_of_view(fov_radius_,fieldID,field_size_,field_size_*field_size_); // indexes of neighbors
        int counter=0;
#ifndef INVISIBLE_FOOD
        // fill in inputs 0 to 4: food
        for(auto &v:neighbors) {
            counter=0;
            for(auto &f:v)
                counter+=fields_[f].get_food();
            perceptions.push_back(counter);
        }
#endif //INVISIBLE_FOOD
#ifdef INTERACT
        // fill in inputs 5 to 9: agents
        for(auto &v:neighbors) {
            counter=0;
#ifdef FILTER_STATIC_PERCEPTIONS
    // Consider only static agents (eating)
            for(auto &f:v) {
                if(fields_[f].get_food()>0) // Only if agents are eating
                    counter+=fields_[f].get_num_agents();
            }
#else // FILTER_STATIC_PERCEPTIONS
    // Consider all agents
            for(auto &f:v) {
                counter+=fields_[f].get_num_agents();
            }
#endif // FILTER_STATIC_PERCEPTIONS
                perceptions.push_back(counter);
        }
#ifdef INVISIBLE_FOOD
        // substitute information about agents in the current cell with info about food
        counter=0;
        for(auto &f:neighbors[2])
            counter+=fields_[f].get_food();
        perceptions[2]=counter;
#elif !defined INVISIBLE_FOOD
        perceptions[7]-=1;      // do not count the agent in the current cell
        assert(perceptions[7]>=0);
#endif //INVISIBLE_FOOD
#endif //INTERACT
        //// ---- some debug code ----
#ifdef DEBUG
#ifdef INVISIBLE_FOOD
#ifdef INTERACT
        std::cout<<"Perceptions: agent_north="<<perceptions[0]<<" agent_west="<<perceptions[1]<<" food_here="<<perceptions[2]<<" agent_east="<<perceptions[3]<<" agent_south="<<perceptions[4]<<std::endl;
#endif // INTERACT
#elif !defined INVISIBLE_FOOD
#ifdef INTERACT
        std::cout<<"Perceptions: food_north="<<perceptions[0]<<" food_west="<<perceptions[1]<<" food_here="<<perceptions[2]<<" food_east="<<perceptions[3]<<" food_south="<<perceptions[4]<<"agent_north="<<perceptions[5]<<" agent_west="<<perceptions[6]<<" agent_here="<<perceptions[7]<<" agent_east="<<perceptions[8]<<" agent_south="<<perceptions[9]<<std::endl;
#elif !defined INTERACT
        std::cout<<"Perceptions: food_north="<<perceptions[0]<<" food_west="<<perceptions[1]<<" food_here="<<perceptions[2]<<" food_east="<<perceptions[3]<<" food_south="<<perceptions[4]<<std::endl;
#endif // INTERACT
#endif //INVISIBLE_FOOD
#endif  // debug
        return perceptions;
    }

    std::string Population::print_pop(std::vector<population_type> pop) {
        std::ostringstream retval;
        std::cout<<"Testing population of "<<pop.size()<<" agents"<<std::endl;
        long int avg=0;
        for(auto i=pop.begin();i!=pop.end();i++) {
            std::cout<<"agent "<<get_agent_at(i).get_ID()<<" has energy "<<get_agent_at(i).get_energy()<<std::endl;
            avg+=get_agent_at(i).get_energy();
            retval<<get_agent_at(i).get_ID()<<","<<get_agent_at(i).get_energy()<<","<<get_agent_at(i).get_seed()<<std::endl;}
        std::cout<<"Population avg energy: "<<avg/(double)pop.size()<<std::endl;
        std::vector<std::vector<double> > stats = write_pop(pop);
        std::cout<<"Avg population performance= "<<std::endl;
        for(size_t i=0;i<stats[0].size();i++)
            std::cout<<stats[0][i]<<" ("<<std::sqrt(stats[1][i])<<") "<<stats[2][i]<<std::endl;
        std::cout<<std::endl<<"Values are: 0=north, 1=west, 2=east, 3=south, 4=eat"<<std::endl;;
        return retval.str();
    }

    std::string Population::print_pop(int seed){
        std::vector<population_type> temp_pop;
        std::for_each(population_.begin(),population_.end(),[this,seed,&temp_pop](population_type a)
                 {
                     if(get_agent_at(a).get_seed()==seed)
                         temp_pop.push_back(a);
                 });
        return print_pop(temp_pop);
    }

    std::vector<std::vector<double>> Population::write_pop(std::vector<population_type> pop) {
        std::vector<std::vector <double> > result;
        int lng = N_PERCEPTIONS;
#ifndef INTERACT
        Genome::actions_type corrects={Agent::aton("eat"),
                                       Agent::aton("north"),
                                       Agent::aton("west"),
                                       Agent::aton("east"),
                                       Agent::aton("south")};
#elif defined INTERACT
#ifdef INVISIBLE_FOOD
        Genome::actions_type corrects={Agent::aton("eat"),
                                       Agent::aton("north"),
                                       Agent::aton("west"),
                                       Agent::aton("east"),
                                       Agent::aton("south")};
#elif !defined INVISIBLE_FOOD
        Genome::actions_type corrects={Agent::aton("eat"),
                                       Agent::aton("north"),
                                       Agent::aton("west"),
                                       Agent::aton("east"),
                                       Agent::aton("south"),
                                       Agent::aton("eat"),
                                       Agent::aton("north"),
                                       Agent::aton("west"),
                                       Agent::aton("east"),
                                       Agent::aton("south")};
#endif
#endif
        std::vector<double> sums(lng,0);
        std::vector<double> errors(lng,0);
        for(auto i=pop.begin();i!=pop.end();i++)
            sums+=get_agent_at(i).test_agent();
        sums=sums/(double)pop.size();
        for(auto i=pop.begin();i!=pop.end();i++)
            errors+=(sums-get_agent_at(i).test_agent())*(sums-get_agent_at(i).test_agent());
        errors=errors/(double)pop.size();

        result.push_back(sums);
        result.push_back(errors);
        result.push_back(corrects);
        return result;
    }

    void Population::initialize_fields() {
        for (size_t i = 0; i < field_size_*field_size_; i++) {
            fields_.push_back(Field());
        }

#ifdef DEBUG
        std::cout<<"Debugging fields..."<<std::endl;
        for(auto &f : fields_) {
            assert(&f!=NULL);    // object has been created
            assert(f.get_num_agents()==0); // contains 0 agents
            assert(f.get_food()==0);       // contains 0 food
        }
#endif //DEBUGGING

        // spawn initial food
        for( size_t i = 0; i < f0_; ++i ){
            create_food_source(fields_,1,fmax_,0); // no init food, pick type at random
        }
#ifdef DEBUG
        size_t food_sum=0;
        for(auto &f : fields_) {
            if(f.get_food()!=0)
                food_sum++;
        }
        assert(food_sum==f0_);
        std::cout<<"done!"<<std::endl;
#endif //DEBUG
    }

    void Population::initialize_population() {
        population_.reserve(n0_);
        std::uniform_int_distribution<int> fields_index_distribution(0,field_size_*field_size_-1);
        // TODO put into function
        for (size_t i = 0; i < n0_; i++) {
            Agent ag = Agent(agent_counter_++);
            size_t idx = fields_index_distribution(rng);    // place in random cell
            population_.push_back(population_type(ag,idx)); // add to list
            assert(idx<fields_.size());
            fields_[idx].add_agent();
        }
#ifdef DEBUG
        for_each(population_.begin(),population_.end(),[this](population_type a){std::cout<<"Agent "<<get_agent_at(a).get_ID()<<" in field "<<get_field_at(a)<<std::endl;});
        std::cout<<"debugging population...";
        assert(std::accumulate(fields_.begin(),
                               fields_.end(),
                               0,
                               [](int &a, Field &b){return a+=b.get_num_agents();}) == n0_); // num of agents if fields is consistent with number of agents
        std::vector<int>counts(field_size_*field_size_,0);
        for(auto & a : population_) {
            counts[get_field_at(a)]++;
        }
#endif //DEBUG
    }

    // }
    void Population::reproduction() {
        /**
           Choose agents that reproduce at this timestep using the roulette wheel selection mechanism with stochastic acceptance:
           The probability of reproducing is the ratio between the individual fitness and ENERGY_MAX
         */
        // Get the fitness of all individuals
        int max_energy_=max_age_*0.7;
        if(population_.size()==0) { // generate a new population
#ifdef RESPAWN
            std::cout<<"Warning: all agents are dead, spawning a new population"<<std::endl;
            initialize_population();
            seed_population();
#endif
            // exit(666);
        }else {
            std::uniform_real_distribution<double> dist01(0,1);
            std::vector<population_type> offsprings;
            // Separate population and offsprings to avoid trouble with iterators: the iterators get invalidated when the population is resized
            for(auto &a : population_){
                float weight=((float)get_agent_at(a).get_energy()/max_energy_);
                if(dist01(rng)<weight){ // accept with probability proportional to age and inversely proportional to fitness
#ifdef DEBUG
                    std::cout<<"Agent "<<get_agent_at(a).get_ID()<<" with energy "<<get_agent_at(a).get_energy()<<" reproduces"<<std::endl;
#endif
                    Agent offspring=agent_replicates(get_agent_at(a));
                    if(offspring.get_ID()!=-9){
                        offsprings.push_back(population_type(offspring,get_field_at(a))); // add agent to list
                    }else{
                        std::cout<<"Warning: invalid offspring"<<std::endl;
                    }
                }
            }
            for(auto &a: offsprings){ // merge with population, loop is on offspring vector which does not get resized
                population_.push_back(a);
                fields_[get_field_at(a)].add_agent(); // increase the a counter
            }
        }
    }

    void Population::remove_dead_agents() {
        /**
           Remove agents using the roulette wheel selection algorithm via stochastic acceptance.
           Agents are drawn uniformly at random. Agents with higher age and lower energy are more likely to be killed.
           The number of agents removed is stochastic and between 1 and population.size*FRACTION_DEATHS
         */
        //remove dead agent
        population_.erase(std::remove_if(population_.begin(), population_.end(),
                                         [this](Population::population_type &a) {
                                             bool dead=get_agent_at(a).is_dead();
                                             if(dead){
                                                 std::cout<<"removing zombie "<<get_agent_at(a).get_ID()<<" with energy "<<get_agent_at(a).get_energy()<<" and age "<<get_agent_at(a).get_age()<<std::endl;
                                                 fields_[get_field_at(a)].rem_agent(); // remove the agent from the field
                                             }
                                             return dead;
                                         }), population_.end());
        size_t nkills=(int)(FRACTION_DEATHS*population_.size());
        if(nkills==0)
            nkills=population_.size(); // test all the population
#ifdef DEBUG
        std::cout<<"Population has size "<<population_.size()<<". Removing "<<nkills<<" agents from the population"<<std::endl;
#endif
        std::uniform_real_distribution<double> dist01(0,1);
        shuffle_agents();             // shuffle population
        // We will be deleting agents one after the other, so the population gets progressively reduced. We cannot use a integer to identify the current agent, as we would be skipping over some agents, so we use an iterator instead (the function del_agent deals with keeping the iterator valid after a removal)
        // test the remaining agents
        std::vector<population_type>::iterator idx=population_.begin();
        for(size_t i=0;i<nkills;i++){ // pick the first nkills agents
            assert(idx!=population_.end());
            float weight=((float)get_agent_at(idx).get_age()/max_age_);
            if(dist01(rng)<weight){ // accept with probability proportional to age and inversely proportional to fitness
#ifdef DEBUG
                std::cout<<"Agent "<<get_agent_at(idx).get_ID()<<" with energy "<<get_agent_at(idx).get_energy()<<" and age "<<get_agent_at(idx).get_age()<<" is removed from the population"<<std::endl;
#endif
                fields_[get_field_at(idx)].rem_agent(); // remove the agent from the field
                del_agent(idx);  // remove the agent from the population
            }
            idx++;              // move to the next agent
        }
    }

    void Population::debug_step(int fieldID, Genome::perception_type perceptions) {
        // check that the number of agents seen is correct
        std::vector<std::vector<int> > neighbors = field_of_view(fov_radius_,fieldID,field_size_,field_size_*field_size_); // indexes of neighbors
        std::vector<int>::iterator it;
        std::vector<int> sums (neighbors.size(),0);
        std::vector<int> foods (neighbors.size(),0);
        for(auto & b : population_) { // count agents
            for (size_t i=0;i<neighbors.size();i++) {
                it = std::find(neighbors[i].begin(),neighbors[i].end(),get_field_at(b));
                if(it!=neighbors[i].end()) // if the agent is in one of the visible cells
                    sums[i]++;
            }
        }
        sums[2]--;      // do not count the current agent in the current cell

        for(size_t i=0;i<neighbors.size();i++)
            for(auto &v:neighbors[i]) // count food
                foods[i]+=fields_[v].get_food();
#ifdef INVISIBLE_FOOD
        assert(perceptions[2]==foods[2]); // check that the perceived food is real
#ifdef INTERACT
        for (size_t i = 0; i < neighbors.size(); i++)
            if(i!=2){                             // food here
                assert__(perceptions[i]==sums[i]) { // check that the perceived agents are real
                    std::vector<int> visible_cells = get_neighboring_cells(fov_radius_,fieldID,field_size_,field_size_*field_size_); // indexes of neighbors
                    std::cout<<"Cell "<<visible_cells[i]<<" contains "<<sums[i]<<" agents but "<<perceptions[i+1]<<" are perceived"<<std::endl;
                }}
#endif //INTERACT
#elif !defined INVISIBLE_FOOD
        for (size_t i = 0; i < neighbors.size(); i++) {
            assert(perceptions[i]==foods[i]); // check that the perceived food is real
#ifdef INTERACT
            assert(perceptions[i+neighbors.size()]==sums[i]); // check that the perceived agents are real
#endif //INTERACT
        }
#endif //INVISIBLE_FOOD
    }

    void Population::seed_population() {
        //// Initialize population with different initial values
        // TODO make it depend on whether the parameters (anti)social_ratio are set
#ifdef SEED_FORAGING
        for(size_t z=0;z<population_.size();z++){
            get_agent_at(z).seed_foraging(SEED_VALUE);
        }
#endif
        size_t social_start=0;
        size_t social_end=population_.size()*social_ratio_;
        size_t antisocial_start=social_end;
        size_t antisocial_end=social_end+population_.size()*antisocial_ratio_;

        for(size_t z=social_start;z<social_end;z++)
            get_agent_at(z).seed_social(SEED_VALUE);
        for(size_t z=antisocial_start;z<antisocial_end;z++)
            get_agent_at(z).seed_antisocial(SEED_VALUE);
    }

    void Population::create_food_source(std::vector<field_type> &fields,int x0,int x1,int init_food) {
        std::uniform_int_distribution<int> fields_index_distribution(0,fields.size()-1);
        std::uniform_int_distribution<int> initial_food_quantity(x0,x1);
        int inc=0;
        ///////////////////////////
        // Uniformly distributed //
        ///////////////////////////
#ifdef FOOD_UNIFORM
        // we generally use this
        inc=initial_food_quantity(rng);
#endif
        // TODO check if it makes any difference
        ///////////////
        // Power law //
        ///////////////
#ifdef FOOD_POWER               // TODO check for bugs
        std::uniform_real_distribution<double> dist01(0,1);
        double rand01=dist01(rng);
        double n = 0.01;
        inc=std::pow((std::pow(x1,n+1)-std::pow(x0,n+1))*rand01+std::pow(x0,n+1),1/(n+1));
#endif
        ///////////////////
        // Bimodal distr //
        ///////////////////
#ifdef FOOD_BIMODAL
        std::uniform_real_distribution<double> dist01(0,1);
        std::uniform_int_distribution<int> initial_food_quantity_low(1,5);
        std::uniform_int_distribution<int> initial_food_quantity_high(x1/2,x1);
        if(init_food==0) {      // there was no food previously, pick a type at random
            if(dist01(rng)<=FOOD_BIMODAL_PROB)
                inc=initial_food_quantity_low(rng);
            else
                inc=initial_food_quantity_high(rng);
        } else if(init_food<x1/2) // previously a poor field, spawn little food
            inc=initial_food_quantity_low(rng);
        else                    // previously a rich field, spawn much food
            inc=initial_food_quantity_high(rng);
#endif
        // ------------------------------

        int idx=fields_index_distribution(rng);
        while(fields[idx].get_food()!=0) // find a new field if it already contains food
            idx=fields_index_distribution(rng);
#ifdef DEBUG
        std::cout<<": creating a new food source in field "<<idx<<" of value "<<inc<<std::endl;
#endif //DEBUG
        fields[idx].inc_food(inc);
        fields[idx].set_initial_food(inc);
    }
} // end namespace
