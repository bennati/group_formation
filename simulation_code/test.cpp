/*
Stefano Bennati, Leonar Wossnig, Johannes Thiele. 2017.
 */

#include "population.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdexcept>
#include <unistd.h>             // getpid()

//MPI
#include <mpi.h>

//========= some macros for nicer presentation (not essential) =========
//use as litte macros as possible in c++ (most stuff can be solved without)
#define TYPE(T) typeid(T).name()
#define CLR_SCR() std::cout << "\033[2J\033[100A";
#define NEW_LINE() std::cout << std::endl;
#define WAIT_FOR_INPUT() while(std::cin.gcount() == 0) std::cin.get(); std::cin.get();
#define ASSERT_MSG(cond, msg) if(cond) {PRINT_RED(msg); throw std::runtime_error("error");}
#define PRINT_NAMED(x) std::cout << #x << " = " << x << std::endl; //#x changes the variable name into a string "x"
#define PRINT_RED(x) std::cout << "\033[1;31m" << x << "\033[0m" << std::endl;
#define PRINT_BLUE(x) std::cout << "\033[1;34m" << x << "\033[0m" << std::endl;
#define PRINT_CYAN(x) std::cout << "\033[1;36m" << x << "\033[0m" << std::endl;
#define PRINT_GREEN(x) std::cout << "\033[1;32m" << x << "\033[0m" << std::endl;
#define PRINT_YELLOW(x) std::cout << "\033[1;33m" << x << "\033[0m" << std::endl;
#define PRINT_MAGENTA(x) std::cout << "\033[1;35m" << x << "\033[0m" << std::endl;

void print_info();
long seedgen(void);

using namespace Joleste;

int main(int argc, char** argv)
{
    int rank(0), world_size(0);
    /* starts MPI */
    MPI_Init(&argc, &argv);
    /* get current process id */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    /* get number of processes */
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    if(argc != 14 )
    {
        if (rank == 0) {
            std::cout << "Usage: "<<argv[0]<<" num_agents amax amin num_food fmax num_runs samples max_age fov_radius binary_input direct_feedback social_ratio antisocial_ratio\n";
            std::cout<<"Example usage: ./test 20  2000 40 10 100 2000 10 100 1 0 0 0 0\n";
            std::cerr << "Not enough input arguments given! only "<<argc-1<< std::endl;
        }
        MPI_Finalize();
        return 0; //Avoids mpi_runtime complaining about error
    }

    #ifdef DEBUG
    PRINT_RED("Starting tests");
    #endif
    const int field_size = 20;
    const int num_agents      = std::stoi(argv[1]);
    const int amax            = std::stoi(argv[2]);
    const int amin            = std::stoi(argv[3]);
    int num_food        = std::stoi(argv[4]);
    const size_t fmax         = std::stoi(argv[5]);
    const int num_runs        = std::stoi(argv[6]);
    const int samples         = std::stoi(argv[7]);
    const int max_age         = std::stoi(argv[8]);
    int fov_radius      = std::stoi(argv[9]);
    const int binary_in       = std::stoi(argv[10]);
    const int direct_feedback = std::stoi(argv[11]);
    const double social_ratio  = std::stod(argv[12]);
    const double antisocial_ratio  = std::stod(argv[13]);

    PRINT_BLUE("Parameters are:");
    PRINT_GREEN("num_agents: "<<num_agents);
    PRINT_GREEN("amax      : "<<amax);
    PRINT_GREEN("amin      : "<<amin);
    PRINT_GREEN("num_food  : "<<num_food);
    PRINT_GREEN("fmax      : "<<fmax);
    PRINT_GREEN("num_runs  : "<<num_runs);
    PRINT_GREEN("samples   : "<<samples);
    PRINT_GREEN("max_age   : "<<max_age);
    PRINT_GREEN("fov_radius: "<<fov_radius);
    PRINT_GREEN("binary_in : "<<binary_in);
    PRINT_GREEN("direct_feedback: "<<direct_feedback);
    PRINT_GREEN("social_ratio: "<<social_ratio);
    PRINT_GREEN("antisocial_ratio: "<<antisocial_ratio);

    print_info();

    if (world_size != samples) {
        MPI_Finalize();
        if (rank == 0) std::cerr << " Not enough number of process, spawn #processes == samples " << std::endl;
        return 0;
    }
    if (fov_radius*2+1 > field_size){
        if (rank == 0)
            std::cerr << "# " << rank << " Warning visual radius larger than the domain\n reducing visual radius to domain size" << std::endl;
        fov_radius = (int)(((float)field_size - 1.0)/2.0); //if domain size is even, a column and row will be blinded
    }

    if (num_food > field_size*field_size){
        if (rank == 0)
            std::cerr << "# " << rank << " Warning more food than cells\n reducing number of food" << std::endl;
        num_food = field_size*field_size; //if domain size is even, a column and row will be blinded
    }

    std::stringstream report_filename;
    std::stringstream filename_agents;

    report_filename << "./results/report" << rank << ".csv";
    auto tmp=report_filename.str();
    std::ifstream repfile(tmp.c_str());
    if(repfile.good()){
        std::cout<<"Deleting previous report file "<<tmp<<std::endl;
        remove(tmp.c_str());
    }
    filename_agents << "./results/stats" << rank << ".csv";
    tmp=filename_agents.str();
    std::ifstream statfile(tmp.c_str());
    if(statfile.good()){
        std::cout<<"Deleting previous statistics file "<<tmp<<std::endl;
        remove(tmp.c_str());
    }

    PRINT_MAGENTA("Running sample " << rank+1 << " of " << world_size << " Saving results in " << report_filename.str() << " , " << filename_agents.str());

    MPI_Barrier(MPI_COMM_WORLD);
#ifdef DEBUG
    long seed=rank;
#else
    long seed=seedgen();
#endif
    std::cout<<"Seeding process "<<rank<<" with seed "<<seed<<std::endl;
    rng.seed(seed);

    std::stringstream out;
    if(rank==0)
        out<<"agent.id,energy,seed"<<std::endl;

    Population pop(amax,amin,num_agents,num_food,fmax,field_size, max_age, fov_radius,binary_in, direct_feedback,social_ratio,antisocial_ratio);
    double start=0,end=0;
    start= MPI_Wtime();
    pop.simulate(num_runs,filename_agents.str());
    end= MPI_Wtime();
    PRINT_RED("# " << rank << " Sample finished... Time = "<<(end-start));

    std::cout<<"normals"<<std::endl;
    out<<pop.print_pop(0);
    std::cout<<"socials"<<std::endl;
    out<<pop.print_pop(1);
    std::cout<<"antisocials"<<std::endl;
    out<<pop.print_pop(2);
    std::cout<<"After loop iteration "<<rank<<std::endl;

    std::ofstream report;
    report.open(report_filename.str());         //Opening file to print info to
    report<<out.str();
    report.flush();
    report.close();
    MPI_Barrier(MPI_COMM_WORLD); // wait until all files are written
    MPI_Finalize();
    return 0;
}
void print_info(){
    int numprocs, rank, namelen;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int iam = 0, np = 1;

    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Get_processor_name(processor_name, &namelen);
    printf("Process %d out of %d on %s\n",
           rank, numprocs, processor_name);
}

//Random seed generator from Helmut G. Katzgraber, Random Numbers in Scientific Computing: An Introduction, arXiv:1005.4117v1
long seedgen(void){
    long s, seed, pid;
    pid = getpid();
    s = time(NULL); /* get CPU seconds since 01/01/1970 */
    seed = labs(((s*181)*((pid-83)*359))%104729);
    return seed;
}
