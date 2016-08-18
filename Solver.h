#if 0
Defining Grid Parameters
#endif

#define GRID 1

#if GRID == 1
#define SOLVER_GRID_UG

#else
#define SOLVER_GRID_NON_UG

#endif

#if 0
Defining Poisson Solver Parameters
#endif

#define POIS_SOLVER 2

#if POIS_SOLVER == 1
#define POISSON_SOLVER_JACOBI
#endif

#if POIS_SOLVER == 2
#define POISSON_SOLVER_GS
#endif

#if POIS_SOLVER == 3
#define POISSON_SOLVER_GSOR
#define omega 1.1
#endif

#if 0
Defining Temperature Solver
#endif

#define TEMP_SOLVER 2

#if TEMP_SOLVER == 1
#define TEMP_SOLVER_CENTRAL
#endif

#if TEMP_SOLVER == 2
#define TEMP_SOLVER_UPWIND
#endif

#if 0
Defining Simulation Parameters - Block Size, Domain Length, etc
#endif

#define MAX_STRING_LENGTH 80


#define Nxb 100
#define Nyb 100


#define D_xmin -0.5
#define D_ymin -0.5


#define D_xmax 0.5
#define D_ymax 0.5


#define HK 2
#define HD 2
 

#define MaxIt 1500


#define PRES_VAR 1
#define TEMP_VAR 2
#define DFUN_VAR 3
#define PFUN_VAR 4
#define THCO_VAR 5
#define CPRS_VAR 6

#define CENT_VAR 6

#define VELC_VAR 1
#define FACE_VAR 1


#if 0
Defining Flow Type
#endif

#define FLOW 1

#if FLOW == 1
#define LID_DRIVEN_FLOW
#endif

#if FLOW == 2
#define CHANNEL_FLOW
#endif

#if FLOW == 3
#define CYLINDER_FLOW
#endif

#if 0
Multiphase On/Off
#endif

#define MPH 0

#if MPH == 1
#define MULTIPHASE
#endif

#if MPH == 0
#define SINGLEPHASE
#endif


