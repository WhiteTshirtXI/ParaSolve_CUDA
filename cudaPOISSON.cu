#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include "Solver.h"

#define BLOCKS 1
#define THREADS 20

// poisson solver

__global__ void solve_poisson(float ps[Nxb+2][Nyb+2],float ps_old[Nxb+2][Nyb+2],float gr_dx_centers[Nxb+2][Nyb+2],float gr_dy_centers[Nxb+2][Nyb+2],
                         float gr_dx_nodes[Nxb+2][Nyb+2],float gr_dy_nodes[Nxb+2][Nyb+2],float ps_RHS[Nxb][Nyb])
{
   int i,j;

   j = (blockIdx.x * blockDim.x) + threadIdx.x;
   i = (blockIdx.y * blockDim.y) + threadIdx.y;

  if(i>=1 && i<=Nxb && j>=1 && j<=Nyb) 

  {

   ps[i][j]=((ps_old[i][j+1]/(gr_dy_centers[i][j]*gr_dy_nodes[i][j]))+(ps[i][j-1]/(gr_dy_nodes[i][j]*gr_dy_centers[i-1][j-1]))
            +(ps_old[i+1][j]/(gr_dx_centers[i][j]*gr_dx_nodes[i][j]))+(ps[i-1][j]/(gr_dx_nodes[i][j]*gr_dx_centers[i-1][j-1]))
           +ps_RHS[i-1][j-1])
           *(1/((1/(gr_dx_nodes[i][j]*gr_dx_centers[i-1][j-1]))+(1/(gr_dy_nodes[i][j]*gr_dy_centers[i-1][j-1]))+
                (1/(gr_dx_nodes[i][j]*gr_dx_centers[i][j]))+(1/(gr_dy_nodes[i][j]*gr_dy_centers[i][j]))));

   }
   
}

// function called from main fortran program
extern "C" void poisson_kernel_wrapper_(float *ps,float *ps_old,float *gr_dx_centers,float *gr_dy_centers,float *gr_dx_nodes,float *gr_dy_nodes,float *ps_RHS)
{
   float  *ps_d,*ps_old_d,*gr_dx_centers_d,*gr_dy_centers_d,*gr_dx_nodes_d,*gr_dy_nodes_d,*ps_RHS_d; // declare GPU vector copies

   dim3 numBlocks(BLOCKS,BLOCKS);
   dim3 threadsPerBlock(THREADS,THREADS);

   // Allocate memory on GPU
   cudaMalloc( (void **)&ps_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&ps_old_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&gr_dx_centers_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&gr_dy_centers_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&gr_dx_nodes_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&gr_dy_nodes_d, sizeof(float) * (Nxb+2) * (Nyb+2) );
   cudaMalloc( (void **)&ps_RHS_d, sizeof(float) * (Nxb) * (Nyb) );

   // copy vectors from CPU to GPU

   cudaMemcpy( ps_d, ps, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( ps_old_d, ps_old, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( gr_dx_centers_d, gr_dx_centers, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( gr_dy_centers_d, gr_dy_centers, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( gr_dx_nodes_d, gr_dx_nodes, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( gr_dy_nodes_d, gr_dy_nodes, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyHostToDevice );
   cudaMemcpy( ps_RHS_d, ps_RHS, sizeof(float) * (Nxb)*(Nyb), cudaMemcpyHostToDevice );

   // call function on GPU
   solve_poisson<<<numBlocks, threadsPerBlock>>>((float(*) [Nyb+2])ps_d,(float(*) [Nyb+2])ps_old_d,(float(*) [Nyb+2])gr_dx_centers_d,
                                            (float(*) [Nyb+2])gr_dy_centers_d,(float(*) [Nyb+2])gr_dx_nodes_d,(float(*) [Nyb+2])gr_dy_nodes_d,(float(*) [Nyb])ps_RHS_d);

   // copy vectors back from GPU to CPU

   cudaMemcpy(ps, ps_d, sizeof(float) * (Nxb+2)*(Nyb+2), cudaMemcpyDeviceToHost );

   // free GPU memory

   cudaFree(ps_d);
   cudaFree(ps_old_d);
   cudaFree(gr_dx_centers_d);
   cudaFree(gr_dx_nodes_d);
   cudaFree(gr_dy_centers_d);
   cudaFree(gr_dy_nodes_d);
   cudaFree(ps_RHS);

   return;
}

