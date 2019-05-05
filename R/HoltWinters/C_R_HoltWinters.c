/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 2003-7  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/.
 */

/* Originally contributed by David Meyer */
/*Modified for Robustness by Cameron Fincher */

#include <stdlib.h>
#include <string.h>  // memcpy
#include <math.h>

#include <R.h>
//#include "ts.h"



void R_HoltWinters (
		  double *x, /*as.double(x) */
		  double *x_adj, /*Adjust time series data, if need be                   Added*/
		  int    *xl, /*lenx - Length of the current time series*/
		  double *alpha, /*as.double(max(min(alpha, 1), 0)), */
		  double *beta, /*as.double(max(min(beta,1), 0)), */
		  double *gamma, /*as.double(max(min(gamma, 1), 0)), */ 
		  double *llamda,/*as.double(max(min(llamda,1),0)),                     ADDED*/
		  int    *start_time, /*as.integer(start.time),  */
		  int    *seasonal, /*as.integer(!+(seasonal == "multiplicative")), */
		  int    *period, /* as.integer(f),  */
		  int    *dotrend, /* as.integer(!is.logical(beta) || beta),  */
		  int    *doseasonal, /* as.integer(!is.logical(gamma) || gamma), */
		
		  double *a, /*l.start - starting values for level*/
		  double *b, /*b.start - starting values for Trend*/
		  double *s, /*s.start - starting values for Sesonal*/
		  double *l, /*t.start - starting values for LLamda                      ADDED*/
		  double *k, /* Value for K                                              ADDED*/
		  double *ck, /*value for ck                                             ADDED*/
		  double *theta, /*t.start - starting value for theta					 ADDED*/
		  /* return values */
		  double *SSE,
		  double *level,
		  double *trend,
		  double *season
		  
    )

{
    double res = 0, xhat = 0, stmp = 0, RhoK = 0, phi = 0 ;
    int i, i0, s0; /*i is the current t, i0 is the current LESS starting period, and s0 = is the seasonal current LESS Starting period*/

    /* copy start values to the beginning of the vectors */
    level[0] = *a;
    if (*dotrend == 1) trend[0] = *b;
    if (*doseasonal == 1) memcpy(season, s, *period * sizeof(double));

    for (i = *start_time - 1; i < *xl; i++) {
	/* indices for period i */
	i0 = i - *start_time + 2;
	s0 = i0 + *period - 1;
	//printf("   starting the for loop: looop %i \n",i0);
	/* forecast *for* period i */
	xhat = level[i0 - 1] + (*dotrend == 1 ? trend[i0 - 1] : 0);
	stmp = *doseasonal == 1 ? season[s0 - *period] : (*seasonal != 1);
	
	if (*seasonal == 1)
		{xhat += stmp;}
	else
		{xhat *= stmp;}
		
	/* Sum of Squared Errors */
	res = x[i] - xhat;
	/*adjusting for robustness....Gahds*/	
	RhoK = (abs(res / *theta) <= *k ? *ck * (1 - pow(1 - pow((res / (*k * *theta)),2),3)): *ck);
	*theta = sqrt(*llamda * RhoK * pow(*theta,2) + (1 - *llamda) * pow(*theta,2));
	phi = (abs(res / *theta) < *k ? res / *theta : (((res / *theta) / abs(res / *theta)) * (*k)));
	x_adj[i] = phi * *theta + xhat;

	//res = x_adj[i] - xhat;
	//printf("i: %i\tx: %f\tx_adj: %f\n", i0, x[i], x_adj[i]);
	*SSE += res * res;

	/* estimate of level *in* period i */
	if (*seasonal == 1)
	    level[i0] = *alpha       * (x_adj[i] - stmp)
		      + (1 - *alpha) * (level[i0 - 1] + trend[i0 - 1]);
	else
	    level[i0] = *alpha       * (x_adj[i] / stmp)
		      + (1 - *alpha) * (level[i0 - 1] + trend[i0 - 1]);

	/* estimate of trend *in* period i */
	if (*dotrend == 1)
	    trend[i0] = *beta        * (level[i0] - level[i0 - 1])
		      + (1 - *beta)  * trend[i0 - 1];

	/* estimate of seasonal component *in* period i */
	if (*doseasonal == 1) {
	    if (*seasonal == 1)
		season[s0] = *gamma       * (x_adj[i] - level[i0])
			   + (1 - *gamma) * stmp;
	    else
		season[s0] = *gamma       * (x_adj[i] / level[i0])
			   + (1 - *gamma) * stmp;
	}
    }
};
