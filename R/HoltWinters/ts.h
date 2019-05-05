/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-12   The R Core Team.
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
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_TS_H
#define R_TS_H
#include <Rinternals.h>
#include "stats.h"

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
		  
    );
		  
void

#endif
