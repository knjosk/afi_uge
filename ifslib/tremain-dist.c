static char USMID[] = "@(#) libu/util/tremain.c 91.1    07/13/97 10:45:33";
/*
 *      (C) COPYRIGHT CRAY RESEARCH, INC.
 *      UNPUBLISHED PROPRIETARY INFORMATION.
 *      ALL RIGHTS RESERVED.
 */
#include <time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/times.h>

/*
 *      TREMAIN - Returns the CPU time remaining for current process in seconds.
 *
 *      Returns 0 after time limit has been exceeded, returns a large number
 *      if no limits are in effect.
 */

void
tremain_(cpu_left)
double *cpu_left;
{
        int res;
        extern long limit(), times();
        double  proc_limit, cpu_used, ptime_left;
        struct tms b_1;
        struct rlimit rlp;

        res = getrlimit(RLIMIT_CPU, &rlp);
        proc_limit = rlp.rlim_cur;
        if (proc_limit > 0.) {
                /* Get the cpu time left for this process */
                (void) times(&b_1);
                cpu_used = (double) (b_1.tms_utime + b_1.tms_stime) / (double) CLK_TCK;
                ptime_left = proc_limit - cpu_used;
        }
        else
                ptime_left = 315576000.;


        if (ptime_left < 0) {
                /* CPU time has exceeded  */
                ptime_left = 0;
        }

        *cpu_left = ptime_left;

        return;
}
