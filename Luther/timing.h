/* Timing (by Alexander Jean-Claude Bottema)
   C version written 1992-09-10

   Example:

   #include "timing.h"

   int main()
   {
       int catch;

       initialize_timer();

       reset_timer();
       !!! Algorithm !!!
       catch = get_timer();

       printf("%d ms.\n", catch );
       
       return 0;
   }
   
*/

#include <sys/time.h>
#include <stdio.h>

struct itimerval alex_timer;
struct itimerval alex_timer_saved;

void initialize_timer()
{
    alex_timer.it_value.tv_sec = 1000000;
    alex_timer.it_value.tv_usec = 0;
    alex_timer.it_interval.tv_sec = 0;
    alex_timer.it_interval.tv_usec = 1;

    alex_timer_saved = alex_timer;

    setitimer( ITIMER_VIRTUAL, &alex_timer, NULL );
}

/* ----------------------------------------------------------------------
    Reset timer to 0
   ---------------------------------------------------------------------- */

void reset_timer()
{
    getitimer( ITIMER_VIRTUAL, &alex_timer );
    alex_timer_saved = alex_timer;
}

/* ----------------------------------------------------------------------
    Return timer value in milliseconds
   ---------------------------------------------------------------------- */

int get_timer()
{
    getitimer( ITIMER_VIRTUAL, &alex_timer );
    return (alex_timer_saved.it_value.tv_sec -
	    alex_timer.it_value.tv_sec) * 1000 +
           (alex_timer_saved.it_value.tv_usec -
	    alex_timer.it_value.tv_usec) / 1000;
}








