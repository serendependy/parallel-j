#include <stdlib.h>
#include <omp.h>

/* data structure for two-dimensional array */
typedef struct twoD_array {
    int rows;
    int cols;
    int ** elems;
} twoD_array_t;

void update_board(twoD_array_t *board, twoD_array_t *new_board);

/*
 * updates board configuration
 */
void update_board(twoD_array_t *board, twoD_array_t *new_board) {
    int i, j;
   
    #pragma omp parallel for private(j) schedule(static) 
    for (i = 1; i <= board->rows-2; ++i) {
    for (j = 1; j <= board->cols-2; ++j) {
        int neighbs = 0;
        int mycell = board->elems[i][j];
        int k, l;
	    
	    /*count neighbors*/
	    for (k = i - 1; k <= i+1; ++k) {
    	for (l = j - 1; l <= j+1; ++l) {
            if (!(k == i && l == j) ) {
                neighbs += board->elems[k][l];
            }
        }
        }

	    /*Logic of game*/
	    if (mycell) {
            if (!(neighbs == 2 || neighbs == 3))
                mycell = 0;
	    }

        else {
            if (neighbs == 3)
                mycell = 1;
        }

	    /*Update board*/
        new_board->elems[i][j] = mycell;
    }
    }
}
