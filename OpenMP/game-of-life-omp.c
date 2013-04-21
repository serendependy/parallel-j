/*
 * C program for Conway's ``game of life''.
 *
 * Input is from command line and a file:
 * 
 * Command-line arguments are as follows:
 *   either the name of an input file or the keyword "random" and the size
 *     of the board and the seed for random-number generation
 *   number of steps
 *   how often to print results (P means print results every P steps)
 *
 * If an input file is specified, it contains a representation of the initial 
 *  board configuration:  N (size of board) and N*N values (each 0 or 1).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "timer.h"
#include <omp.h>

/* data structure for two-dimensional array */
typedef struct twoD_array {
    int rows;
    int cols;
    int ** elems;
} twoD_array_t;

/* function declarations (comments with code below) */
twoD_array_t * build_array(twoD_array_t * a, int rows, int cols);
void free_array(twoD_array_t * a);
int read_board(FILE* infile, twoD_array_t *board);
int random_board(int size, int seed, twoD_array_t *board);
void update_board(twoD_array_t *board, twoD_array_t *new_board);
void print_board(FILE* outfile, twoD_array_t *board);
void clear_border(twoD_array_t *board);

/* main */
int main(int argc, char* argv[]) {
    int steps = 0, print_interval = 0, size = 0;
    twoD_array_t board1, board2;
    twoD_array_t *board = &board1;
    twoD_array_t *new_board = &board2;
    double start_time, init_done_time, end_time;
    char* args_message = 
        "[ infile | 'random' boardsize seed ] num_steps print_interval";
	int num_threads;

	#pragma omp parallel
	{
		#pragma omp master
		{
			num_threads = omp_get_num_threads();
		}
	}


    start_time = get_time();

    if (argc < 4) {
        fprintf(stderr, "usage:  %s %s\n", argv[0], args_message);
        return EXIT_FAILURE;
    }

    /* initialize board */
    if (strcmp(argv[1], "random") != 0) {

        /* from file */
        FILE* infile;
        infile = fopen(argv[1], "r");
        if (infile == NULL) {
            fprintf(stderr, "unable to open input file %s\n", argv[1]);
            return EXIT_FAILURE;
        }
        steps = atoi(argv[2]);
        print_interval = atoi(argv[3]);
        if ((steps <= 0) || (print_interval <= 0)) {
            fprintf(stderr, "usage:  %s %s\n", argv[0], args_message);
            return EXIT_FAILURE;
        }
        if (read_board(infile, board) != 0) {
            fclose(infile);
            return EXIT_FAILURE;
        }
        size = board->rows-2;
        fclose(infile);
        /* print input information:
         * writes this to standard error so it goes with timing information
         * (see below)
         */
        fprintf(stderr, "\n\nInput:  board read from file %s\n", argv[1]);
        fprintf(stderr, "%d steps, print interval %d, %d thread(s)\n\n", 
            steps, print_interval, num_threads);
    }
    else {

        /* with randomly-generated data */
        int seed = 0;
        if (argc < 6) {
            fprintf(stderr, "usage:  %s %s\n", argv[0], args_message);
            return EXIT_FAILURE;
        }
        size = atoi(argv[2]);
        seed = atoi(argv[3]);
        steps = atoi(argv[4]);
        print_interval = atoi(argv[5]);
        if ((steps <= 0) || (print_interval <= 0) ||
                (size <= 0) || (seed <= 0)) {
            fprintf(stderr, "usage:  %s %s\n", argv[0], args_message);
            return EXIT_FAILURE;
        }
        if (random_board(size, seed, board) != 0)
            return EXIT_FAILURE;
	/* print input information:
         * writes this to standard error so it goes with timing information
         * (see below)
         */
        fprintf(stderr, "\n\nInput:  board of size %d generated with seed %d\n",
                size, seed);
        fprintf(stderr, "%d steps, print interval %d, %d thread(s)\n", steps, print_interval, num_threads);
    }

    /* create "new board" and clear borders */
    if (build_array(new_board, size+2, size+2) == NULL) {
        fprintf(stderr, "unable to allocate space for board of size %d\n",
                size);
        return EXIT_FAILURE;
    }
    clear_border(new_board);

    /* print initial configuration */
/*    fprintf(stdout, "\nInitial board\n\n");
    print_board(stdout, board);
    fprintf(stdout, "\n\n");
*/
    init_done_time = get_time();

    /* loop to update board and print */
    for (int step = 0; step < steps; ++step) {
        /* update (results in new_board) */
        update_board(board, new_board);
        /* print */
 /*       if (((step+1) % print_interval) == 0) {
            fprintf(stdout, "Board after step %d\n\n", step+1);
            print_board(stdout, new_board);
            fprintf(stdout, "\n\n");
        }*/
        /* swap old and new boards */
        {
            twoD_array_t *temp = board;
            board = new_board;
            new_board = temp;
        }
    }

    end_time = get_time();

    /* print timing information:
     * writes this to standard error so it can be easily separated from
     * rest of output (which could be long)
     */
    fprintf(stderr, "\nTotal time %g\n", end_time - start_time);
    fprintf(stderr, "Time not counting initialization %g\n", 
            end_time - init_done_time);
 
    /* tidy up and return */
    free_array(board);
    free_array(new_board);
    return EXIT_SUCCESS;
}

/* 
 * constructs twoD_array structure.  returns NULL if unable to allocate
 * space for elements, pointer to structure otherwise.
 */
twoD_array_t * build_array(twoD_array_t * a, int rows, int cols) {
    int * temp;
    a->rows = rows;
    a->cols = cols;
    if ((a->elems = malloc(rows * sizeof(int *))) == NULL) {
        return NULL;
    }
    if ((temp = malloc(rows * cols * sizeof(int))) == NULL) {
        free (a->elems);
        return NULL;
    }
    for (int row = 0; row < rows; ++row, temp+=cols) {
        a->elems[row] = temp;
    }
    return a;
}

/* frees space pointed to by twoD_array structure */
void free_array(twoD_array_t * a) {
    free(a->elems[0]);
    free(a->elems);
}

/*
 * sets unused "edge" cells to 0 
 */
void clear_border(twoD_array_t *board) {
    for (int c = 0; c < board->cols; ++c) {
        board->elems[0][c] = 0;
        board->elems[board->rows-1][c] = 0;
    }
    for (int r = 0; r < board->rows; ++r) {
        board->elems[r][0] = 0;
        board->elems[r][board->cols-1] = 0;
    }
}

/*
 * reads initial configuration from infile.
 * returns 0 if all is well, otherwise prints error message and returns
 *   a non-zero value.
 */
int read_board(FILE* infile, twoD_array_t *board) {
    int i, j, size, temp;
    if (fscanf(infile, "%d", &size) != 1) {
        fprintf(stderr, "unable to read size of board\n");
        return 1;
    }
    if (build_array(board, size+2, size+2) == NULL) {
        fprintf(stderr, "unable to allocate space for board of size %d\n",
                size);
        return 2;
    }
    for (i = 1; i <= size; ++i) {
        for (j = 1; j <= size; ++j) {
            if (fscanf(infile, "%d", &temp) != 1) {
                fprintf(stderr, "unable to read values for board\n");
                return 1;
            }
            if ((temp == 0) || (temp == 1))
                board->elems[i][j] = temp;
            else {
                fprintf(stderr, "unable to read values for board\n");
                return 1;
            }
        }
    }
    clear_border(board);
    return 0;
}

/*
 * generates random board configuration for given size and seed.
 * returns 0 if all is well, otherwise prints error message and returns
 *   a non-zero value.
 */
int random_board(int size, int seed, twoD_array_t *board) {
    int i, j;
    if (build_array(board, size+2, size+2) == NULL) {
        fprintf(stderr, "unable to allocate space for board of size %d\n",
                size);
        return 2;
    }
    srand(seed);
    for (i = 1; i <= board->rows-2; ++i) {
        for (j = 1; j <= board->cols-2; ++j) {
            board->elems[i][j] = 
                (rand() < (RAND_MAX/2)) ? 0 : 1;
        }
    }
    clear_border(board);
    return 0;
}

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
		if (!(neighbs == 2 || neighbs == 3)) {
		    mycell = 0;
		}
	    }
	    else {
		if (neighbs == 3) {
		    mycell = 1;
		}
	    }

	    /*Update board*/
	    new_board->elems[i][j] = mycell;
	}
    }
}

/*
 * prints current board configuration.
 */
void print_board(FILE* outfile, twoD_array_t *board) {
    for (int i = 1; i <= board->rows-2; ++i) {
        for (int j = 1; j <= board->cols-2; ++j) {
            if (board->elems[i][j] == 0)
                fprintf(outfile, ". ");
            else
                fprintf(outfile, "1 ");
        }
        fprintf(outfile, "\n");
    }
}
