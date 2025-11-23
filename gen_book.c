#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h> // POSIX Threads

// --- CONFIGURATION ---
#define P_WIDTH 7
#define P_HEIGHT 6

// Large Transposition Table (~1GB RAM recommended for God Mode)
// If this crashes your PC, change it back to 8388593 (~128MB)
#define TT_SIZE 67108859 

// Depth of the book itself (How many moves from start to memorize)
// 8 plies = 4 full turns. 
#define BOOK_GEN_DEPTH 8

typedef uint64_t bitboard_t;

typedef struct {
    bitboard_t current_position; 
    bitboard_t mask;
    int moves;
} Position;

typedef struct {
    uint64_t key;
    int8_t val; // Store exact score (-21 to +21)
    uint8_t flag; // 0=Empty, 1=Exact, 2=Lower, 3=Upper
} TTEntry;

// Shared Global Resources
TTEntry* transTable = NULL;
int columnOrder[P_WIDTH];
uint8_t* visitedMap = NULL;
pthread_mutex_t writeLock; // For writing results safely

// --- BITBOARD LOGIC ---
static int popcount(bitboard_t m) { return __builtin_popcountll(m); }
static bitboard_t bottom_mask_col(int col) { return 1ULL << (col * (P_HEIGHT + 1)); }
static bitboard_t column_mask(int col) { return ((1ULL << P_HEIGHT) - 1) << (col * (P_HEIGHT + 1)); }
static bitboard_t get_board_mask() {
    bitboard_t m = 0;
    for(int c=0; c<P_WIDTH; c++) m |= column_mask(c);
    return m;
}
static bitboard_t compute_winning_position(bitboard_t position, bitboard_t mask) {
    bitboard_t r = (position << 1) & (position << 2) & (position << 3); 
    bitboard_t p = (position << (P_HEIGHT + 1)) & (position << 2 * (P_HEIGHT + 1));
    r |= p & (position << 3 * (P_HEIGHT + 1));
    r |= p & (position >> (P_HEIGHT + 1));
    p = (position >> (P_HEIGHT + 1)) & (position >> 2 * (P_HEIGHT + 1));
    r |= p & (position << (P_HEIGHT + 1));
    r |= p & (position >> 3 * (P_HEIGHT + 1));
    p = (position << P_HEIGHT) & (position << 2 * P_HEIGHT);
    r |= p & (position << 3 * P_HEIGHT);
    r |= p & (position >> P_HEIGHT);
    p = (position >> P_HEIGHT) & (position >> 2 * P_HEIGHT);
    r |= p & (position << P_HEIGHT);
    r |= p & (position >> 3 * P_HEIGHT);
    p = (position << (P_HEIGHT + 2)) & (position << 2 * (P_HEIGHT + 2)); 
    r |= p & (position << 3 * (P_HEIGHT + 2));
    r |= p & (position >> (P_HEIGHT + 2));
    p = (position >> (P_HEIGHT + 2)) & (position >> 2 * (P_HEIGHT + 2));
    r |= p & (position << (P_HEIGHT + 2));
    r |= p & (position >> 3 * (P_HEIGHT + 2));
    return r & (get_board_mask() ^ mask);
}
static void pos_play(Position *p, bitboard_t move) {
    p->current_position ^= p->mask;
    p->mask |= move;
    p->moves++;
}
static uint64_t pos_key(const Position *p) { return p->current_position + p->mask; }
static int pos_can_win_next(const Position *p) {
    bitboard_t bottom = 0;
    for(int c=0; c<P_WIDTH; c++) bottom |= bottom_mask_col(c);
    bitboard_t possible = (p->mask + bottom) & get_board_mask();
    return (compute_winning_position(p->current_position, p->mask) & possible) != 0;
}
static bitboard_t pos_possible_non_losing_moves(const Position *p) {
    bitboard_t bottom = 0;
    for(int c=0; c<P_WIDTH; c++) bottom |= bottom_mask_col(c);
    bitboard_t possible_mask = (p->mask + bottom) & get_board_mask();
    bitboard_t opponent_win = compute_winning_position(p->current_position ^ p->mask, p->mask);
    bitboard_t forced_moves = possible_mask & opponent_win;
    if(forced_moves) {
        if(forced_moves & (forced_moves - 1)) return 0; 
        else possible_mask = forced_moves; 
    }
    return possible_mask & ~(opponent_win >> 1); 
}
static int pos_move_score(const Position *p, bitboard_t move) {
    return popcount(compute_winning_position(p->current_position | move, p->mask));
}

// --- SOLVER (GOD MODE) ---
static void tt_put(uint64_t key, int8_t val, uint8_t flag) {
    int idx = key % TT_SIZE;
    // Always overwrite (Simple strategy for generation)
    transTable[idx].key = key;
    transTable[idx].val = val;
    transTable[idx].flag = flag;
}

static int negamax(const Position *P, int alpha, int beta) {
    bitboard_t possible = pos_possible_non_losing_moves(P);
    if (possible == 0) return -(P_WIDTH * P_HEIGHT - P->moves) / 2; 
    if (P->moves >= P_WIDTH * P_HEIGHT - 2) return 0; 

    int min = -(P_WIDTH * P_HEIGHT - 2 - P->moves) / 2;
    if (alpha < min) { alpha = min; if (alpha >= beta) return alpha; }
    int max = (P_WIDTH * P_HEIGHT - 1 - P->moves) / 2;
    if (beta > max) { beta = max; if (alpha >= beta) return beta; }

    uint64_t key = pos_key(P);
    int idx = key % TT_SIZE;
    
    // TT Lookup
    if (transTable[idx].key == key) {
        if (transTable[idx].flag == 1) return transTable[idx].val;
        if (transTable[idx].flag == 2 && transTable[idx].val >= beta) return transTable[idx].val;
        if (transTable[idx].flag == 3 && transTable[idx].val <= alpha) return transTable[idx].val;
    }

    // Move Sorter (Crucial for God Mode performance)
    struct { int move; int score; } moves[P_WIDTH];
    int count = 0;

    for (int i = P_WIDTH - 1; i >= 0; i--) {
        int col = columnOrder[i];
        bitboard_t move = possible & column_mask(col);
        if (move) {
            moves[count].move = col;
            moves[count].score = pos_move_score(P, move); // Simple heuristic for sorting
            count++;
        }
    }

    // Sort Moves (Bubble sort is fast enough for 7 items)
    for(int i=0; i<count; i++) {
        for(int j=i+1; j<count; j++) {
            if(moves[j].score > moves[i].score) {
                int tm = moves[i].move; int ts = moves[i].score;
                moves[i].move = moves[j].move; moves[i].score = moves[j].score;
                moves[j].move = tm; moves[j].score = ts;
            }
        }
    }

    int bestScore = -100; // Init with something lower than min possible

    for(int i=0; i<count; i++) {
        int col = moves[i].move;
        bitboard_t move = possible & column_mask(col);
        
        Position P2 = *P;
        pos_play(&P2, move);
        
        int score = -negamax(&P2, -beta, -alpha);
        
        if(score > bestScore) bestScore = score;
        
        if (score >= beta) {
            tt_put(key, score, 2); // Lower bound
            return score;
        }
        if (score > alpha) alpha = score;
    }
    
    // If we didn't prune, we found an exact or upper bound
    if(bestScore <= alpha) tt_put(key, bestScore, 3); // Upper
    else tt_put(key, bestScore, 1); // Exact
    
    return alpha;
}

static int solve(const Position *P) {
    if (pos_can_win_next(P)) return (P_WIDTH * P_HEIGHT + 1 - P->moves) / 2;
    return negamax(P, -100, 100); // Full Window search
}

// --- GENERATOR ---
void mark_visited(uint64_t key) { visitedMap[key % TT_SIZE] = 1; }
int is_visited(uint64_t key) { return visitedMap[key % TT_SIZE]; }

typedef struct { uint64_t key; int move; } Result;
Result results[1000000];
int res_count = 0;

void add_result(uint64_t k, int m) {
    pthread_mutex_lock(&writeLock);
    if (!is_visited(k)) {
        results[res_count].key = k;
        results[res_count].move = m;
        res_count++;
        mark_visited(k);
    }
    pthread_mutex_unlock(&writeLock);
}

void generate_book_recursive(Position p, int depth) {
    if (depth > BOOK_GEN_DEPTH) return;

    int bestCol = -1;
    int bestScore = -9999;

    // 1. Solve this position perfectly
    for(int i = 0; i < P_WIDTH; i++) {
        int col = columnOrder[i]; 
        if((p.mask & (1ULL << ((P_HEIGHT - 1) + col * (P_HEIGHT + 1)))) == 0) {
            Position p2 = p;
            bitboard_t move = (p2.mask + bottom_mask_col(col)) & column_mask(col);
            pos_play(&p2, move); 
            
            int score = -solve(&p2);
            
            if(score > bestScore) {
                bestScore = score;
                bestCol = col;
            }
        }
    }

    if (bestCol != -1) {
        add_result(pos_key(&p), bestCol + 1);
    }

    // 2. Expand Tree if game not over
    // Optimization: If we found a forced win, maybe we don't need to memorize deeper?
    // For book generation, we usually want to be robust, so we continue.
    if (bestScore > 20 || bestScore < -20) return; // End of game reached

    for (int c = 0; c < P_WIDTH; c++) {
        if ((p.mask & (1ULL << ((P_HEIGHT - 1) + c * (P_HEIGHT + 1)))) == 0) {
             Position next = p;
             bitboard_t move = (next.mask + bottom_mask_col(c)) & column_mask(c);
             pos_play(&next, move);
             generate_book_recursive(next, depth + 1);
        }
    }
}

// --- THREAD WORKER ---
struct ThreadArgs {
    Position startP;
    int col;
};

void* worker_thread(void* arg) {
    struct ThreadArgs* args = (struct ThreadArgs*)arg;
    Position p = args->startP;
    int col = args->col;
    
    fprintf(stderr, "Thread solving Col %d branch...\n", col+1);
    
    bitboard_t move = (p.mask + bottom_mask_col(col)) & column_mask(col);
    pos_play(&p, move);
    
    // Start recursing
    generate_book_recursive(p, 2);
    
    fprintf(stderr, "Thread finished Col %d.\n", col+1);
    return NULL;
}

int main() {
    pthread_mutex_init(&writeLock, NULL);
    
    // Allocate huge tables
    fprintf(stderr, "Allocating %.2f MB for Transposition Table...\n", (double)(TT_SIZE * sizeof(TTEntry))/(1024*1024));
    transTable = (TTEntry*)calloc(TT_SIZE, sizeof(TTEntry));
    visitedMap = (uint8_t*)calloc(TT_SIZE, sizeof(uint8_t));
    
    if(!transTable || !visitedMap) {
        fprintf(stderr, "Memory allocation failed! Try reducing TT_SIZE.\n");
        return 1;
    }

    for(int i = 0; i < P_WIDTH; i++) columnOrder[i] = P_WIDTH/2 + (1-2*(i%2))*(i+1)/2;

    printf("// Generated Book (God Mode - Full Solve)\n");
    printf("void init_book() {\n");
    printf("add_to_book(0ULL, 4);\n"); 

    // Root: P1 played Col 4
    Position root = {0, 0, 0};
    bitboard_t m = (root.mask + bottom_mask_col(3)) & column_mask(3);
    pos_play(&root, m);

    // Launch Threads
    pthread_t threads[P_WIDTH];
    struct ThreadArgs args[P_WIDTH];
    int valid_threads = 0;

    for (int c = 0; c < P_WIDTH; c++) {
        int col = columnOrder[c];
        if ((root.mask & (1ULL << ((P_HEIGHT - 1) + col * (P_HEIGHT + 1)))) == 0) {
            args[valid_threads].startP = root;
            args[valid_threads].col = col;
            pthread_create(&threads[valid_threads], NULL, worker_thread, &args[valid_threads]);
            valid_threads++;
        }
    }

    for(int i=0; i<valid_threads; i++) pthread_join(threads[i], NULL);

    // Print Results
    for(int i=0; i<res_count; i++) {
        printf("add_to_book(%lluULL, %d);\n", results[i].key, results[i].move);
    }
    printf("}\n");
    
    fprintf(stderr, "Done! Generated %d moves.\n", res_count);

    free(transTable);
    free(visitedMap);
    pthread_mutex_destroy(&writeLock);
    return 0;
}
