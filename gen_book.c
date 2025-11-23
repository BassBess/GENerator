#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define P_WIDTH 7
#define P_HEIGHT 6
#define TT_SIZE 8388593 
// SEARCH DEPTH: Higher = Smarter but Slower. 
// 12 is a good balance for generating a book quickly.
#define MAX_SEARCH_DEPTH 12 

typedef uint64_t bitboard_t;

typedef struct {
    bitboard_t current_position; 
    bitboard_t mask;
    int moves;
} Position;

typedef struct {
    uint64_t key;
    uint8_t val; 
} TTEntry;

static TTEntry* transTable = NULL;
static int columnOrder[P_WIDTH];

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

// --- DEPTH-LIMITED SOLVER ---
static void tt_put(uint64_t key, uint8_t val) {
    int idx = key % TT_SIZE;
    transTable[idx].key = key;
    transTable[idx].val = val;
}
static uint8_t tt_get(uint64_t key) {
    int idx = key % TT_SIZE;
    if(transTable[idx].key == key) return transTable[idx].val;
    return 0;
}

// Simple Heuristic: Score based on central pieces + potential wins
int heuristic(const Position *P) {
    return 0; // Placeholder: Assume Draw if depth reached (Safe play)
}

static int negamax(const Position *P, int alpha, int beta, int depth_left) {
    if (depth_left <= 0) return heuristic(P); // STOP condition

    bitboard_t possible = pos_possible_non_losing_moves(P);
    if (possible == 0) return -(P_WIDTH * P_HEIGHT - P->moves) / 2; 
    if (P->moves >= P_WIDTH * P_HEIGHT - 2) return 0; 

    int min = -(P_WIDTH * P_HEIGHT - 2 - P->moves) / 2;
    if (alpha < min) { alpha = min; if (alpha >= beta) return alpha; }
    int max = (P_WIDTH * P_HEIGHT - 1 - P->moves) / 2;
    if (beta > max) { beta = max; if (alpha >= beta) return beta; }

    uint64_t key = pos_key(P);
    uint8_t val = tt_get(key);
    // Only use TT if stored depth is high enough? For generation, trust it.
    if (val && val > 200) { 
        // Simplified TT check
    }

    for (int i = P_WIDTH - 1; i >= 0; i--) {
        int col = columnOrder[i];
        bitboard_t move = possible & column_mask(col);
        if (move) {
            Position P2 = *P;
            pos_play(&P2, move);
            int score = -negamax(&P2, -beta, -alpha, depth_left - 1);
            if (score >= beta) return score;
            if (score > alpha) alpha = score;
        }
    }
    return alpha;
}

static int solve(const Position *P) {
    if (pos_can_win_next(P)) return (P_WIDTH * P_HEIGHT + 1 - P->moves) / 2;
    int min = -100; // We limit score range because we aren't solving perfect endgames
    int max = 100;
    
    // Search fixed depth
    int r = negamax(P, min, max, MAX_SEARCH_DEPTH);
    return r;
}

// --- GENERATOR ---
uint8_t* visitedMap; 
void mark_visited(uint64_t key) { visitedMap[key % TT_SIZE] = 1; }
int is_visited(uint64_t key) { return visitedMap[key % TT_SIZE]; }

void generate_book(Position p, int depth, int max_gen_depth) {
    if (depth > max_gen_depth) return;

    // Progress for the user (printed to stderr so it shows on screen)
    if (depth < 2) {
        fprintf(stderr, "Generating Depth %d...\n", depth);
    }

    int bestCol = -1;
    int bestScore = -9999;

    // Find best move for this node
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
        uint64_t k = pos_key(&p);
        if (!is_visited(k)) {
            printf("add_to_book(%lluULL, %d);\n", k, bestCol + 1);
            fflush(stdout); // FORCE PRINT TO FILE
            mark_visited(k);
        }
    }

    // Recurse
    for (int c = 0; c < P_WIDTH; c++) {
        if ((p.mask & (1ULL << ((P_HEIGHT - 1) + c * (P_HEIGHT + 1)))) == 0) {
             Position next = p;
             bitboard_t move = (next.mask + bottom_mask_col(c)) & column_mask(c);
             pos_play(&next, move);
             generate_book(next, depth + 1, max_gen_depth);
        }
    }
}

int main() {
    transTable = (TTEntry*)calloc(TT_SIZE, sizeof(TTEntry));
    visitedMap = (uint8_t*)calloc(TT_SIZE, sizeof(uint8_t));
    for(int i = 0; i < P_WIDTH; i++) columnOrder[i] = P_WIDTH/2 + (1-2*(i%2))*(i+1)/2;

    printf("// Generated Book (Depth 12 analysis)\n");
    printf("void init_book() {\n");
    
    Position root = {0, 0, 0};
    // Generate book for the first 6 moves of the game
    generate_book(root, 0, 6);
    
    printf("}\n");
    
    fprintf(stderr, "Done! Check book_data.c\n");

    free(transTable);
    free(visitedMap);
    return 0;
}
