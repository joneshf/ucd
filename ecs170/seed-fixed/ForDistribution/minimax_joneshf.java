import java.util.*;

public class minimax_joneshf extends AIModule {

    public class MoveComparator implements Comparator<Move> {
        @Override
        public int compare(Move a, Move b) {
            return Integer.compare(a.util, b.util);
        }
    }

    public class Move {
        public int col;
        public int util;

        public Move(int col, int util) {
            this.col = col;
            this.util = util;
        }
    }

    public GameStateModule game;
    public boolean useStatic;
    public TextDisplay td = new TextDisplay();
    public int height;
    public int width;
    public int player;
    public int opponent;

    public void getNextMove(final GameStateModule game) {
        this.game = game;
        this.width = this.game.getWidth();
        this.height = this.game.getHeight();
        this.player = this.game.getActivePlayer();
        this.opponent = this.player == 1 ? 2 : 1;
        int depth = 0;
        while (!this.terminate) {
            minimaxDecision(++depth);
        }
    }

    public List<Integer> availableMoves() {
        Queue<Move> moves = new PriorityQueue<Move>(10, new MoveComparator());
        List<Integer> ms = new ArrayList<Integer>();
        for (int c = 0; c < this.width; ++c) {
            if (this.game.canMakeMove(c)) {
                this.game.makeMove(c);
                moves.add(new Move(c, utility()));
                this.game.unMakeMove();
            }
        }
        for (Move m : moves) {
            ms.add(m.col);
        }
        return ms;
    }

    public void minimaxDecision(int depth) {
        PriorityQueue<Move> vals = new PriorityQueue<Move>(10, new MoveComparator());
        int move = 0;
        int α = -Integer.MAX_VALUE;
        int β = Integer.MAX_VALUE;
        for (int c : availableMoves()) {
            vals.add(new Move(c, maxValue(depth, α, β)));
        }
        this.chosenMove = vals.poll().col;
    }

    public int maxValue(int depth, int α, int β) {
        if (depth == 0 || this.terminate || this.game.isGameOver()) {
            return utility();
        }

        for (int c : availableMoves()) {
            this.game.makeMove(c);
            α = Math.max(α, minValue(depth - 1, α, β));
            this.game.unMakeMove();
            if (α >= β) {
                return β;
            }
        }

        return α;
    }

    public int minValue(int depth, int α, int β) {
        if (depth == 0 || this.terminate || this.game.isGameOver()) {
            return utility();
        }

        for (int c : availableMoves()) {
            this.game.makeMove(c);
            β = Math.min(β, maxValue(depth - 1, α, β));
            this.game.unMakeMove();
            if (α >= β) {
                return α;
            }
        }

        return β;
    }

    public int utility() {
        long[] boards = gameToBits();
        int total = 0;

        if (this.game.isGameOver()) {
            if (this.game.getWinner() == this.player) {
                return Integer.MAX_VALUE;
            } else if (this.game.getWinner() == this.opponent) {
                return -Integer.MAX_VALUE;
            } else {
                return 0;
            }
        } else {
            // total += 10000 * checkWins(boards[0]);
            // total += -10000 * checkWins(boards[0]);
            total += 500 * checkBlocks(boards[0], boards[1]);
            // total += 500 * checkBlocks(boards[0], boards[0]);
            total += -500 * checkBlocks(boards[1], boards[0]);
            // total += -500 * checkBlocks(boards[1], boards[1]);
            // total += 75 * checkThrees(boards[0]);
            // total += -75 * checkThrees(boards[1]);
            // total += 25 * checkTwos(boards[0]);
            // total += -25 * checkTwos(boards[1]);
            return total;
        }
    }

    public int checkWins(long board) {
        int wins = 0;
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        temp &= temp >> 2 * (this.height - 2);
        wins += countBits(temp);
        // Horizontal
        temp = board & (board >> (this.height + 1));
        temp &= temp >> 2 * (this.height + 1);
        wins += countBits(temp);
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        temp &= temp >> 2 * (this.height + 2);
        wins += countBits(temp);
        // Vertical
        temp = board & (board >> 1);
        temp &= temp >> 2 * 1;
        wins += countBits(temp);

        return wins;
    }

    public int checkThrees(long board) {
        int threes = 0;
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        temp &= temp >> (this.height - 2);
        threes += countBits(temp);
        // Horizontal
        temp = board & (board >> (this.height + 1));
        temp &= temp >> (this.height + 1);
        threes += countBits(temp);
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        temp &= temp >> (this.height + 2);
        threes += countBits(temp);
        // Vertical
        temp = board & (board >> 1);
        temp &= temp >> 1;
        threes += countBits(temp);

        return threes;
    }

    public int checkTwos(long board) {
        int twos = 0;
        // Diagonal down
        twos += countBits(board & (board >> (this.height - 2)));
        // Horizontal
        twos += countBits(board & (board >> (this.height + 1)));
        // Diagonal up
        twos += countBits(board & (board >> (this.height + 2)));
        // Vertical
        twos += countBits(board & (board >> 1));

        return twos;
    }

    public int checkBlocks(long player, long opponent) {
        int blocks = 0;
        // We need to see if 3 in a row for the opponent
        // intersects with one of the player's pieces.
        // Make the diagonal for opponent.

        // Backslash up
        long oppo = opponent & (opponent >> (this.height - 2));
        oppo &= oppo >> (this.height - 2);
        blocks += countBits(player & (oppo >> (this.height - 2)));

        // Backslash down
        oppo = opponent & (opponent << (this.height - 2));
        oppo &= (oppo << (this.height - 2));
        blocks += countBits(player & (oppo << (this.height - 2)));

        // Forwardslash up
        oppo = opponent & (opponent << (this.height + 2));
        oppo &= oppo << (this.height + 2);
        blocks += countBits(player & (oppo << (this.height + 2)));

        // Forwardslash down
        oppo = opponent & (opponent >> (this.height + 2));
        oppo &= (oppo >> (this.height + 2));
        blocks += countBits(player & (oppo >> (this.height + 2)));

        // Horizontal right
        oppo = opponent & (opponent << (this.height + 1));
        oppo &= oppo << (this.height + 1);
        blocks += countBits(player & (oppo << (this.height + 1)));

        // Horizontal left
        oppo = opponent & (opponent >> (this.height + 1));
        oppo &= (oppo >> (this.height + 1));
        blocks += countBits(player & (oppo >> (this.height + 1)));

        // Vertical up
        oppo = opponent & (opponent << 1);
        oppo &= oppo << 1;
        blocks += countBits(player & (oppo << 1));

        // Vertical down
        oppo = opponent & (opponent >> 1);
        oppo &= (oppo >> 1);
        blocks += countBits(player & (oppo >> 1));

        return blocks;
    }

    public long[] gameToBits() {
        long playerBoard = 0l;
        long opponentBoard = 0l;
        int pos = 0;
        for (int c = 0; c < this.game.getWidth(); ++c) {
            for (int r = 0; r < this.game.getHeight(); ++r) {
                if (this.game.getAt(c, r) == this.player) {
                    playerBoard |= 1l << (c * (this.game.getHeight() + 1) + r);
                } else if (this.game.getAt(c, r) == this.opponent) {
                    opponentBoard |= 1l << (c * (this.game.getHeight() + 1) + r);
                }
            }
        }

        return new long[] {playerBoard, opponentBoard};
    }

    public int countBits(long v) {
        int count = 0;
        for (; v > 0; ++count)
        {
          v &= v - 1; // clear the least significant bit set
        }
        return count;
    }
}
