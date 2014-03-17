import java.util.*;

public class minimax_joneshf extends AIModule {

    public class MoveComparator implements Comparator<Move> {
        // public int compare(Move a, Move b) {
        //     return Integer.compare(a.util, b.util);
        // }
        // // Distance from the center of the board.
        public int compare(Move a, Move b) {
            Integer mid = width / 2;
            return Integer.compare(Math.abs(a.col - mid), Math.abs(b.col - mid));
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

    public int player;
    public int opponent;
    public int height = 0;
    public int width = 0;
    public boolean firstMove = true;
    public GameStateModule game;

    public void getNextMove(final GameStateModule game) {
        int player = game.getActivePlayer();
        this.game = game;
        this.player = player;
        this.opponent = player == 1 ? 2 : 1;
        this.width = this.game.getWidth();
        this.height = this.game.getHeight();
        if (this.firstMove && this.player == 1) {
            this.chosenMove = this.width / 2;
            this.firstMove = false;
            return;
        }
        int maxDepth = this.width;
        for (int i = 0; i < this.width; ++i) {
            maxDepth *= Math.max(this.height - this.game.getHeightAt(i), 1);
        }
        int depth = maxDepth - 1;
        while (!this.terminate && depth <= maxDepth) {
            minimaxDecision(depth++);
        }
    }

    public List<Integer> availableMoves() {
        Queue<Move> moves = new PriorityQueue<Move>(10, new MoveComparator());
        List<Integer> ms = new ArrayList<Integer>();
        for (int c = 0; c < this.width; ++c) {
            if (this.game.canMakeMove(c)) {
                this.game.makeMove(c);
                moves.add(new Move(c, utility(c)));
                this.game.unMakeMove();
            }
        }
        for (Move m : moves) {
            ms.add(m.col);
        }
        System.out.println(ms);
        return ms;
    }

    public void minimaxDecision(int depth) {
        HashMap<Integer, Integer> vals = new HashMap<Integer, Integer>();
        int max = -Integer.MAX_VALUE;
        int move = 0;
        int α = -Integer.MAX_VALUE;
        int β = Integer.MAX_VALUE;
        for (int c : availableMoves()) {
            vals.put(maxValue(depth, c, α, β), c);
        }
        for (Map.Entry<Integer, Integer> m : vals.entrySet()) {
            if (m.getKey() > max) {
                move = m.getValue();
            }
        }
        this.chosenMove = move;
    }

    public int maxValue(int depth, int col, int α, int β) {
        if (this.terminate || this.game.isGameOver() || depth == 0) {
            return utility(col);
        }

        for (int c : availableMoves()) {
            this.game.makeMove(c);
            α = Math.max(α, minValue(depth - 1, c, α, β));
            if (α >= β) {
                return β;
            }
            this.game.unMakeMove();
        }
        return α;
    }

    public int minValue(int depth, int col, int α, int β) {
        if (this.terminate || this.game.isGameOver() || depth == 0) {
            return utility(col);
        }

        for (int c : availableMoves()) {
            this.game.makeMove(c);
            β = Math.min(β, maxValue(depth - 1, c, α, β));
            if (β < α) {
                return α;
            }
            this.game.unMakeMove();
        }
        return β;
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

    public boolean checkWins(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != (temp & (temp >> 2 * (this.height - 2)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != (temp & (temp >> 2 * (this.height + 1)))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != (temp & (temp >> 2 * (this.height + 2)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != (temp & (temp >> 2 * 1))) {
            return true;
        }

        return false;
    }

    public boolean checkThrees(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != (temp & (temp >> (this.height - 2)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != (temp & (temp >> (this.height + 1)))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != (temp & (temp >> (this.height + 2)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != (temp & (temp >> 1))) {
            return true;
        }

        return false;
    }

    public boolean checkTwos(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != temp) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != temp) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != temp) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != temp) {
            return true;
        }

        return false;
    }

    public boolean checkBlocks(long player, long opponent) {
        // We need to see if 3 in a row for the opponent
        // intersects with one of the player's pieces.
        // Make the diagonal for opponent.

        // Backslash up
        long oppo = opponent & (opponent >> (this.height - 2));
        oppo &= oppo >> (this.height - 2);
        boolean block = 0 != (player & (oppo >> (this.height - 2)));

        // Backslash down
        oppo = opponent & (opponent << (this.height - 2));
        oppo &= (oppo << (this.height - 2));
        block |= 0 != (player & (oppo << (this.height - 2)));

        // Forwardslash up
        oppo = opponent & (opponent << (this.height + 2));
        oppo &= oppo << (this.height + 2);
        block |= 0 != (player & (oppo << (this.height + 2)));

        // Forwardslash down
        oppo = opponent & (opponent >> (this.height + 2));
        oppo &= (oppo >> (this.height + 2));
        block |= 0 != (player & (oppo >> (this.height + 2)));

        // Horizontal right
        oppo = opponent & (opponent << (this.height + 1));
        oppo &= oppo << (this.height + 1);
        block |= 0 != (player & (oppo << (this.height + 1)));

        // Horizontal left
        oppo = opponent & (opponent >> (this.height + 1));
        oppo &= (oppo >> (this.height + 1));
        block |= 0 != (player & (oppo >> (this.height + 1)));

        // Vertical up
        oppo = opponent & (opponent << 1);
        oppo &= oppo << 1;
        block |= 0 != (player & (oppo << 1));

        // Vertical down
        oppo = opponent & (opponent >> 1);
        oppo &= (oppo >> 1);
        block |= 0 != (player & (oppo >> 1));

        return block;
    }

    public int utility(int col) {
        long[] boards = gameToBits();
        int total = 0;
        int row = this.game.getHeightAt(col);
        long player = 1 << (col * (this.height + 1) + row);
        if (this.game.isGameOver()) {
            switch (this.game.getWinner()) {
                case 1:
                    return Integer.MAX_VALUE;
                case 2:
                    return -Integer.MAX_VALUE;
                default:
                    return 0;
                }
        }
        if (checkBlocks(player, boards[1])) {
            total += 500;
        }
        if (checkBlocks(player, boards[0])) {
            total += -500;
        }
        if (checkThrees(boards[0])) {
            total += 75;
        }
        if (checkThrees(boards[1])) {
            total += -75;
        }
        if (checkTwos(boards[0])) {
            total += 25;
        }
        if (checkTwos(boards[1])) {
            total += -25;
        }
        return total;
    }
}
