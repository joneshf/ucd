import java.util.*;

public class minimax_joneshf extends AIModule {

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
        List<Integer> moves = new ArrayList<Integer>();
        for (int c = 0; c < this.width; ++c) {
            if (this.game.canMakeMove(c)) {
                moves.add(c);
            }
        }
        return moves;
    }

    public void minimaxDecision(int depth) {
        HashMap<Integer, Integer> vals = new HashMap<Integer, Integer>();
        int max = -Integer.MAX_VALUE;
        int move = 0;
        int α = -Integer.MAX_VALUE;
        int β = Integer.MAX_VALUE;
        for (int c : availableMoves()) {
            vals.put(maxValue(depth, α, β), c);
        }
        for (Map.Entry<Integer, Integer> m : vals.entrySet()) {
            if (m.getKey() > max) {
                move = m.getValue();
            }
        }
        this.chosenMove = move;
    }

    public int maxValue(int depth, int α, int β) {
        if (depth == 0 || this.terminate || this.game.isGameOver()) {
            return utility();
        }

        for (int c : availableMoves()) {
            this.game.makeMove(c);
            α = Math.max(α, minValue(depth, α, β));
            this.game.unMakeMove();
            if (α >= β) {
                break;
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
            // Only update the depth here.
            // Then we'll always have a max static board.
            β = Math.min(β, maxValue(depth - 1, α, β));
            this.game.unMakeMove();
            if (β <= α) {
                break;
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
            if (this.game.getActivePlayer() == this.player) {
                if (checkBlocks(boards[0], boards[1])) {
                    total += 500;
                }
                if (checkBlocks(boards[0], boards[0])) {
                    total += 1500;
                }
            } else if (this.game.getActivePlayer() == this.opponent) {
                if (checkBlocks(boards[1], boards[0])) {
                    total += -500;
                }
                if (checkBlocks(boards[1], boards[1])) {
                    total += -1500;
                }
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
}
