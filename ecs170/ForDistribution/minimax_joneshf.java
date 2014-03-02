import java.util.*;

public class minimax_joneshf extends AIModule {

    public int player;
    public int opponent;
    public int height = 0;
    public int width = 0;
    public boolean firstMove = true;
    public TextDisplay td;

    public int evaluations = 0;

    public minimax_joneshf() {
        this.td = new TextDisplay();
        this.player = 0;
        this.opponent = 0;
    }

    public void showBoard(final GameStateModule game) {
        this.td.drawBoard(game);
    }

    public void getNextMove(final GameStateModule game) {
        int player = game.getActivePlayer();
        this.player = player;
        this.opponent = player == 1 ? 2 : 1;
        this.width = game.getWidth();
        this.height = game.getHeight();
        if (this.firstMove && this.player == 1) {
            this.chosenMove = this.width / 2;
            this.firstMove = false;
            return;
        }
        int maxDepth = this.width;
        for (int i = 0; i < this.width; ++i) {
            maxDepth *= Math.max(this.height - game.getHeightAt(i), 1);
        }
        showBoard(game);
        int depth = maxDepth - 1;
        // while (!this.terminate && depth <= maxDepth) {
        //     minimaxDecision(depth++, game);
        // }
        minimaxDecision(depth++, game);
    }

    public void minimaxDecision(int depth, final GameStateModule game) {
        int[] v = maxValue(depth, game.getWidth() / 2, game);
        System.out.println("Choosing "+v[1]);
        System.out.println("Value "+v[0]);
        this.chosenMove = v[1];
    }

    public int[] maxValue(int depth, int col, final GameStateModule game) {

        if (/*depth == 0 ||*/ this.terminate || game.isGameOver()) {
            return new int[] {utility(game, game.getHeightAt(col), col), col};
        }

        int[] v = {-Integer.MAX_VALUE, col};
        int temp[];
        for (int c : getMoves(this.player, game)) {
            game.makeMove(c);
            temp = minValue(depth - 1, c, game);
            v[0] = v[0] < temp[0] ? temp[0] : v[0];
            game.unMakeMove();
        }

        if (v[0] > 10000) {
            System.out.println("Max value "+v[0]+" at "+v[1]);
            showBoard(game);
        }
        return v;

    }

    public int[] minValue(int depth, int col, final GameStateModule game) {

        if (/*depth == 0 ||*/ this.terminate || game.isGameOver()) {
            return new int[] {utility(game, game.getHeightAt(col) - 1, col), col};
        }

        int[] v = {Integer.MAX_VALUE, 0};
        int temp[];
        for (int c : getMoves(this.opponent, game)) {
            game.makeMove(c);
            temp = maxValue(depth - 1, c, game);
            v = v[0] > temp[0] ? temp : v;
            game.unMakeMove();
        }

        return v;

    }

    public boolean terminal(final GameStateModule game) {
        long[] boards = gameToBits(game);

        return checkWins(boards[0]) || checkWins(boards[1]);
    }

    public long[] gameToBits(final GameStateModule game) {
        long playerBoard = 0l;
        long opponentBoard = 0l;
        int pos = 0;
        for (int c = 0; c < game.getWidth(); ++c) {
            for (int r = 0; r < game.getHeight(); ++r) {
                if (game.getAt(c, r) == this.player) {
                    playerBoard |= 1l << (c * (game.getHeight() + 1) + r);
                } else if (game.getAt(c, r) == this.opponent) {
                    opponentBoard |= 1l << (c * (game.getHeight() + 1) + r);
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

    public int utility(final GameStateModule game, int row, int col) {
        long[] boards = gameToBits(game);
        int total = 0;
        long player = 1 << (col * (game.getHeight() + 1) + row);
        if (game.isGameOver()) {
            switch (game.getWinner()) {
                case 1:
                    return Integer.MAX_VALUE;
                case 2:
                    return -Integer.MAX_VALUE;
                default:
                    return 0;
                }
        }
        if (game.getActivePlayer() == 1) {
            if (checkBlocks(player, boards[1])) {
                return Integer.MAX_VALUE;
            }
            if (checkBlocks(player, boards[0])) {
                return Integer.MAX_VALUE;
            }
        } else if (game.getActivePlayer() == 2) {
            if (checkBlocks(player, boards[0])) {
                return -Integer.MAX_VALUE;
            }
            if (checkBlocks(player, boards[1])) {
                return -Integer.MAX_VALUE;
            }
        }
        if (checkWins(boards[0])) {
            total += 250;
        }
        if (checkWins(boards[1])) {
            total += -250;
        }
        // if (checkBlocks(player, boards[1])) {
        //     total += 500;
        // }
        // if (checkBlocks(player, boards[0])) {
        //     total += -500;
        // }
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

    public List<Integer> getMoves(int player, final GameStateModule game) {
        List<Integer> moves = new ArrayList<Integer>();
        int row;
        int size = 0;
        for (int col = 0; col < game.getWidth(); ++col) {
            if (game.canMakeMove(col)) {
                ++size;
                moves.add(col);
            }
        }
        return moves;
    }
}
